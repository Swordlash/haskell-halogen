{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- The program around lesson 14 is a page that is not Halogen's: it puts
-- plain HTML into #learn-result (the lesson's element, three buttons and a
-- paragraph) and a script of a few lines that drives the element as any
-- page's JavaScript would, then defines <x-lesson> from the lesson's
-- definition and component, which mounts the component in the element
-- already there. What it keeps for the next run (which the next compile
-- starts with, and the page runs when it leaves the lesson) disposes of the
-- registration, ending this run's element, and takes the HTML out again.
module Main (main) where

import Data.Text (Text)
import Halogen.CustomElement (Registration (..))
import Halogen.CustomElement qualified as CE
import Halogen.HTML.Core (ElemName (..))
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (PropValue (..), appendChild, createElement, document, elementToNode, nodeToElement, runBrowserDOM, setProperty, setTextContent, window)
import Lesson (component, definition)
import Web.DOM.Internal.Types (toDocument, toNode)
import Web.DOM.ParentNode (QuerySelector (..))
import Web.HTML.Common (PropName (..))

page :: Text
page =
  "<x-lesson label=\"Apples\"></x-lesson>\
  \<p><button data-do=\"rename\">Call it Pears</button> <button data-do=\"reset\">reset()</button> <button data-do=\"ask\">count()</button></p>\
  \<p class=\"lesson-heard\">The page has heard nothing yet.</p>"

-- An element script made with createElement runs once it is in the
-- document, as one written in the HTML does (innerHTML's would not run).
script :: Text
script =
  "(() => {\n\
  \  const result = document.getElementById('learn-result');\n\
  \  const element = result.querySelector('x-lesson');\n\
  \  const heard = result.querySelector('.lesson-heard');\n\
  \  const say = (line) => { heard.textContent = line; };\n\
  \  element.addEventListener('count', (event) => say(`The page heard a count event: ${event.detail}`));\n\
  \  result.querySelector('[data-do=rename]').addEventListener('click', () => element.setAttribute('label', 'Pears'));\n\
  \  result.querySelector('[data-do=reset]').addEventListener('click', () => element.reset());\n\
  \  result.querySelector('[data-do=ask]').addEventListener('click', async () => {\n\
  \    if (typeof element.count !== 'function') say('The element has no count() method.');\n\
  \    else say(`count() answered ${await element.count()}`);\n\
  \  });\n\
  \})();"

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  result <- HA.selectElement (QuerySelector "#learn-result") >>= maybe (error "the page has no #learn-result") pure
  container <- nodeToElement (toNode result) >>= maybe (error "#learn-result is not an element") pure
  setProperty (PropName "innerHTML") (TxtProp page) container
  doc <- toDocument <$> (document =<< window)
  runner <- elementToNode =<< createElement Nothing (ElemName "script") doc
  setTextContent script runner
  appendChild runner (toNode result)
  registration <- CE.define "x-lesson" definition component
  HA.keepForNextRun (registration.dispose >> setProperty (PropName "innerHTML") (TxtProp "") container)
