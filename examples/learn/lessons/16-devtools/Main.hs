{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- The program around lesson 16 opens Halogen.DevTools' panel under the
-- lesson's component: #learn-result gets two elements, the panel goes into
-- the second and then the component into the first. The panel is opened
-- first so that it hears the component from its start, which is what lets
-- it outline the component and put its states back. DevTools.showing tells
-- it how to show the lesson's State. What it keeps for the next run ends the
-- component's tree and the panel's, and takes the two elements away.
module Main (main) where

import Data.Coerce (coerce)
import Halogen (HalogenSocket (..))
import Halogen.DevTools qualified as DevTools
import Halogen.HTML.Core (ElemName (..))
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (appendChild, createElement, document, elementToNode, removeChild, runBrowserDOM, window)
import Halogen.VDom.Driver (runUI)
import Lesson (State, component)
import Web.DOM.Internal.Types (Element (..), HTMLElement (..), toDocument, toNode)
import Web.DOM.ParentNode (QuerySelector (..))

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  result <- HA.selectElement (QuerySelector "#learn-result") >>= maybe (error "the page has no #learn-result") pure
  doc <- toDocument <$> (document =<< window)
  app <- createElement Nothing (ElemName "div") doc
  tools <- createElement Nothing (ElemName "div") doc
  appNode <- elementToNode app
  toolsNode <- elementToNode tools
  appendChild appNode (toNode result)
  appendChild toolsNode (toNode result)
  DevTools.showing @State
  panel <- DevTools.openIn (coerce tools :: HTMLElement)
  socket <- runUI component () (coerce app :: HTMLElement)
  HA.keepForNextRun $ do
    socket.dispose
    panel.dispose
    removeChild appNode (toNode result)
    removeChild toolsNode (toNode result)
