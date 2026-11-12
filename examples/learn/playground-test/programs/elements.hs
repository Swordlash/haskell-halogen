-- The elements layer's program in the playground's test (../Playground.hs,
-- which says what it checks): a custom element defined over HTML in the
-- page, its attribute set from JavaScript.
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.State (put)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Halogen qualified as H
import Halogen.CustomElement (Definition (..), Registration (..))
import Halogen.CustomElement qualified as CE
import Halogen.HTML qualified as HH
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (BrowserDOM, PropValue (..), nodeToElement, runBrowserDOM, setProperty)
import Web.DOM.Internal.Types (toNode)
import Web.DOM.ParentNode (QuerySelector (..))
import Web.HTML.Common (PropName (..))

greeting :: H.Component q Text o BrowserDOM
greeting = H.component pure (\name -> HH.p_ [HH.text ("an element greets " <> name)]) [H.onReceive put]

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  container <- maybe (error "#app is not an element") pure =<< nodeToElement (toNode app)
  let html = setProperty (PropName "innerHTML") . TxtProp
  html "<x-playground-greeting name=\"Ada\"></x-playground-greeting>" container
  registration <- CE.define "x-playground-greeting" CE.defaultDefinition {input = fromMaybe "nobody" . CE.attr "name", observed = ["name"]} greeting
  HA.keepForNextRun (registration.dispose >> html "" container)
