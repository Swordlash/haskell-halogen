-- The material layer's program in the playground's test (../Playground.hs,
-- which says what it checks): a Material button, with material.js's ripple
-- and material.css's look.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad.State (modify)
import Data.Row (type (.==))
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.IO.Util qualified as HA
import Halogen.Material.Button qualified as Button
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))

type Slots = "button" .== H.Slot H.VoidF Button.ButtonClicked ()

data Action = Clicked

clicks :: H.Component q () o BrowserDOM
clicks =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure (0 :: Int)
      , H.render = render
      , H.eval = H.mkEval H.defaultEval {H.handleAction = \Clicked -> modify (+ 1)}
      }
  where
    render :: Int -> H.ComponentHTML Action Slots BrowserDOM
    render n =
      HH.div_
        [ HH.slot "button" () Button.button Button.emptyButtonSpec {Button.label = "Material", Button.style = Just Button.Raised} (const Clicked)
        , HH.p_ [HH.text ("material clicks: " <> T.pack (show n))]
        ]

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  socket <- runUI clicks () app
  HA.keepForNextRun socket.dispose
