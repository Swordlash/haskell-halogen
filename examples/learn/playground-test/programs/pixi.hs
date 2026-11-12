-- The pixi layer's program in the playground's test (../Playground.hs, which
-- says what it checks): a rectangle on a Pixi canvas, tapped.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Clay qualified as C
import Control.Monad.State (modify)
import Data.Row (type (.==))
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.Canvas.Elements qualified as CE
import Halogen.Canvas.Pixi qualified as Pixi
import Halogen.Canvas.Properties qualified as CP
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))

type Slots = "canvas" .== H.Slot H.VoidF (Pixi.CanvasEvent ()) ()

newtype Action = Canvas (Pixi.CanvasEvent ())

scene :: Pixi.View ()
scene =
  Pixi.view
    Pixi.defaultCamera
    Pixi.defaultInteraction
    [CE.rectangle (Pixi.Point 0 0) (Pixi.Point 400 400) (Just (Pixi.FillStyle 0x0ea5e9 1)) Nothing [CP.onClick (const (Just ()))]]

taps :: H.Component q () o BrowserDOM
taps =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure (0 :: Int)
      , H.render = render
      , H.eval = H.mkEval H.defaultEval {H.handleAction = handle}
      }
  where
    render :: Int -> H.ComponentHTML Action Slots BrowserDOM
    render n =
      HH.div_
        [ HH.div
            [HP.style (C.position C.relative >> C.width (C.px 300) >> C.height (C.px 200))]
            [HH.slot "canvas" () Pixi.component scene Canvas]
        , HH.p_ [HH.text ("pixi taps: " <> T.pack (show n))]
        ]
    handle (Canvas (Pixi.Fired ())) = modify (+ 1)
    handle (Canvas _) = pure ()

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  socket <- runUI taps () app
  HA.keepForNextRun socket.dispose
