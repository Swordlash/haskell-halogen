-- The three layer's program in the playground's test (../Playground.hs,
-- which says what it checks): a Three.js scene, one box at the origin the
-- camera looks at, clicked.
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
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.IO.Util qualified as HA
import Halogen.Three qualified as Three
import Halogen.Three.Elements qualified as TE
import Halogen.Three.Properties qualified as TP
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))

type Slots = "scene" .== H.Slot H.VoidF (Three.SceneEvent ()) ()

newtype Action = Scene (Three.SceneEvent ())

scene :: Three.View ()
scene =
  Three.view
    Three.defaultCamera
    Three.defaultInteraction
    [ TE.ambientLight 0xffffff 1
    , TE.mesh (Three.Box 2 2 2) (Three.basic 0x0ea5e9) [TP.onClick (const (Just ()))]
    ]

clicks :: H.Component q () o BrowserDOM
clicks =
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
            [HH.slot "scene" () Three.component scene Scene]
        , HH.p_ [HH.text ("three clicks: " <> T.pack (show n))]
        ]
    handle (Scene (Three.Fired ())) = modify (+ 1)
    handle (Scene _) = pure ()

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  socket <- runUI clicks () app
  HA.keepForNextRun socket.dispose
