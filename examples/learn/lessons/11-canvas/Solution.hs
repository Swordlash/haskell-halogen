{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (component) where

import Clay qualified as C
import Control.Monad (void, when)
import Control.Monad.State (get, modify, put)
import Control.Monad.UUID (MonadUUID)
import Data.Row (type (.==))
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.Canvas.Elements qualified as CE
import Halogen.Canvas.Pixi qualified as Pixi
import Halogen.Canvas.Properties qualified as CP
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.Query.Event.Window qualified as Window
import UnliftIO (MonadUnliftIO)

type Slots = "canvas" .== H.Slot H.VoidF (Pixi.CanvasEvent Int) ()

data State = State {x :: Double, target :: Double}

data Action = Frame | Canvas (Pixi.CanvasEvent Int)

-- Where pad i is: three pads 100 pixels apart, the middle one at the origin.
padX :: Int -> Double
padX i = fromIntegral (i - 1) * 100

-- The nodes are a VDom tree, as a render's HTML is, of Pixi display objects.
scene :: State -> Pixi.View Int
scene st =
  Pixi.view
    Pixi.defaultCamera
    Pixi.defaultInteraction
    [ CE.rectangle_ (Pixi.Point 0 0) (Pixi.Point 320 200) (Just (Pixi.FillStyle 0x1e293b 1)) Nothing
    , CE.keyedGroup [] [(T.pack (show i), pad i) | i <- [0, 1, 2]]
    , CE.circle_ (Pixi.Point st.x (-30)) 18 (Just (Pixi.FillStyle 0xf59e0b 1)) Nothing
    ]
  where
    pad i = CE.rectangle (Pixi.Point (padX i) 60) (Pixi.Point 70 30) (Just (Pixi.FillStyle 0x38bdf8 1)) Nothing [CP.onClick (\_ -> Just i)]

component :: forall q o m. (MonadUnliftIO m, MonadUUID m) => H.Component q () o m
component =
  H.component initialState render [H.onInitialize startFrames, H.onAction handleAction]
  where
    initialState _ = pure State {x = padX 1, target = padX 1}
    render :: State -> H.ComponentHTML Action Slots m
    render st =
      HH.div_
        [ HH.div
            [HP.style (C.position C.relative >> C.width (C.px 320) >> C.height (C.px 200))]
            [HH.slot "canvas" () Pixi.component (scene st) Canvas]
        , HH.p_ [HH.text ("The ball is at x = " <> T.pack (show st.x) <> ". Click a pad.")]
        ]
    startFrames = void (H.subscribe (Frame <$ Window.animationFrame))
    -- Six pixels a frame towards the target; once there, nothing to render.
    handleAction Frame = do
      st <- get
      when (st.x /= st.target) (put st {x = st.x + max (-6) (min 6 (st.target - st.x))})
    handleAction (Canvas (Pixi.Fired i)) = modify (\st -> st {target = padX i})
    handleAction (Canvas _) = pure ()
