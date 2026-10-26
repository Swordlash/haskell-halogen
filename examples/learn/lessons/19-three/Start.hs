{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (component) where

import Clay qualified as C
import Control.Monad.State (modify)
import Control.Monad.UUID (MonadUUID)
import Data.Row (type (.==))
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.Three qualified as Three
import Halogen.Three.Elements qualified as TE
import Halogen.Three.Properties qualified as TP
import UnliftIO (MonadUnliftIO)

type Slots = "scene" .== H.Slot H.VoidF (Three.SceneEvent ()) ()

data Colour = Amber | Sky | Rose | Lime
  deriving (Show, Eq, Enum, Bounded)

-- The colour after this one, and after the last the first again.
next :: Colour -> Colour
next c = if c == maxBound then minBound else succ c

hex :: Colour -> Three.Color
hex Amber = 0xf59e0b
hex Sky = 0x38bdf8
hex Rose = 0xf43f5e
hex Lime = 0x84cc16

newtype State = State {colour :: Colour}

newtype Action = Scene (Three.SceneEvent ())

-- The nodes are a VDom tree, as a render's HTML is, of Three.js objects:
-- a floor, a cube and a sphere, lit by the room and by a sun.
scene :: State -> Three.View ()
scene st =
  Three.view
    Three.defaultCamera
    Three.defaultInteraction
    [ TE.ambientLight 0xffffff 0.6
    , TE.directionalLight 0xffffff 2.5 (Three.Vec3 3 5 4)
    , TE.mesh (Three.Plane 8 8) (Three.standard 0x334155) [TP.position (Three.Vec3 0 (-1) 0), TP.rotation (Three.Euler (-pi / 2) 0 0)]
    , TE.mesh (Three.Box 1.2 1.2 1.2) (Three.standard 0x6366f1) [TP.position (Three.Vec3 (-2.5) (-0.4) 0)]
    , TE.mesh (Three.Sphere 1 48 24) (Three.standard (hex st.colour)) [TP.onClick (\_ -> Just ())]
    ]

component :: forall q o m. (MonadUnliftIO m, MonadUUID m) => H.Component q () o m
component =
  H.component initialState render [H.onAction handleAction]
  where
    initialState _ = pure State {colour = Amber}
    render :: State -> H.ComponentHTML Action Slots m
    render st =
      HH.div_
        [ HH.div
            [HP.style (C.position C.relative >> C.width (C.px 360) >> C.height (C.px 240))]
            [HH.slot "scene" () Three.component (scene st) Scene]
        , HH.p_ [HH.text ("The sphere is " <> T.toLower (T.pack (show st.colour)) <> ". Click it, or drag to look around.")]
        ]
    handleAction (Scene (Three.Fired ())) = modify (\st -> st {colour = next st.colour})
    handleAction (Scene _) = pure ()
