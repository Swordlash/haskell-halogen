-- | A scene of one box at the origin, facing the default camera, with a
-- speck between the camera and its near plane in the way, in a component
-- that counts what the scene raises and can recolour the box or take it
-- away.
module Example.Scene (component, Query (..), Heard (..), boxName, firstColor, secondColor) where

import Clay qualified as C
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.UUID (MonadUUID)
import Data.Row (type (.==))
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Three qualified as Three
import Halogen.Three.Elements qualified as TE
import Halogen.Three.Properties qualified as TP
import Protolude hiding (State)

-- | How many clicks on the box the scene raised, and how many camera moves.
data Heard = Heard {clicks :: Int, moves :: Int}
  deriving stock (Eq, Show)

newtype Query a = GetHeard (Heard -> a)

data State = State {color :: Three.Color, present :: Bool, heard :: Heard}

data Action = Recolour | Remove | Scene (Three.SceneEvent ())

type Slots = "scene" .== H.Slot H.VoidF (Three.SceneEvent ()) ()

boxName :: Text
boxName = "box"

firstColor, secondColor :: Three.Color
firstColor = 0x3366ff
secondColor = 0xff6633

component :: forall i o m. (MonadUnliftIO m, MonadUUID m) => Three.Config -> H.Component Query i o m
component config =
  H.component (const (pure State {color = firstColor, present = True, heard = Heard 0 0})) render [H.onAction handleAction, H.onQuery handleQuery]
  where
    render :: State -> H.ComponentHTML Action Slots m
    render st =
      HH.div_
        [ HH.div
            [HP.class_ (HH.ClassName "stage"), HP.style (C.position C.relative >> C.width (C.px 300) >> C.height (C.px 200))]
            [HH.slot "scene" () (Three.componentWith config) (scene st) Scene]
        , -- Somewhere off the canvas to drag the pointer to.
          HH.div [HP.class_ (HH.ClassName "elsewhere"), HP.style (C.width (C.px 40) >> C.height (C.px 40))] []
        , HH.button [HP.class_ (HH.ClassName "recolour"), HE.onClick (const Recolour)] [HH.text "recolour"]
        , HH.button [HP.class_ (HH.ClassName "remove"), HE.onClick (const Remove)] [HH.text "remove"]
        , HH.p [HP.class_ (HH.ClassName "heard")] [HH.text ("clicks: " <> show st.heard.clicks <> ", moves: " <> show st.heard.moves)]
        ]

    scene :: State -> Three.View ()
    scene st =
      Three.view
        Three.defaultCamera
        Three.defaultInteraction
        ( TE.ambientLight 0xffffff 1
            : TE.mesh (Three.Box 0.02 0.02 0.02) (Three.basic 0x000000) [TP.position speck, TP.onClick (const Nothing)]
            : [TE.mesh (Three.Box 1 1 1) (Three.basic st.color) [TP.name boxName, TP.onClick (const (Just ()))] | st.present]
        )

    -- A speck on the camera's line of sight to the box, nearer to the camera
    -- than its near plane: it is never drawn, so a click through it is a
    -- click on the box. It listens so that a click picked on it by mistake
    -- would be its, not the box's.
    speck :: Three.Vec3
    speck =
      let Three.Vec3 x y z = Three.defaultCamera.position
          k = 1 - 0.05 / sqrt (x * x + y * y + z * z)
       in Three.Vec3 (k * x) (k * y) (k * z)

    handleAction :: Action -> H.HalogenM State Action Slots o m ()
    handleAction = \case
      Recolour -> modify (\st -> st {color = secondColor})
      Remove -> modify (\st -> st {present = False})
      Scene (Three.Fired ()) -> modify (\st -> st {heard = st.heard {clicks = st.heard.clicks + 1}})
      Scene (Three.CameraMoved _) -> modify (\st -> st {heard = st.heard {moves = st.heard.moves + 1}})

    handleQuery :: Query a -> H.HalogenM State Action Slots o m (Maybe a)
    handleQuery (GetHeard reply) = Just . reply <$> gets (.heard)
