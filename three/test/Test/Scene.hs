-- | What the elements build: a VDom whose element names are the objects the
-- renderer makes, and whose props are what the elements and properties say.
module Test.Scene (spec) where

import Halogen.Three qualified as Three
import Halogen.Three.Core
import Halogen.Three.Elements qualified as TE
import Halogen.Three.Properties qualified as TP
import Halogen.VDom.Types (ElemName (..), VDom (..))
import Protolude hiding (Handler)
import Test.Hspec

-- | A node as its name, its props' keys and its children (keyed ones with
-- their keys), or a marker for what is not an element.
data Shape
  = Element Text [Text] [Shape]
  | KeyedElement Text [Text] [(Text, Shape)]
  | Other Text
  deriving stock (Eq, Show)

shape :: VDom [Prop i] w -> Shape
shape = \case
  Elem _ (ElemName name) props children -> Element name (map propKey props) (map shape children)
  Keyed _ (ElemName name) props children -> KeyedElement name (map propKey props) (map (second shape) children)
  Text _ -> Other "text"
  Widget _ -> Other "widget"
  Grafted _ -> Other "graft"

shapeOf :: Node i -> Shape
shapeOf = shape . unNode

spec :: Spec
spec = do
  it "makes a mesh of its geometry, its material and its props" $ do
    let node = TE.mesh (Three.Box 1 2 3) (Three.standard 0xff0000) [TP.position (Three.Vec3 1 0 0), TP.name "box"] :: Node ()
    shapeOf node `shouldBe` Element "mesh" ["geometry", "material", "position", "name"] []
    case unNode node of
      Elem _ _ (Shape geometry : Paint material : _) _ -> do
        geometry `shouldBe` Three.Box 1 2 3
        material `shouldBe` Three.Standard {color = 0xff0000, opacity = 1, wireframe = False, metalness = 0, roughness = 1}
      _ -> expectationFailure "the mesh's first props are its geometry and its material"

  it "makes lights of a colour and an intensity, placed where they shine from" $ do
    shapeOf (TE.ambientLight 0xffffff 0.5 :: Node ()) `shouldBe` Element "ambientLight" ["light"] []
    shapeOf (TE.directionalLight 0xffffff 2 (Three.Vec3 3 5 2) :: Node ()) `shouldBe` Element "directionalLight" ["light", "position"] []
    shapeOf (TE.pointLight 0xffaa00 20 (Three.Vec3 0 2 0) :: Node ()) `shouldBe` Element "pointLight" ["light", "position"] []

  it "makes groups, keyed or not, of their children" $ do
    let ball = TE.mesh_ (Three.Sphere 1 32 16) (Three.basic 0x00ff00)
        scene = TE.group [TP.rotation (Three.Euler 0 1 0)] [ball, TE.keyedGroup [] [("a", ball), ("b", TE.axes 2)]] :: Node ()
    shapeOf scene
      `shouldBe` Element
        "group"
        ["rotation"]
        [ Element "mesh" ["geometry", "material"] []
        , KeyedElement "group" [] [("a", Element "mesh" ["geometry", "material"] []), ("b", Element "axes" ["scale"] [])]
        ]

  it "keys a handler by its event, so that handlers of different events stand side by side" $ do
    let props = [TP.onClick (const (Just ())), TP.onPointerOver (const Nothing), TP.handler PointerOut ()]
    map propKey props `shouldBe` ["on/click", "on/pointerover", "on/pointerout"]

  it "maps the actions of handlers deep in the scene" $ do
    let scene = fmap (* 2) (TE.group_ [TE.mesh (Three.Plane 1 1) (Three.phong 0) [TP.handler Click (21 :: Int)]])
        event = ThreeEvent {point = Three.Vec3 0 0 0, object = "", pressed = 0, held = Three.noModifiers}
    case unNode scene of
      Elem _ _ _ [Elem _ _ props _] -> [f event | Handler Click f <- props] `shouldBe` [Just 42]
      _ -> expectationFailure "a group of one mesh"

  it "gives a view no background, and maps its nodes" $ do
    let scene = Three.view Three.defaultCamera Three.defaultInteraction [TE.mesh (Three.Box 1 1 1) (Three.basic 0) [TP.handler Click (1 :: Int)]]
    scene.background `shouldBe` Nothing
    scene.camera `shouldBe` Three.defaultCamera
    length (fmap show scene :: Three.View Text).nodes `shouldBe` 1
