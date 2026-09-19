-- | The canvas scene language, which is pure: a scene is a 'VDom' value, so
-- everything about how it is built can be checked without a backend.
module Test.Canvas (spec) where

import Data.List (nub, sort)
import Data.Text (Text)
import Data.Void (Void)
import Halogen.Canvas.Core
import Halogen.Canvas.Elements qualified as CE
import Halogen.Canvas.Properties qualified as CP
import Halogen.Canvas.Types
import Halogen.Svg.Attributes qualified as SA
import Halogen.VDom.Types (ElemName (..), VDom (..))
import Prelude
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)

data Action = Clicked | Hovered
  deriving stock (Eq, Show)

stroke :: StrokeStyle
stroke = StrokeStyle {strokeColor = 0xffffff, strokeWidth = 1, strokeAlpha = 1}

style :: TextStyle
style = TextStyle {font = SystemFont "serif", fontSize = 12, textColor = 0, textAlign = AlignLeft}

-- | Every prop that is not a handler, so that the key test covers the whole
-- constructor set. A missing case here shows up as a duplicate key.
everyProp :: [CanvasProp () Action]
everyProp =
  [ Draw (Circle (Point 0 0) 1 Nothing (Just stroke))
  , Place defaultTransform
  , Label "hello" style
  , Src (Texture "tile.png") (Point 1 1)
  , Outline stroke 1
  , Interactive EventsStatic
  , Cursor "pointer"
  , Hit (CircleHit (Point 0 0) 1)
  ]

unwrap :: CanvasNode event i -> VDom [CanvasProp event i] Void
unwrap = unCanvasNode

spec :: Spec
spec = describe "canvas scenes" $ do
  describe "prop keys" $ do
    it "gives each kind of prop a key of its own" $ do
      let keys = map propKey everyProp
      assertEqual "distinct keys" (sort keys) (sort (nub keys))

    it "keys handlers by event, so two listeners do not displace each other" $ do
      let keys = map (\e -> propKey (Handler e (const (Just Clicked)) :: CanvasProp () Action)) events
          events = [PointerTap, PointerDown, PointerUp, PointerOver, PointerOut, PointerMoveGlobal]
      assertEqual "distinct keys" (length events) (length (nub keys))

    it "keys a handler apart from every other prop" $ do
      let handlerKey = propKey (Handler PointerTap (const (Just Clicked)) :: CanvasProp () Action)
      assertWith "handler key is its own" (handlerKey `notElem` map propKey everyProp)

  describe "elements" $ do
    it "names each primitive after the display object that draws it" $ do
      let named node = case unwrap node of
            Elem _ (ElemName name) _ _ -> name
            _ -> error "expected an element"
      assertEqual "group" "container" (named (CE.group_ [] :: CanvasNode () Action))
      assertEqual "circle" "graphics" (named (CE.circle_ (Point 0 0) 1 Nothing (Just stroke)))
      assertEqual "path" "graphics" (named (CE.path_ [SA.z] Nothing (Just stroke)))
      assertEqual "text" "text" (named (CE.text_ (Point 0 0) "hi" style))
      assertEqual "sprite" "sprite" (named (CE.sprite_ (Point 0 0) (Point 1 1) (Texture "t.png")))

    it "puts the geometry in a prop, not in the children" $ do
      case unwrap (CE.circle_ (Point 1 2) 3 Nothing (Just stroke) :: CanvasNode () Action) of
        Elem _ _ props children -> do
          assertEqual "no children" 0 (length children)
          assertEqual "the shape is the prop" ["draw"] (map propKey props)
        _ -> error "expected an element"

    it "keeps the styling props a caller passes alongside the geometry" $ do
      case unwrap (CE.circle (Point 0 0) 1 Nothing (Just stroke) [CP.cursor "grab"] :: CanvasNode () Action) of
        Elem _ _ props _ -> assertEqual "both props" ["draw", "cursor"] (map propKey props)
        _ -> error "expected an element"

  describe "keys" $ do
    it "turns an element constructor into its keyed form" $ do
      let child = ("a", CE.circle_ (Point 0 0) 1 Nothing (Just stroke))
      case unwrap (CE.withKeys CE.group [CP.cursor "grab"] [child] :: CanvasNode () Action) of
        Keyed _ (ElemName name) props children -> do
          assertEqual "same element" "container" name
          assertEqual "props survive" ["cursor"] (map propKey props)
          assertEqual "keys survive" ["a"] (map fst children)
        _ -> error "expected a keyed element"

  describe "fmap" $ do
    it "maps the action inside the prop list, which is not VDom's parameter" $ do
      let node = CE.circle (Point 0 0) 1 Nothing (Just stroke) [CP.onClick (const (Just Clicked))]
      case unwrap (fmap (const Hovered) (node :: CanvasNode () Action)) of
        Elem _ _ props _ ->
          assertEqual "the handler now yields the new action" [Just Hovered] (fired props)
        _ -> error "expected an element"

  describe "paths" $ do
    it "renders commands as an SVG d attribute" $
      assertEqual
        "d"
        ("M0.0, 0.0 L10.0, 0.0 z" :: Text)
        (pathData [SA.m SA.Abs 0 0, SA.l SA.Abs 10 0, SA.z])

fired :: [CanvasProp () Action] -> [Maybe Action]
fired props = [f () | Handler _ f <- props]
