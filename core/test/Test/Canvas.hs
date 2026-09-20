-- | The canvas scene language, which is pure: a scene is a 'VDom' value, so
-- everything about how it is built can be checked without a backend.
module Test.Canvas (spec) where

import Clay qualified as C
import Clay.Extra.Pointer qualified as CP
import Clay.Render qualified as CR
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
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
  , Cursor CursorPointer
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
      case unwrap (CE.circle (Point 0 0) 1 Nothing (Just stroke) [CP.cursor CursorGrab] :: CanvasNode () Action) of
        Elem _ _ props _ -> assertEqual "both props" ["draw", "cursor"] (map propKey props)
        _ -> error "expected an element"

  describe "positioning" $ do
    it "positions a label and a sprite, which have no geometry to carry it" $ do
      assertEqual "text" ["label", "place"] (keysOf (CE.text_ (Point 1 2) "hi" style))
      assertEqual "sprite" ["src", "place"] (keysOf (CE.sprite_ (Point 1 2) (Point 3 4) (Texture "t.png")))

    it "never emits two props under the place key" $ do
      let explicit = CP.at (Point 9 9)
      assertEqual "text" ["label", "place"] (keysOf (CE.text (Point 1 2) "hi" style [explicit]))
      assertEqual
        "sprite"
        ["src", "place"]
        (keysOf (CE.sprite (Point 1 2) (Point 3 4) (Texture "t.png") [explicit]))

    it "lets the caller's transform replace the default rather than shadow it" $ do
      let moved = CE.text (Point 1 2) "hi" style [CP.at (Point 9 9)] :: CanvasNode () Action
      assertEqual "the caller's position" [Point 9 9] (placements moved)

  describe "keys" $ do
    it "turns an element constructor into its keyed form" $ do
      let child = ("a", CE.circle_ (Point 0 0) 1 Nothing (Just stroke))
      case unwrap (CE.withKeys CE.group [CP.cursor CursorGrab] [child] :: CanvasNode () Action) of
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

  describe "cursors" $ do
    it "gives each cursor a name of its own" $ do
      let names = map cursorName everyCursor
      assertEqual "distinct names" (sort names) (sort (nub names))

    it "spells the families out in full" $ do
      assertEqual "resize" "nwse-resize" (cursorName (CursorResize ResizeNWSE))
      assertEqual "two words" "not-allowed" (cursorName CursorNotAllowed)
      -- CSS needs a keyword to fall back on when the image will not load.
      assertEqual "url" "url(cur.png), auto" (cursorName (CursorUrl "cur.png"))

  describe "touch-action" $ do
    it "renders the property Clay itself does not have" $
      assertEqual "inline css" "touch-action:none" (inlineCss (CP.touchAction C.none))

  describe "paths" $ do
    it "renders commands as an SVG d attribute" $
      assertEqual
        "d"
        ("M0.0, 0.0 L10.0, 0.0 z" :: Text)
        (pathData [SA.m SA.Abs 0 0, SA.l SA.Abs 10 0, SA.z])

-- | Every cursor, so that a name repeated by a copy-and-paste slip in the
-- table shows up as a collision.
everyCursor :: [Cursor]
everyCursor =
  [ CursorAuto
  , CursorDefault
  , CursorNone
  , CursorContextMenu
  , CursorHelp
  , CursorPointer
  , CursorProgress
  , CursorWait
  , CursorCell
  , CursorCrosshair
  , CursorText
  , CursorVerticalText
  , CursorAlias
  , CursorCopy
  , CursorMove
  , CursorNoDrop
  , CursorNotAllowed
  , CursorGrab
  , CursorGrabbing
  , CursorAllScroll
  , CursorZoomIn
  , CursorZoomOut
  , CursorUrl "cur.png"
  ]
    <> map
      CursorResize
      [ ResizeN
      , ResizeE
      , ResizeS
      , ResizeW
      , ResizeNE
      , ResizeNW
      , ResizeSE
      , ResizeSW
      , ResizeEW
      , ResizeNS
      , ResizeNESW
      , ResizeNWSE
      , ResizeCol
      , ResizeRow
      ]

inlineCss :: C.Css -> Text
inlineCss = toStrict . CR.renderWith CR.htmlInline []

keysOf :: CanvasNode () Action -> [Text]
keysOf node = case unwrap node of
  Elem _ _ props _ -> map propKey props
  _ -> error "expected an element"

placements :: CanvasNode () Action -> [Point]
placements node = case unwrap node of
  Elem _ _ props _ -> [p | Place Transform {position = p} <- props]
  _ -> error "expected an element"

fired :: [CanvasProp () Action] -> [Maybe Action]
fired props = [f () | Handler _ f <- props]
