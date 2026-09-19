-- | The canvas counterpart to "Halogen.HTML.Core".
--
-- A scene is a 'Halogen.VDom.Types.VDom' like any other, so it reconciles
-- through 'Halogen.VDom.DOM.buildVDom' rather than through a reconciler each
-- backend writes for itself. The widget slot is 'Void': a canvas scene holds
-- no component slots, which is also why nothing below a canvas ever needs a
-- 'Halogen.Query.HalogenM.HalogenM'.
module Halogen.Canvas.Core
  ( CanvasProp (..)
  , PointerEventType (..)
  , pointerEventName
  , CanvasNode (..)
  , unCanvasNode
  , CanvasElem
  , CanvasLeaf
  , propKey
  , element
  , keyed
  , withKeys
  )
where

import HPrelude
import Halogen.Canvas.Types
import Halogen.VDom.Types (ElemName (..), VDom (..))

-- | The events a canvas element can raise.
--
-- Pointer events rather than mouse events: one set covers mouse, touch and
-- pen, which is what a canvas wants when the same scene is dragged with a
-- finger and clicked with a mouse.
data PointerEventType
  = PointerTap
  | PointerDown
  | PointerUp
  | PointerOver
  | PointerOut
  | -- | Fires even once the pointer has left this element's bounds, which is
    -- what a drag needs to survive being dragged quickly.
    PointerMoveGlobal
  deriving stock (Eq, Ord, Show)

pointerEventName :: PointerEventType -> Text
pointerEventName = \case
  PointerTap -> "pointertap"
  PointerDown -> "pointerdown"
  PointerUp -> "pointerup"
  PointerOver -> "pointerover"
  PointerOut -> "pointerout"
  PointerMoveGlobal -> "globalpointermove"

-- | A property of a canvas element.
--
-- @event@ is the backend's pointer event; it is a parameter because a
-- federated canvas event is not a DOM one - it carries a position local to
-- the element that was hit, which has no DOM analogue.
data CanvasProp event i
  = -- | What a @graphics@ element draws.
    Draw Shape
  | -- | Where a @container@, @text@ or @sprite@ sits.
    Place Transform
  | -- | The string and style of a @text@ element.
    --
    -- One prop, not two, for the same reason 'Draw' is one: a backend sets a
    -- label's text and its style together, and splitting them would make the
    -- order the two were applied in observable.
    Label Text TextStyle
  | -- | The texture and size of a @sprite@ element.
    Src Texture Point
  | Handler PointerEventType (event -> Maybe i)
  | Interactive EventMode
  | Cursor Text
  | Hit HitArea
  deriving stock (Functor)

-- | The key a prop is diffed under.
--
-- Two props with the same key replace each other across a patch; props with
-- different keys are independent. Handlers key by event type so that
-- listeners for different events do not displace one another.
propKey :: CanvasProp event i -> Text
propKey = \case
  Draw _ -> "draw"
  Place _ -> "place"
  Label _ _ -> "label"
  Src _ _ -> "src"
  Handler eventType _ -> "on/" <> pointerEventName eventType
  Interactive _ -> "eventMode"
  Cursor _ -> "cursor"
  Hit _ -> "hitArea"

newtype CanvasNode event i = CanvasNode {unCanvasNode :: VDom [CanvasProp event i] Void}

unCanvasNode :: CanvasNode event i -> VDom [CanvasProp event i] Void
unCanvasNode (CanvasNode vdom) = vdom

-- Not derivable: the action sits inside the attribute list, which is VDom's
-- first parameter, so mapping it goes through 'first' rather than 'fmap'.
instance Functor (CanvasNode event) where
  fmap f (CanvasNode vdom) = CanvasNode (first (map (fmap f)) vdom)

type CanvasElem event i = [CanvasProp event i] -> [CanvasNode event i] -> CanvasNode event i

type CanvasLeaf event i = [CanvasProp event i] -> CanvasNode event i

element :: ElemName -> CanvasElem event i
element name props children =
  CanvasNode $ Elem Nothing name props (coerce children)

-- | Give children stable identities, so a reorder moves display objects
-- instead of repainting them.
keyed :: ElemName -> [CanvasProp event i] -> [(Text, CanvasNode event i)] -> CanvasNode event i
keyed name props children =
  CanvasNode $ Keyed Nothing name props (coerce children)

-- | Turn any element constructor into its keyed form, as
-- 'Halogen.HTML.Elements.withKeys' does for HTML.
withKeys
  :: CanvasElem event i
  -> [CanvasProp event i]
  -> [(Text, CanvasNode event i)]
  -> CanvasNode event i
withKeys ctor props children =
  case ctor props [] of
    CanvasNode (Elem ns name as _) -> CanvasNode (Keyed ns name as (coerce children))
    node -> node
