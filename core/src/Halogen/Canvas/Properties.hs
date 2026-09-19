-- | Canvas properties and event handlers, in the shape
-- "Halogen.HTML.Properties" and "Halogen.HTML.Events" use.
module Halogen.Canvas.Properties
  ( transform
  , at
  , cursor
  , eventMode
  , hitArea
  , onClick
  , onPointerDown
  , onPointerUp
  , onPointerOver
  , onPointerOut
  , onPointerMove
  , handler
  )
where

import HPrelude
import Halogen.Canvas.Core
import Halogen.Canvas.Types

transform :: Transform -> CanvasProp event i
transform = Place

-- | Position an element, leaving its scale and rotation alone.
at :: Point -> CanvasProp event i
at position = Place defaultTransform {position}

cursor :: Text -> CanvasProp event i
cursor = Cursor

eventMode :: EventMode -> CanvasProp event i
eventMode = Interactive

-- | Override the region an element is hit-tested against.
--
-- Needed more often than it looks: a stroke-only shape is otherwise hit only
-- along the stroke, and a group has no bounds of its own.
hitArea :: HitArea -> CanvasProp event i
hitArea = Hit

-- | Raise an action on an event, ignoring the event itself.
handler :: PointerEventType -> i -> CanvasProp event i
handler eventType action = Handler eventType (const (Just action))

onClick :: (event -> Maybe i) -> CanvasProp event i
onClick = Handler PointerTap

onPointerDown :: (event -> Maybe i) -> CanvasProp event i
onPointerDown = Handler PointerDown

onPointerUp :: (event -> Maybe i) -> CanvasProp event i
onPointerUp = Handler PointerUp

onPointerOver :: (event -> Maybe i) -> CanvasProp event i
onPointerOver = Handler PointerOver

onPointerOut :: (event -> Maybe i) -> CanvasProp event i
onPointerOut = Handler PointerOut

-- | Fires even after the pointer leaves the element, so a drag survives the
-- pointer outrunning it.
onPointerMove :: (event -> Maybe i) -> CanvasProp event i
onPointerMove = Handler PointerMoveGlobal
