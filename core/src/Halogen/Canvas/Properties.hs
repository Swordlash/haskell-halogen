-- | Canvas properties and event handlers, in the shape
-- "Halogen.HTML.Properties" and "Halogen.HTML.Events" use.
module Halogen.Canvas.Properties
  ( transform
  , at
  , cursor
  , eventMode
  , hitArea
  , outline
  , cacheAsTexture
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

cursor :: Cursor -> CanvasProp event i
cursor = Cursor

eventMode :: EventMode -> CanvasProp event i
eventMode = Interactive

-- | Override the region an element is hit-tested against.
--
-- Needed more often than it looks: a stroke-only shape is otherwise hit only
-- along the stroke, and a group has no bounds of its own.
hitArea :: HitArea -> CanvasProp event i
hitArea = Hit

-- | Draw a border around the element, the given distance outside its
-- measured extent.
--
-- The extent is whatever the backend laid out and hit-tests against, so
-- this frames exactly the region that responds to a pointer - which is
-- something the scene itself has no way to work out for a label, or for a
-- sprite whose texture has not arrived.
outline :: StrokeStyle -> Double -> CanvasProp event i
outline = Outline

-- | Draw a group into a texture once, at the given resolution (1: a pixel
-- of the texture to a unit of the scene), and show the texture until
-- something inside the group changes, when it is drawn again (once a
-- frame at most). For a group of many shapes that seldom change: panning
-- and zooming then draw one texture instead of all of them. Zoomed in past
-- the resolution, the group looks as soft as the texture is.
cacheAsTexture :: Double -> CanvasProp event i
cacheAsTexture = CacheAsTexture

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
