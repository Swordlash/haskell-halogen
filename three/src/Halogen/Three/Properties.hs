-- | The properties of a scene's objects and their pointer handlers, in the
-- shape "Halogen.HTML.Properties" and "Halogen.HTML.Events" use.
--
-- Every prop undoes itself when a patch drops it: an object whose
-- 'position' goes back to the origin, a mesh whose handler goes stops
-- listening.
module Halogen.Three.Properties
  ( position
  , rotation
  , scale
  , visible
  , name
  , onClick
  , onPointerDown
  , onPointerUp
  , onPointerOver
  , onPointerOut
  , handler
  )
where

import Halogen.Three.Core
import Halogen.Three.Types
import Protolude hiding (Handler)

-- | Where the object is, relative to its group (the scene's origin at the
-- top).
position :: Vec3 -> Prop i
position = Position

-- | How the object is turned, about its own origin.
rotation :: Euler -> Prop i
rotation = Rotation

-- | How much larger the object is along each axis (1 is its own size).
scale :: Vec3 -> Prop i
scale = Scale

-- | Whether the object, and everything in it, is drawn and can be hit.
visible :: Bool -> Prop i
visible = Visible

-- | A name for the object, which the events of a mesh in it report
-- ('Halogen.Three.hitName').
name :: Text -> Prop i
name = Name

-- | Raise an action on an event, whatever the event says.
handler :: EventType -> i -> Prop i
handler eventType action = Handler eventType (const (Just action))

-- | The pointer went down and up on the object without turning the camera
-- in between.
onClick :: (ThreeEvent -> Maybe i) -> Prop i
onClick = Handler Click

onPointerDown :: (ThreeEvent -> Maybe i) -> Prop i
onPointerDown = Handler PointerDown

onPointerUp :: (ThreeEvent -> Maybe i) -> Prop i
onPointerUp = Handler PointerUp

-- | The pointer came over the object.
onPointerOver :: (ThreeEvent -> Maybe i) -> Prop i
onPointerOver = Handler PointerOver

-- | The pointer left the object.
onPointerOut :: (ThreeEvent -> Maybe i) -> Prop i
onPointerOut = Handler PointerOut
