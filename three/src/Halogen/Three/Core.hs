-- | A three-dimensional scene as a 'Halogen.VDom.Types.VDom', as
-- "Halogen.Canvas.Core" makes a canvas scene one: the same reconciler
-- builds and patches it, and only the backend's nodes differ (Three.js
-- objects instead of DOM elements or Pixi display objects).
--
-- Its widgets are thunks ('memoized', 'lazy'), never component slots, so
-- nothing below a scene needs a 'Halogen.Query.HalogenM.HalogenM'.
module Halogen.Three.Core
  ( Prop (..)
  , EventType (..)
  , eventName
  , propKey
  , ThreeEvent (..)
  , Modifiers (..)
  , noModifiers
  , Node (..)
  , unNode
  , element
  , keyed
  , memoized
  , lazy
  )
where

import Data.Coerce (coerce)
import Data.Foreign (unsafeRefEq)
import Halogen.Three.Types
import Halogen.VDom.Thunk (Thunk, mkThunk, unsafeThunkId)
import Halogen.VDom.Types (ElemName (..), VDom (..))
import Protolude hiding (Handler)

-- | The pointer events an object can raise. Three.js has none of its own:
-- the renderer listens on the canvas, casts a ray from the pointer into the
-- scene and hands the event to the nearest mesh it meets, or to the nearest
-- of that mesh's ancestors listening for it.
data EventType
  = -- | The pointer went down and up on the object, and did not turn the
    -- camera in between.
    Click
  | PointerDown
  | PointerUp
  | -- | The pointer came over the object (over a mesh in it, for a group).
    PointerOver
  | -- | The pointer left it.
    PointerOut
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The event's name, as the renderer's JavaScript names it.
eventName :: EventType -> Text
eventName = \case
  Click -> "click"
  PointerDown -> "pointerdown"
  PointerUp -> "pointerup"
  PointerOver -> "pointerover"
  PointerOut -> "pointerout"

-- | The modifier keys held during a pointer event, with the names the
-- browser gives them.
data Modifiers = Modifiers
  { shiftKey :: Bool
  , ctrlKey :: Bool
  , altKey :: Bool
  , metaKey :: Bool
  -- ^ Cmd on a Mac, the Windows key elsewhere.
  }
  deriving stock (Eq, Show)

noModifiers :: Modifiers
noModifiers = Modifiers {shiftKey = False, ctrlKey = False, altKey = False, metaKey = False}

-- | A pointer event on an object, read as it was dispatched.
data ThreeEvent = ThreeEvent
  { point :: Vec3
  -- ^ Where the ray met the mesh, in the scene's coordinates.
  , object :: Text
  -- ^ The 'Halogen.Three.Properties.name' of the mesh it met, which for a
  -- handler on a group is one of the group's meshes ("" for one without).
  , pressed :: Int
  -- ^ Which mouse button, in the @MouseEvent.button@ numbering.
  , held :: Modifiers
  }
  deriving stock (Eq, Show)

-- | A property of an object in the scene.
data Prop i
  = -- | A mesh's shape.
    Shape Geometry
  | -- | What a mesh is made of.
    Paint Material
  | Position Vec3
  | Rotation Euler
  | Scale Vec3
  | Visible Bool
  | -- | The object's name, which a 'ThreeEvent' reports.
    Name Text
  | -- | A light's colour and intensity.
    Light Color Double
  | Handler EventType (ThreeEvent -> Maybe i)
  deriving stock (Functor)

-- | The key a prop is diffed under: two props with the same key replace
-- each other across a patch. Handlers key by event, so that handlers for
-- different events do not displace one another.
propKey :: Prop i -> Text
propKey = \case
  Shape _ -> "geometry"
  Paint _ -> "material"
  Position _ -> "position"
  Rotation _ -> "rotation"
  Scale _ -> "scale"
  Visible _ -> "visible"
  Name _ -> "name"
  Light _ _ -> "light"
  Handler eventType _ -> "on/" <> eventName eventType

-- | A node of a scene.
newtype Node i = Node {unNode :: VDom [Prop i] (Thunk Node i)}

unNode :: Node i -> VDom [Prop i] (Thunk Node i)
unNode (Node vdom) = vdom

-- The action is inside the prop list, which is VDom's first parameter, so
-- mapping it goes through 'bimap'.
instance Functor Node where
  fmap f (Node vdom) = Node (bimap (map (fmap f)) (fmap f) vdom)

element :: ElemName -> [Prop i] -> [Node i] -> Node i
element name props children = Node (Elem Nothing name props (coerce children))

-- | Children with stable identities, so that a reorder moves objects
-- instead of rebuilding them.
keyed :: ElemName -> [Prop i] -> [(Text, Node i)] -> Node i
keyed name props children = Node (Keyed Nothing name props (coerce children))

-- | A part of the scene rendered from @a@, skipped by a render while @a@ is
-- equal by the given equality: its nodes are not built, not diffed, and
-- their objects are not touched. As 'Halogen.Canvas.Core.memoized', apply it
-- partially once rather than in each render: the function's identity is
-- part of what is compared.
memoized :: (a -> a -> Bool) -> (a -> Node i) -> a -> Node i
memoized eqFn f =
  -- Not eta-expanded: the partial application is what keeps @f@'s identity.
  Node . Widget <$> mkThunk (unsafeThunkId f) eqFn f

-- | 'memoized' by reference: skipped while the very same value comes back.
lazy :: (a -> Node i) -> a -> Node i
lazy = memoized unsafeRefEq
