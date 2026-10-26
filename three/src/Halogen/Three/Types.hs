-- | The vocabulary a three-dimensional scene is described in: points and
-- angles, the shapes a mesh can take and what it is made of, and the camera
-- that looks at it.
--
-- Plain values, compared with 'Eq': the renderer patches a mesh's material
-- or swaps its geometry exactly when the value differs from the one it
-- rendered last, as a canvas scene repaints a drawing whose 'Eq' says it
-- changed.
module Halogen.Three.Types
  ( Vec3 (..)
  , Euler (..)
  , Color
  , Geometry (..)
  , Material (..)
  , basic
  , standard
  , phong
  , Camera (..)
  , defaultCamera
  , Interaction (..)
  , defaultInteraction
  , noInteraction
  )
where

import Protolude

-- | A point or a direction in the scene. Three.js's axes: x to the right,
-- y up, z towards the viewer of the default camera.
data Vec3 = Vec3 Double Double Double
  deriving stock (Eq, Show)

-- | A rotation, as angles in radians about the x, y and z axes, applied in
-- that order (Three.js's default @XYZ@).
data Euler = Euler Double Double Double
  deriving stock (Eq, Show)

-- | A colour as a 24-bit RGB number, @0xRRGGBB@, in sRGB as a page's colours
-- are, as the canvas scene's fills take it.
type Color = Int

-- | The shape of a mesh, centred on its origin.
data Geometry
  = -- | Width (x), height (y) and depth (z).
    Box Double Double Double
  | -- | Radius, and how many segments around and from pole to pole: more is
    -- rounder, and costs more to draw. 32 and 16 look round at most sizes.
    Sphere Double Int Int
  | -- | Width (x) and height (y), facing +z: rotate it by @-pi / 2@ about x
    -- for a floor.
    Plane Double Double
  | -- | The top's radius, the bottom's, the height (y) and how many
    -- segments around. A radius of 0 makes a cone.
    Cylinder Double Double Double Int
  | -- | The ring's radius, the tube's, and how many segments around the tube
    -- and around the ring.
    Torus Double Double Int Int
  deriving stock (Eq, Show)

-- | What a mesh is made of, as Three.js's materials of the same names.
--
-- A change of colour, opacity or another field patches the material the
-- mesh has; a change from one kind to another replaces it, and the old one
-- is disposed of.
data Material
  = -- | Not lit: the colour as it is, whatever the lights. For a marker, or
    -- a scene without lights.
    Basic {color :: Color, opacity :: Double, wireframe :: Bool}
  | -- | Lit physically: @metalness@ from 0 (plastic, wood) to 1 (metal),
    -- @roughness@ from 0 (a mirror) to 1 (chalk). Without a light it is
    -- black.
    Standard {color :: Color, opacity :: Double, wireframe :: Bool, metalness :: Double, roughness :: Double}
  | -- | Lit with a highlight, cheaper than 'Standard': the higher
    -- @shininess@, the smaller and sharper the highlight.
    Phong {color :: Color, opacity :: Double, wireframe :: Bool, shininess :: Double}
  deriving stock (Eq, Show)

-- | A 'Basic' material of this colour, opaque.
basic :: Color -> Material
basic color = Basic {color, opacity = 1, wireframe = False}

-- | A 'Standard' material of this colour, opaque, not metal and fairly
-- rough, as Three.js's defaults are.
standard :: Color -> Material
standard color = Standard {color, opacity = 1, wireframe = False, metalness = 0, roughness = 1}

-- | A 'Phong' material of this colour, opaque, with Three.js's default
-- shininess.
phong :: Color -> Material
phong color = Phong {color, opacity = 1, wireframe = False, shininess = 30}

-- | Where the scene is seen from. The camera is at @position@, looks at
-- @target@ and keeps +y up; what it shows is cut off nearer than @near@ and
-- further than @far@. The canvas's shape gives the width.
data Camera
  = -- | Things further away look smaller: @fov@ is the vertical field of
    -- view, in degrees.
    Perspective {fov :: Double, near :: Double, far :: Double, position :: Vec3, target :: Vec3}
  | -- | Things look as large wherever they are: @height@ is how much of the
    -- scene the canvas shows from its bottom to its top, in the scene's
    -- units.
    Orthographic {height :: Double, near :: Double, far :: Double, position :: Vec3, target :: Vec3}
  deriving stock (Eq, Show)

-- | A perspective camera above and to the right of the origin, looking at
-- it, about eight units away: a scene a few units across, around the
-- origin, fills most of the canvas.
defaultCamera :: Camera
defaultCamera = Perspective {fov = 50, near = 0.1, far = 1000, position = Vec3 4 3 6, target = Vec3 0 0 0}

-- | How the reader may move the camera.
data Interaction = Interaction
  { orbit :: Bool
  -- ^ A drag turns the camera about its target, keeping its distance, as
  -- Three.js's OrbitControls do.
  , zoomRange :: Maybe (Double, Double)
  -- ^ The wheel moves the camera towards its target or away, within these
  -- distances (a perspective camera), or shows less or more of the scene,
  -- within these heights (an orthographic one). 'Nothing': the wheel does
  -- nothing, and scrolls the page.
  }
  deriving stock (Eq, Show)

-- | Drag to orbit, and wheel to zoom from 1 to 50 units away.
defaultInteraction :: Interaction
defaultInteraction = Interaction {orbit = True, zoomRange = Just (1, 50)}

-- | The camera stays where the view puts it.
noInteraction :: Interaction
noInteraction = Interaction {orbit = False, zoomRange = Nothing}
