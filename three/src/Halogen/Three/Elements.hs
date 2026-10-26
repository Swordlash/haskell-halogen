-- | The objects of a scene, in the shape "Halogen.HTML.Elements" uses: a
-- group takes props and children, a mesh its shape, its material and props.
--
-- A mesh's geometry and material are props of its node rather than nodes
-- of their own, so that a change of colour patches the material the mesh
-- has, and a change of shape gives it a new geometry and disposes of the
-- old one (Three.js keeps a geometry's buffers on the GPU until told to let
-- them go).
module Halogen.Three.Elements
  ( group
  , group_
  , keyedGroup
  , mesh
  , mesh_
  , ambientLight
  , directionalLight
  , pointLight
  , axes
  , memoized
  , lazy
  )
where

import Halogen.Three.Core
import Halogen.Three.Types
import Halogen.VDom.Types (ElemName (..))
import Protolude hiding (group)

-- | Objects moved, turned and scaled together.
group :: [Prop i] -> [Node i] -> Node i
group = element (ElemName "group")

group_ :: [Node i] -> Node i
group_ = group []

-- | A group whose children carry keys: a child keeps its object (and its
-- geometry and material) when the list is reordered.
keyedGroup :: [Prop i] -> [(Text, Node i)] -> Node i
keyedGroup = keyed (ElemName "group")

-- | A shape made of a material, at the origin unless a
-- 'Halogen.Three.Properties.position' says otherwise.
mesh :: Geometry -> Material -> [Prop i] -> Node i
mesh geometry material props = element (ElemName "mesh") (Shape geometry : Paint material : props) []

mesh_ :: Geometry -> Material -> Node i
mesh_ geometry material = mesh geometry material []

-- | Light from everywhere at once, of a colour and an intensity (1 is
-- Three.js's default): it shows the side a mesh turns away from the other
-- lights, which would be black without it.
ambientLight :: Color -> Double -> Node i
ambientLight color intensity = element (ElemName "ambientLight") [Light color intensity] []

-- | Light from far away, as the sun's, from this position towards the
-- origin: every mesh is lit from the same side.
directionalLight :: Color -> Double -> Vec3 -> Node i
directionalLight color intensity position =
  element (ElemName "directionalLight") [Light color intensity, Position position] []

-- | Light from a point, in every direction, as a bulb's. It falls off with
-- the square of the distance, as Three.js's physically correct lighting
-- has it, so a light a few units away needs an intensity of tens to show.
pointLight :: Color -> Double -> Vec3 -> Node i
pointLight color intensity position =
  element (ElemName "pointLight") [Light color intensity, Position position] []

-- | The axes from the origin, this long: x red, y green, z blue. For finding
-- one's way about a scene while writing it.
axes :: Double -> Node i
axes size = element (ElemName "axes") [Scale (Vec3 size size size)] []
