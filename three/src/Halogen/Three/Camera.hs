-- | Moving a 'Camera' about its target, as the renderer's orbit interaction
-- does: in angles and factors, so that a program can move the camera the
-- same way from keys, a button or a timer.
--
-- The camera's offset from its target is taken in spherical coordinates, as
-- Three.js's @Spherical@ takes it: a distance, a polar angle from +y (0
-- straight above the target, @pi@ straight below) and an azimuth about y
-- (0 on +z, growing towards +x).
module Halogen.Three.Camera
  ( orbitBy
  , zoomBy
  , distance
  , followCamera
  , polarMargin
  )
where

import Halogen.Three.Types
import Protolude hiding (polar)

-- | The camera turned about its target: the azimuth and the polar angle
-- changed by these many radians, the distance kept. The polar angle stays
-- 'polarMargin' away from straight above and straight below, where "up"
-- would no longer say which way the camera is turned.
orbitBy :: Double -> Double -> Camera -> Camera
orbitBy dAzimuth dPolar camera =
  let (radius, polar, azimuth) = spherical (offset camera)
      polar' = max polarMargin (min (pi - polarMargin) (polar + dPolar))
   in setOffset camera (cartesian radius polar' (azimuth + dAzimuth))

-- | The camera nearer its target or further from it, by a factor (above 1
-- further, below 1 nearer), within the range if there is one: for a
-- perspective camera the distance, for an orthographic one the height of
-- what it shows, which is what makes it look nearer or further. Without a
-- range it does not zoom.
zoomBy :: Maybe (Double, Double) -> Double -> Camera -> Camera
zoomBy range factor camera = case range of
  Nothing -> camera
  Just (low, high) -> case camera of
    Orthographic {height} -> camera {height = bounded low high (height * factor)}
    Perspective {} ->
      let (radius, polar, azimuth) = spherical (offset camera)
       in if radius <= 0 then camera else setOffset camera (cartesian (bounded low high (radius * factor)) polar azimuth)
  where
    bounded low high = max low . min high

-- | How far the camera is from its target.
distance :: Camera -> Double
distance camera = let (radius, _, _) = spherical (offset camera) in radius

-- | Which camera a renderer shows, given the one the application asked for
-- last time, the one it asks for now and the one on screen: a view's camera
-- is followed only when it differs from what the application asked for
-- before, so that a view rendered mid-drag for another reason does not put
-- the camera back where the drag began. As
-- 'Halogen.Canvas.Types.followCamera' is for a canvas scene.
followCamera :: Maybe Camera -> Camera -> Camera -> Camera
followCamera asked next onScreen
  | asked == Just next = onScreen
  | otherwise = next

-- | How near straight above or below the target an orbit may take the
-- camera, in radians.
polarMargin :: Double
polarMargin = 0.01

offset :: Camera -> Vec3
offset camera =
  let Vec3 px py pz = camera.position
      Vec3 tx ty tz = camera.target
   in Vec3 (px - tx) (py - ty) (pz - tz)

setOffset :: Camera -> Vec3 -> Camera
setOffset camera (Vec3 x y z) =
  let Vec3 tx ty tz = camera.target
   in camera {position = Vec3 (tx + x) (ty + y) (tz + z)}

spherical :: Vec3 -> (Double, Double, Double)
spherical (Vec3 x y z) =
  let radius = sqrt (x * x + y * y + z * z)
   in if radius == 0 then (0, 0, 0) else (radius, acos (max (-1) (min 1 (y / radius))), atan2 x z)

cartesian :: Double -> Double -> Double -> Vec3
cartesian radius polar azimuth =
  Vec3 (radius * sin polar * sin azimuth) (radius * cos polar) (radius * sin polar * cos azimuth)
