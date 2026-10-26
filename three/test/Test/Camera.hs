module Test.Camera (spec) where

import Halogen.Three.Camera
import Halogen.Three.Types
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "orbitBy" $ do
    it "turns the camera about its target, keeping its distance" $ do
      let camera = perspectiveAt (Vec3 0 0 5) (Vec3 0 0 0)
          turned = orbitBy (pi / 2) 0 camera
      turned.position `shouldBeNear` Vec3 5 0 0
      distance turned `shouldBeAbout` 5
      turned.target `shouldBe` Vec3 0 0 0

    it "turns about a target away from the origin" $ do
      let camera = perspectiveAt (Vec3 1 2 8) (Vec3 1 2 3)
      (orbitBy pi 0 camera).position `shouldBeNear` Vec3 1 2 (-2)

    it "turns the camera up and down with the polar angle" $ do
      let camera = perspectiveAt (Vec3 0 0 5) (Vec3 0 0 0)
          raised = orbitBy 0 (-(pi / 4)) camera
      raised.position `shouldBeNear` Vec3 0 (5 * sin (pi / 4)) (5 * cos (pi / 4))

    it "stops short of straight above and below" $ do
      let camera = perspectiveAt (Vec3 0 0 5) (Vec3 0 0 0)
          Vec3 _ above _ = (orbitBy 0 (-10) camera).position
          Vec3 _ below _ = (orbitBy 0 10 camera).position
      above `shouldSatisfy` (\y -> y < 5 && y > 4.99)
      below `shouldSatisfy` (\y -> y > -5 && y < -4.99)
      distance (orbitBy 0 (-10) camera) `shouldBeAbout` 5

    it "keeps an orthographic camera's height" $ do
      let camera = Orthographic {height = 10, near = 0.1, far = 100, position = Vec3 0 0 5, target = Vec3 0 0 0}
      case orbitBy 1 0 camera of
        Orthographic {height} -> height `shouldBe` 10
        other -> expectationFailure ("not orthographic: " <> show other)

  describe "zoomBy" $ do
    it "moves a perspective camera along its line of sight" $ do
      let camera = perspectiveAt (Vec3 0 3 4) (Vec3 0 0 0)
          nearer = zoomBy (Just (1, 50)) 0.5 camera
      nearer.position `shouldBeNear` Vec3 0 1.5 2
      distance nearer `shouldBeAbout` 2.5

    it "stays within the range" $ do
      let camera = perspectiveAt (Vec3 0 0 5) (Vec3 0 0 0)
      distance (zoomBy (Just (2, 8)) 0.01 camera) `shouldBeAbout` 2
      distance (zoomBy (Just (2, 8)) 100 camera) `shouldBeAbout` 8

    it "does nothing without a range" $ do
      let camera = perspectiveAt (Vec3 0 0 5) (Vec3 0 0 0)
      zoomBy Nothing 2 camera `shouldBe` camera

    it "changes what an orthographic camera shows, within the range" $ do
      let camera = Orthographic {height = 10, near = 0.1, far = 100, position = Vec3 0 0 5, target = Vec3 0 0 0}
      case zoomBy (Just (4, 20)) 3 camera of
        Orthographic {height, position} -> do
          height `shouldBe` 20
          position `shouldBe` Vec3 0 0 5
        other -> expectationFailure ("not orthographic: " <> show other)

  describe "followCamera" $ do
    let asked = perspectiveAt (Vec3 0 0 5) (Vec3 0 0 0)
        dragged = orbitBy 1 0 asked
    it "keeps the camera on screen while the view asks for the same one" $
      followCamera (Just asked) asked dragged `shouldBe` dragged
    it "follows a camera the view changed" $ do
      let moved = perspectiveAt (Vec3 0 0 9) (Vec3 0 0 0)
      followCamera (Just asked) moved dragged `shouldBe` moved
    it "follows the first view's camera" $
      followCamera Nothing asked dragged `shouldBe` asked

perspectiveAt :: Vec3 -> Vec3 -> Camera
perspectiveAt position target = Perspective {fov = 50, near = 0.1, far = 100, position, target}

shouldBeAbout :: (HasCallStack) => Double -> Double -> Expectation
shouldBeAbout actual expected =
  unless (abs (actual - expected) < 1e-9) $
    expectationFailure (show actual <> " is not about " <> show expected)

shouldBeNear :: (HasCallStack) => Vec3 -> Vec3 -> Expectation
shouldBeNear actual@(Vec3 x y z) expected@(Vec3 x' y' z') =
  unless (all (< 1e-9) [abs (x - x'), abs (y - y'), abs (z - z')]) $
    expectationFailure (show actual <> " is not near " <> show expected)
