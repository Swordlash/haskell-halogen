module Main (main) where

-- On the JavaScript backend core's polyfills (jsbits) are what lets hspec
-- start under Node at all.
import Data.Foreign ()
import Protolude
import Test.Camera qualified
import Test.Hspec (describe, hspec)
import Test.Scene qualified

main :: IO ()
main = hspec $ do
  describe "the camera" Test.Camera.spec
  describe "the scene" Test.Scene.spec
