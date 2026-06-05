{-# LANGUAGE CPP #-}

-- | JavaScript-backend-only tests. On any other architecture the spec is a
-- single pending item so the suite still builds and runs natively.
module Test.GHCJS (spec) where

import Prelude

import Test.Hspec

#if defined(javascript_HOST_ARCH)

import Data.Foreign (Foreign, foreignToBool)

foreign import javascript unsafe "(() => { return true; })" js_true :: Foreign Bool

foreign import javascript unsafe "(() => { return false; })" js_false :: Foreign Bool

spec :: Spec
spec = describe "Data.Foreign (GHC-JS)" $ do
  it "converts true to true" $
    foreignToBool js_true `shouldBe` True
  it "converts false to false" $
    foreignToBool js_false `shouldBe` False

#else

spec :: Spec
spec =
  describe "GHC-JS" $
    it "JavaScript-backend tests" $
      pendingWith "requires the JavaScript backend (arch javascript)"

#endif
