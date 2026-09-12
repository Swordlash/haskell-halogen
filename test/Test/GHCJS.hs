module Test.GHCJS (spec) where

import Prelude

#if defined(javascript_HOST_ARCH)

import Data.Foreign (Foreign, foreignToBool)
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertWith)

foreign import javascript unsafe "(() => { return true; })" js_true :: Foreign Bool

foreign import javascript unsafe "(() => { return false; })" js_false :: Foreign Bool

spec :: Spec
spec = describe "GHCJS FFI" $ do
  it "converts true to True" $
    assertWith "foreignToBool should convert true to True" (foreignToBool js_true)
  it "converts false to False" $
    assertWith "foreignToBool should convert false to False" (not (foreignToBool js_false))

#else

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "GHCJS FFI" $ pure ()

#endif