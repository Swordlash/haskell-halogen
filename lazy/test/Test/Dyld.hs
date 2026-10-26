-- | "Halogen.Lazy.Dyld" where no page started the program with dyld:
-- natively, and under Node on both browser backends.
module Test.Dyld (spec) where

import Halogen qualified as H
import Halogen.Lazy qualified as Lazy
import Halogen.Lazy.Dyld (Library (..))
import Halogen.Lazy.Dyld qualified as Dyld
import Protolude
import Test.Harness
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldThrow)
import Test.Lazy (InnerQuery, loading)

spec :: Spec
spec = describe "Halogen.Lazy.Dyld" $ do
  it "puts a library under /tmp/halogen-lazy, by its file name" $ do
    Dyld.library "rootfs/layers/lazy/liblazy-demo.so?v=2" "entry"
      `shouldBe` Library {url = "rootfs/layers/lazy/liblazy-demo.so?v=2", path = "/tmp/halogen-lazy/liblazy-demo.so", entry = "entry"}

  it "is not available without a dyld, and load says so" $ do
    Dyld.available `shouldReturn` False
    (Dyld.load (Dyld.library "libfeature.so" "feature") :: IO Int) `shouldThrow` (== Dyld.NotAvailable)

  it "makes the wrapper show why it is not available" $ do
    let loader = liftIO (Dyld.load (Dyld.library "libfeature.so" "feature")) :: IO (H.Component InnerQuery Text Int IO)
    h <- start (Lazy.lazy loading loader) "a"
    eventually ["failed: " <> toS (displayException Dyld.NotAvailable)] (rendersOf "failed" h)
    dispose h
