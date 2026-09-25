module Test.Async (spec) where

import Example.Clock qualified as Clock
import Example.Loader qualified as Loader
import Data.IORef (newIORef, readIORef)
import Protolude hiding (find)
import Test.Hspec.Halogen

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  it "shows it is loading, then what it loaded" $ runPage $ do
    ui <- mount m Loader.component (Loader.Input {delayMs = 200, result = "42"})
    status <- find ui ".status"
    status `shouldHaveText` "Loading..."
    status `shouldHaveText` "Loaded 42"

  it "ticks while it is mounted" $ runPage $ do
    counter <- unsafeIOToPageM (newIORef 0)
    ui <- mount m Clock.component counter
    ticks <- find ui ".ticks"
    eventually $ do
      shown <- textContent ticks
      (readMaybe shown :: Maybe Int) `shouldSatisfy` maybe False (>= 3)

  it "stops its timer once it is unmounted" $ runPage $ do
    counter <- unsafeIOToPageM (newIORef 0)
    ui <- mount m Clock.component counter
    eventually $ unsafeIOToPageM (readIORef counter) >>= (`shouldSatisfy` (>= 2))
    unmount ui
    stopped <- unsafeIOToPageM (readIORef counter)
    unsafeIOToPageM (threadDelay 200000)
    unsafeIOToPageM (readIORef counter) `shouldReturn` stopped
