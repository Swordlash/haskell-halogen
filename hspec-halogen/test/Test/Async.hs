module Test.Async (spec) where

import Example.Clock qualified as Clock
import Example.Loader qualified as Loader
import Data.IORef (newIORef, readIORef)
import GHC.Clock (getMonotonicTime)
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

  -- The timeout is a deadline on the clock: neither slow attempts nor a
  -- waiting 'find' inside multiply it.
  it "gives up at its deadline when every attempt is slow" $ do
    started <- getMonotonicTime
    outcome <- try @SomeException $ runPage $
      eventuallyWithin 300 (unsafeIOToPageM (threadDelay 200000) >> panic "not yet")
    took <- subtract started <$> getMonotonicTime
    unless (isLeft outcome && took < 1.5) $
      panic ("expected a failure within 1.5 s, got " <> either (const "a failure") (const "a success") outcome <> " after " <> show took <> " s")

  it "gives up at its deadline when what it waits for waits too" $ do
    started <- getMonotonicTime
    outcome <- try @SomeException $ runPage $ do
      ui <- mount m Loader.component (Loader.Input {delayMs = 0, result = "42"})
      eventuallyWithin 300 (void (find ui ".nowhere"))
    took <- subtract started <$> getMonotonicTime
    unless (isLeft outcome && took < 6) $
      panic ("expected a failure within 6 s, got " <> either (const "a failure") (const "a success") outcome <> " after " <> show took <> " s")
