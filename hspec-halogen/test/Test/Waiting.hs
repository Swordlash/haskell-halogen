-- | Waiting for the program: 'waitQuiet' for what its fibers wait for in a
-- @liftIO@, and not for a pause in 'Halogen.delay'; and the deadline of
-- both it and 'settle', for a program that does not finish.
module Test.Waiting (spec) where

import Data.Text qualified as T
import Example.Waiting qualified as Waiting
import GHC.Clock (getMonotonicTime)
import Halogen qualified as H
import Protolude hiding (find)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec qualified as Hspec
import Test.Hspec.Halogen

-- | Seconds an action takes.
timed :: IO a -> IO (a, Double)
timed action = do
  started <- getMonotonicTime
  a <- action
  (a,) . subtract started <$> getMonotonicTime

-- | How a page that fails on purpose failed: its message's lines, and how
-- long it took.
failing :: (forall s. PageM s ()) -> IO ([Text], Double)
failing test =
  timed (try (runPage test)) >>= \case
    (Left (HUnitFailure _ (Reason message)), took) -> pure (T.lines (toS message), took)
    (Left other, _) -> panic ("another failure: " <> show other)
    (Right (), _) -> panic "the test passed"

-- | The loader, with its ticker.
loader :: forall m -> (MonadBrowserTest m) => PageM s (Mounted s H.VoidF Void m)
loader m = mount m (Waiting.component :: H.Component H.VoidF () Void m) ()

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  -- An action returns once the handler is waiting: what it waits for is
  -- 'waitQuiet''s, and after it one read sees what came back.
  it "waits for a fiber waiting in liftIO for a threadDelay" $ runPage $ do
    ui <- loader m
    status <- find ui ".status"
    find ui ".load" >>= click
    textContent status `shouldReturn` "loading"
    waitQuiet
    textContent status `shouldReturn` "loaded"

  it "waits for a fiber waiting in liftIO for a promise of the page's" $ runPage $ do
    ui <- loader m
    status <- find ui ".status"
    find ui ".load-promise" >>= click
    textContent status `shouldReturn` "loading"
    waitQuiet
    textContent status `shouldReturn` "loaded from a promise"

  -- The ticker beside them pauses in H.delay for ever: were it counted,
  -- the program would never be quiet.
  it "does not wait for a pause in delay" $ runPage $ do
    ui <- loader m
    ticks <- find ui ".ticks"
    eventually (textContent ticks >>= (`shouldSatisfy` (maybe False (> (2 :: Int)) . readMaybe . T.unpack)))
    started <- unsafeIOToPageM getMonotonicTime
    replicateM_ 5 waitQuiet
    finished <- unsafeIOToPageM getMonotonicTime
    (finished - started) `shouldSatisfy` (< 1)

  it "fails at its deadline, saying what still waits, with the tree's events and the program's reading" $ do
    (shown, took) <- failing $ do
      ui <- loader m
      find ui ".load-slowly" >>= click
      waitQuietWithin 200
    took `Hspec.shouldSatisfy` (< 1.5)
    case shown of
      line : _ -> do
        line `Hspec.shouldSatisfy` T.isPrefixOf "waitQuiet: the program was not quiet after 200 ms: "
        line `Hspec.shouldSatisfy` T.isInfixOf "1 fiber waiting in liftIO"
      [] -> Hspec.expectationFailure "an empty message"
    shown `Hspec.shouldContain` ["What the tree did (the last 50 events, oldest first):"]
    lastMay shown `Hspec.shouldSatisfy` maybe False (\l -> "The program then: 1 tree, " `T.isPrefixOf` l && "1 fiber waiting in liftIO." `T.isSuffixOf` l)

  -- A tree whose loop always has work: an action, and so the test, fails
  -- at the deadline rather than waiting for ever. The spinner stops a
  -- second later, which its unmounting waits for.
  it "fails an action on a program that never finishes, at the deadline, saying it was busy" $ do
    (shown, took) <- failing $ do
      ui <- mount m (Waiting.spinner :: H.Component H.VoidF Double Void m) 6
      spins <- find ui ".spins"
      eventually (textContent spins >>= (`shouldNotBe` "0"))
      settle
    took `Hspec.shouldSatisfy` (\t -> t >= 5 && t < 7.5)
    take 1 shown `Hspec.shouldSatisfy` all (T.isPrefixOf "settle: the program was still busy after 5000 ms: 1 of 1 tree running or with work queued")
    lastMay shown `Hspec.shouldSatisfy` maybe False (T.isPrefixOf "The program then: 1 tree, 1 running or with work queued")
