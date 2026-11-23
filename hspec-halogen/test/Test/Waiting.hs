-- | Waiting for the program: 'waitQuiet' for what its fibers wait for in an
-- @H.async@, and not for a pause in 'Halogen.delay'; and the deadline of
-- both it and 'settle', for a program that does not finish.
module Test.Waiting (spec) where

import Data.Text qualified as T
import Example.Waiting qualified as Waiting
import GHC.Clock (getMonotonicTime)
import Halogen qualified as H
import Halogen.Activity (Activity (..))
import Halogen.Activity.Control (advance, withTestClock)
import Protolude hiding (find)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec qualified as Hspec
import Test.Hspec.Halogen
import Test.Settings (keepingSettings, reportingRetries, spinning)

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
  it "waits for a fiber waiting in H.async for a threadDelay" $ runPage $ do
    ui <- loader m
    status <- find ui ".status"
    find ui ".load" >>= click
    textContent status `shouldReturn` "loading"
    waitQuiet
    textContent status `shouldReturn` "loaded"

  it "waits for a fiber waiting in H.async for a promise of the page's" $ runPage $ do
    ui <- loader m
    status <- find ui ".status"
    find ui ".load-promise" >>= click
    textContent status `shouldReturn` "loading"
    waitQuiet
    textContent status `shouldReturn` "loaded from a promise"

  -- Under the test's clock a counted pause sleeps until the test advances
  -- the clock, and waitQuiet does not wait for it: were it waited for, the
  -- wait would run to its deadline and fail. Advancing the clock wakes it,
  -- and the wait after that is for what it led to.
  it "is quiet with a counted pause asleep on the test's clock, and waits for what advancing the clock wakes" $
    withTestClock $ \clock -> runPage $ do
      ui <- loader m
      status <- find ui ".status"
      find ui ".load-later" >>= click
      waitQuiet
      textContent status `shouldReturn` "loading"
      reading <- activity
      (reading.waiting, reading.asleep) `shouldBe` (1, 1)
      perform (advance clock 2)
      waitQuiet
      textContent status `shouldReturn` "loaded later"
      (.asleep) <$> activity `shouldReturn` 0

  -- A test that forgot to advance the clock finds the program quiet before
  -- the work after the pause: its read fails at once, and the failure names
  -- the pause from the clock, with its component, length and due time. The
  -- ticker's ticks (its H.delay sleeps on the clock too) put more events
  -- than a failure shows between the pause and the read, so the name does
  -- not come from the tree's events.
  readsOnce Quiet $ it "fails a read that forgot to advance the clock, naming the pause asleep on it" $ do
    (shown, _) <- withTestClock $ \clock -> failing $ do
      ui <- loader m
      find ui ".load-later" >>= click
      perform (advance clock 1)
      find ui ".status" >>= (`shouldHaveText` "loaded later")
    shown `Hspec.shouldContain` ["expected text \"loaded later\", found \"loading\""]
    shown `Hspec.shouldSatisfy` any (T.isInfixOf "1 fiber waiting in liftIO, 1 of them asleep on the test's clock")
    shown `Hspec.shouldSatisfy` (not . any (T.isInfixOf "a counted pause of 2"))
    shown `Hspec.shouldContain` ["Asleep on the test's clock (advance it to wake them):"]
    shown `Hspec.shouldSatisfy` any (\l -> "  Example.Waiting.State#" `T.isPrefixOf` l && ": a counted pause (delayCounted) of 2s, due at 2s on the test's clock, which is at 1s" `T.isSuffixOf` l)

  -- The ticker beside them pauses in H.delay for ever: were it counted,
  -- the program would never be quiet, and each waitQuiet would fail at its
  -- deadline. That they return is the check, at any speed.
  it "does not wait for a pause in delay" $ runPage $ do
    ui <- loader m
    ticks <- find ui ".ticks"
    eventually (textContent ticks >>= (`shouldSatisfy` (maybe False (> (2 :: Int)) . readMaybe . T.unpack)))
    replicateM_ 5 waitQuiet

  -- A request that never comes back: the wait fails at its deadline, at
  -- any speed, and says what the program was waiting for.
  it "fails at its deadline, saying what still waits, with the tree's events and the program's reading" $ do
    (shown, took) <- failing $ do
      ui <- loader m
      find ui ".load-forever" >>= click
      waitQuietWithin 200
    took `Hspec.shouldSatisfy` (>= 0.2)
    case shown of
      line : _ -> do
        line `Hspec.shouldSatisfy` T.isPrefixOf "waitQuiet: the program was not quiet after 200 ms: "
        line `Hspec.shouldSatisfy` T.isInfixOf "1 fiber waiting in liftIO"
      [] -> Hspec.expectationFailure "an empty message"
    shown `Hspec.shouldContain` ["What the tree did (the last 50 events, oldest first):"]
    lastMay shown `Hspec.shouldSatisfy` maybe False (\l -> "The program then: 1 tree, " `T.isPrefixOf` l && "1 fiber waiting in liftIO." `T.isSuffixOf` l)

  -- A tree whose loop always has work: an action, and so the test, fails
  -- at the deadline rather than waiting for ever. The deadline is the
  -- test's own, a second, whatever the runner's is. The spinner spins until
  -- the settle has run out (its line in the report), so it is busy for the
  -- whole second at any speed: the failure says it was still running, and
  -- the lower bound follows from the deadline.
  keepingSettings $ it "fails an action on a program that never finishes, at the deadline, saying it was busy" $ do
    reportingRetries
    (shown, took) <- failing $ do
      spinning m
      withDeadline 1000 settle
    took `Hspec.shouldSatisfy` (>= 1)
    take 1 shown `Hspec.shouldSatisfy` all (T.isPrefixOf "settle: the program was still busy after 1000 ms: 1 of 1 tree running or with work queued")
    lastMay shown `Hspec.shouldSatisfy` maybe False (T.isPrefixOf "The program then: 1 tree, 1 running or with work queued")
