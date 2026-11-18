-- | 'readsOnce': the reads of a group look once, after the program has
-- come to the group's rest, and say so when they fail; the waits a test
-- holds on purpose ('countHeld'); the steps that make the reads rest again
-- ('query', 'perform'); 'eventually' kept as the named wait, and reported;
-- and 'mountUnsettled', for a program busy from its start.
module Test.ReadsOnce (spec) where

import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Row (Empty)
import Data.Text qualified as T
import Example.Waiting qualified as Waiting
import GHC.Clock (getMonotonicTime)
import Halogen qualified as H
import Halogen.Activity (Activity (..))
import Halogen.Activity.Control (advance, withTestClock)
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Subscription qualified as HS
import Protolude hiding (find)
import GHC.Stack (SrcLoc (..))
import System.Timeout (timeout)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec qualified as Hspec
import Test.Hspec.Halogen
import Test.Settings (keepingSettings, reportedFrom, reportingRetries, spinning)

-- | The loader, with its ticker, which pauses in 'H.delay' and so never
-- keeps the program from being quiet.
loader :: forall m -> (MonadBrowserTest m) => PageM s (Mounted s H.VoidF Void m)
loader m = mount m (Waiting.component :: H.Component H.VoidF () Void m) ()

-- | How a page that fails on purpose failed, and how many seconds it took.
failing :: (forall s. PageM s ()) -> IO (Text, Double)
failing test = do
  started <- getMonotonicTime
  try (runPage test) >>= \case
    Left (HUnitFailure _ (Reason message)) -> (toS message,) . subtract started <$> getMonotonicTime
    Left other -> panic ("another failure: " <> show other)
    Right () -> panic "the test passed"

-- | A failure of a page that fails on purpose, with the file and line it
-- names.
failingAt :: (forall s. PageM s ()) -> IO (Text, Maybe Text)
failingAt test =
  try (runPage test) >>= \case
    Left (HUnitFailure loc (Reason message)) -> pure (toS message, (\l -> toS (srcLocFile l) <> ":" <> show (srcLocStartLine l)) <$> loc)
    Left other -> panic ("another failure: " <> show other)
    Right () -> panic "the test passed"

-- | A status that work started outside the test's steps changes: a message
-- pushed to its listener (which the test asks for, and sends to as a
-- server would) and a pause in 'H.delay' after a click ('later'). Each message loads until the test opens the gate
-- it is given, so that it is under way for as long as the test needs.
data Pushing = Pushing {status :: Text, listener :: Maybe (HS.Listener IO Text), gate :: MVar ()}

data PushAction = StartPushing | Arrived Text | Later

newtype PushQuery a = GetListener (HS.Listener IO Text -> a)

pushing :: forall o m. (MonadIO m) => H.Component PushQuery (MVar ()) o m
pushing =
  H.component
    (\gate -> pure Pushing {status = "idle", listener = Nothing, gate})
    render
    [H.onInitialize (onPush StartPushing), H.onAction onPush, H.onQuery (\(GetListener reply) -> fmap reply <$> gets (.listener))]
  where
    render :: Pushing -> H.ComponentHTML PushAction Empty m
    render st =
      HH.div_
        [ HH.p [HP.class_ (HH.ClassName "status")] [HH.text st.status]
        , HH.button [HP.class_ (HH.ClassName "later"), HE.onClick (const Later)] [HH.text "later"]
        ]
    onPush :: PushAction -> H.HalogenM Pushing PushAction Empty o m ()
    onPush = \case
      StartPushing -> do
        HS.Subscribe {emitter, listener} <- H.sync (liftIO HS.create)
        void (H.subscribe (Arrived <$> emitter))
        modify (\st -> st {listener = Just listener})
      Arrived label -> do
        modify (\st -> st {status = label <> ": loading"})
        gate <- gets (.gate)
        H.async (liftIO (readMVar gate))
        modify (\st -> st {status = label <> ": loaded"})
      Later -> H.delay 0.01 >> onPush (Arrived "later")

-- | A count that a query adds one to.
data Bump a = Bump a

bumper :: forall i o m. (Monad m) => H.Component Bump i o m
bumper =
  H.component
    (const (pure (0 :: Int)))
    (\count -> HH.p [HP.class_ (HH.ClassName "bumps")] [HH.text (show count)] :: H.ComponentHTML () Empty m)
    [H.onQuery (\(Bump a) -> Just a <$ modify (+ 1))]

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  describe "at quiet" $ readsOnce Quiet $ do
    -- No waitQuiet: the read waits for the load's answer itself.
    it "reads what the answer to a click rendered, looking once" $ runPage $ do
      ui <- loader m
      status <- find ui ".status"
      find ui ".load" >>= click
      status `shouldHaveText` "loaded"
      find ui ".load-promise" >>= click
      textContent status `shouldReturn` "loaded from a promise"

    -- A load that never comes back, held on purpose: the read and the
    -- waitQuiet wait for nothing more. Without the count both would fail
    -- at their deadline.
    it "takes the waits the test holds as done" $ runPage $ do
      ui <- loader m
      countHeld (pure 1)
      status <- find ui ".status"
      find ui ".load-forever" >>= click
      textContent status `shouldReturn` "loading"
      waitQuiet

    it "says, when a read fails, at which rest it looked and what it found" $ do
      (message, _) <- failing $ do
        ui <- loader m
        find ui ".load" >>= click
        find ui ".status" >>= (`shouldHaveText` "never")
      message `Hspec.shouldSatisfy` T.isPrefixOf "Read once, at rest: quiet after "
      message `Hspec.shouldSatisfy` T.isInfixOf "expected text \"never\", found \"loaded\""

  describe "at idle" $ readsOnce Idle $ do
    -- Idle does not wait for the load's answer, which never comes: the
    -- read sees the program as the click left it, and looks once.
    it "reads what a click rendered, and not what its answer will" $ do
      (message, _) <- failing $ do
        ui <- loader m
        find ui ".load-forever" >>= click
        status <- find ui ".status"
        status `shouldHaveText` "loading"
        status `shouldHaveText` "never loaded"
      message `Hspec.shouldSatisfy` T.isPrefixOf "Read once, at rest: idle after "
      message `Hspec.shouldSatisfy` T.isInfixOf "expected text \"never loaded\", found \"loading\""

    it "rests again after a query" $ runPage $ do
      ui <- mount m bumper ()
      bumps <- find ui ".bumps"
      bumps `shouldHaveText` "0"
      _ <- query ui (H.mkTell Bump)
      bumps `shouldHaveText` "1"

    -- An absence is read at rest too.
    it "finds nothing where nothing is" $ runPage $ do
      ui <- loader m
      length <$> findAll ui ".missing" `shouldReturn` 0

    -- perform is a step: the reads after it rest again.
    it "rests again after perform" $ runPage $ do
      ui <- mount m bumper ()
      bumps <- find ui ".bumps"
      _ <- perform (pure ())
      bumps `shouldHaveText` "0"

  describe "eventually inside it" $ keepingSettings $ readsOnce Idle $ do
    -- The named wait for what the program does not know of: here a value
    -- that changes on the third look. It is still reported.
    it "retries, and is reported" $ do
      reportingRetries
      runPage $ do
        tries <- unsafeIOToPageM (newIORef (0 :: Int))
        eventually (unsafeIOToPageM (atomicModifyIORef' tries (\n -> (n + 1, n))) >>= (`shouldSatisfy` (>= 2)))
      reportedFrom "Test/ReadsOnce.hs:" "HSPEC_HALOGEN_RETRY" >>= \case
        [[_, _, function, _, _, outcome, attempts, _, _, _]] -> do
          function `Hspec.shouldBe` "eventually"
          outcome `Hspec.shouldBe` "ok"
          attempts `Hspec.shouldBe` "3"
        other -> Hspec.expectationFailure ("not one line of the report: " <> show other)

  -- The report's table for reads in a readsOnce group, row by row: what
  -- each writes (RETRY, SETTLE) and what it counts in the totals (COUNTED,
  -- the lines the harness keeps for its own tests).
  describe "in the retry report" $ keepingSettings $ do
    let ours kind = reportedFrom "Test/ReadsOnce.hs:" kind
        nothingOf kinds = for_ kinds $ \kind -> ours kind `Hspec.shouldReturn` []

    readsOnce Idle $ it "a read that passes at its one look writes and counts nothing" $ do
      reportingRetries
      runPage $ do
        ui <- loader m
        find ui ".status" >>= (`shouldHaveText` "idle")
      nothingOf ["HSPEC_HALOGEN_RETRY", "HSPEC_HALOGEN_SETTLE", "HSPEC_HALOGEN_COUNTED"]

    readsOnce Idle $ it "a read that fails at its one look writes and counts nothing" $ do
      reportingRetries
      _ <- failing $ do
        ui <- loader m
        find ui ".status" >>= (`shouldHaveText` "never")
      nothingOf ["HSPEC_HALOGEN_RETRY", "HSPEC_HALOGEN_SETTLE", "HSPEC_HALOGEN_COUNTED"]

    -- The load never comes back; the rest has 300 ms.
    readsOnce Quiet $ it "a read whose rest runs out writes its rest's line, and counts nothing" $ do
      reportingRetries
      (message, _) <- failing $ do
        ui <- loader m
        status <- find ui ".status"
        find ui ".load-forever" >>= click
        withDeadline 300 (status `shouldHaveText` "never loaded")
      message `Hspec.shouldSatisfy` T.isPrefixOf "waitQuiet: the program was not quiet after 300 ms"
      ours "HSPEC_HALOGEN_SETTLE" >>= \case
        [[_, _, what, _, outcome, _, _]] -> (what, outcome) `Hspec.shouldBe` ("waitQuiet", "deadline")
        other -> Hspec.expectationFailure ("not one line of the report: " <> show other)
      nothingOf ["HSPEC_HALOGEN_RETRY", "HSPEC_HALOGEN_COUNTED"]

    -- Its attempts look without resting, so nothing waits for the load,
    -- and no rest is cut.
    readsOnce Quiet $ it "an eventually writes and counts as a read, and the read inside it neither" $ do
      reportingRetries
      _ <- failing $ do
        ui <- loader m
        status <- find ui ".status"
        find ui ".load-forever" >>= click
        eventuallyWithin 300 (status `shouldHaveText` "never loaded")
      ours "HSPEC_HALOGEN_RETRY" >>= \case
        [[_, _, function, _, _, outcome, _, _, _, _]] -> (function, outcome) `Hspec.shouldBe` ("eventuallyWithin", "timeout")
        other -> Hspec.expectationFailure ("not one line of the report: " <> show other)
      ours "HSPEC_HALOGEN_COUNTED" >>= \case
        [[_, function, _, counted]] -> (function, counted) `Hspec.shouldBe` ("eventuallyWithin", "retried")
        other -> Hspec.expectationFailure ("not one count: " <> show other)
      nothingOf ["HSPEC_HALOGEN_SETTLE"]

  -- The count of held waits reads the page itself, as it is: it is asked
  -- while the program is brought to rest, and must not rest in turn. (Before
  -- this, the count rested, which asked the count again, for ever: the
  -- timeout keeps that from hanging the suite.)
  describe "countHeld" $ readsOnce Quiet $
    it "counts with a read of the page, which does not rest" $ do
      done <- timeout 10_000_000 $ runPage $ do
        ui <- loader m
        status <- find ui ".status"
        countHeld (length <$> findAll ui ".held")
        find ui ".load" >>= click
        status `shouldHaveText` "loaded"
      done `Hspec.shouldBe` Just ()

  -- A rest that runs out names the test's line, in the failure and in the
  -- report, whatever function brought it on.
  describe "a rest that runs out" $ keepingSettings $ do
    readsOnce Quiet $ it "names the test's line, from a getter" $ do
      reportingRetries
      (message, at) <- failingAt $ do
        ui <- loader m
        status <- find ui ".status"
        find ui ".load-forever" >>= click
        _ <- withDeadline 300 (textContent status)
        pure ()
      message `Hspec.shouldSatisfy` T.isPrefixOf "waitQuiet: the program was not quiet after 300 ms"
      at `Hspec.shouldSatisfy` maybe False (T.isPrefixOf "test/Test/ReadsOnce.hs:")
      reportedFrom "Test/ReadsOnce.hs:" "HSPEC_HALOGEN_SETTLE" >>= \case
        [[_, _, what, site, outcome, _, _]] -> do
          (what, outcome) `Hspec.shouldBe` ("waitQuiet", "deadline")
          Just site `Hspec.shouldBe` at
        other -> Hspec.expectationFailure ("not one line of the report: " <> show other)

    -- The spinner, already spinning, keeps the program busy until the
    -- mount's settle has run out (its line in the report): the mount
    -- cannot settle, at any speed.
    it "names the test's line, from a mount" $ do
      reportingRetries
      (message, at) <- failingAt $ do
        spinning m
        _ <- withDeadline 300 (mount m bumper ())
        pure ()
      message `Hspec.shouldSatisfy` T.isPrefixOf "settle: the program was still busy after 300 ms"
      at `Hspec.shouldSatisfy` maybe False (T.isPrefixOf "test/Test/ReadsOnce.hs:")
      reportedFrom "Test/ReadsOnce.hs:" "HSPEC_HALOGEN_SETTLE" >>= \case
        [[_, _, what, site, outcome, _, _]] -> do
          (what, outcome) `Hspec.shouldBe` ("settle", "deadline")
          Just site `Hspec.shouldBe` at
        other -> Hspec.expectationFailure ("not one line of the report: " <> show other)

  -- A read looks at a program at rest when it looks, whatever started the
  -- work since the last read: not only the test's steps.
  --
  -- Each load waits on a gate the test opens with a raw call just before
  -- the read, so the load is surely under way when the gate opens, and the
  -- read finds the program at work unless it waits for it to finish.
  -- (The 30 s bounds below are watchdogs: what they wait for comes at once.)
  describe "work the program started by itself" $ readsOnce Quiet $ do
    it "is waited for after an eventually saw a push start it" $ runPage $ do
      gate <- unsafeIOToPageM newEmptyMVar
      ui <- mount m pushing gate
      Just listener <- query ui (H.mkRequest GetListener)
      status <- find ui ".status"
      status `shouldHaveText` "idle"
      -- A message pushed while the eventually looks, as a server's would.
      sent <- unsafeIOToPageM (newIORef False)
      eventuallyWithin 30000 $ do
        unsafeIOToPageM (unlessM (readIORef sent) (writeIORef sent True >> HS.notify listener "pushed"))
        status `shouldHaveText` "pushed: loading"
      unsafeIOToPageM (putMVar gate ())
      status `shouldHaveText` "pushed: loaded"

    -- The pause sleeps on the test's clock, so that it is surely still
    -- asleep at the first read, and ends when the test advances the clock.
    it "is waited for when a pause in delay started it between two reads" $
      withTestClock $ \clock -> runPage $ do
        gate <- unsafeIOToPageM newEmptyMVar
        ui <- mount m pushing gate
        status <- find ui ".status"
        find ui ".later" >>= click
        status `shouldHaveText` "idle"
        -- The pause ends, and its load is under way: no read in between.
        unsafeIOToPageM (advance clock 0.01 >> putMVar gate ())
        status `shouldHaveText` "later: loaded"

    it "is waited for when a raw call of the test's started it" $ runPage $ do
      gate <- unsafeIOToPageM newEmptyMVar
      ui <- mount m pushing gate
      Just listener <- query ui (H.mkRequest GetListener)
      status <- find ui ".status"
      status `shouldHaveText` "idle"
      unsafeIOToPageM (HS.notify listener "raw" >> putMVar gate ())
      status `shouldHaveText` "raw: loaded"

  -- One attempt and no retrying is still an eventually's attempt: it looks
  -- at the page as it is, without waiting for the slow load.
  -- Were it to rest, it would wait for a load that never comes back, and
  -- fail at the deadline.
  describe "eventuallyWithin 0 inside it" $ readsOnce Quiet $
    it "looks at the page as it is" $ runPage $ do
      ui <- loader m
      status <- find ui ".status"
      find ui ".load-forever" >>= click
      eventuallyWithin 0 (textContent status) `shouldReturn` "loading"

  -- The spinner starts a moment after it is mounted and then keeps its tree
  -- busy for a second: mountUnsettled gives the test the page meanwhile.
  it "mountUnsettled returns while the program is busy" $ runPage $ do
    stop <- unsafeIOToPageM (newIORef False)
    ui <- mountUnsettled m (Waiting.spinner :: H.Component H.VoidF (IO Bool) Void m) (not <$> readIORef stop)
    spins <- find ui ".spins"
    eventually (textContent spins >>= (`shouldNotBe` "0"))
    (.running) <$> activity >>= (`shouldSatisfy` (> 0))
    unsafeIOToPageM (writeIORef stop True)
