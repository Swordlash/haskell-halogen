-- | 'readsOnce': the reads of a group look once, after the program has
-- come to the group's rest, and say so when they fail; the waits a test
-- holds on purpose ('countHeld'); the steps that make the reads rest again
-- ('query', 'perform'); 'eventually' kept as the named wait, and reported;
-- and 'mountUnsettled', for a program busy from its start.
module Test.ReadsOnce (spec) where

import Data.IORef (atomicModifyIORef', newIORef)
import Data.Row (Empty)
import Data.Text qualified as T
import Example.Waiting qualified as Waiting
import GHC.Clock (getMonotonicTime)
import Halogen qualified as H
import Halogen.Activity (Activity (..))
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Protolude hiding (find)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec qualified as Hspec
import Test.Hspec.Halogen
import Test.Settings (keepingSettings, reportedFrom, reportingRetries)

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

    -- The slow load is held on purpose: the read waits for nothing more.
    it "takes the waits the test holds as done" $ runPage $ do
      ui <- loader m
      countHeld (pure 1)
      status <- find ui ".status"
      started <- unsafeIOToPageM getMonotonicTime
      find ui ".load-slowly" >>= click
      textContent status `shouldReturn` "loading"
      waitQuiet
      took <- (subtract started) <$> unsafeIOToPageM getMonotonicTime
      took `shouldSatisfy` (< 1.5)

    it "says, when a read fails, at which rest it looked and what it found" $ do
      (message, took) <- failing $ do
        ui <- loader m
        find ui ".load" >>= click
        find ui ".status" >>= (`shouldHaveText` "never")
      message `Hspec.shouldSatisfy` T.isPrefixOf "Read once, at rest: quiet after "
      message `Hspec.shouldSatisfy` T.isInfixOf "expected text \"never\", found \"loaded\""
      -- One look, not two seconds of them.
      took `Hspec.shouldSatisfy` (< 1.5)

  describe "at idle" $ readsOnce Idle $ do
    -- Idle does not wait for the load's answer: the read sees the program
    -- as the click left it, and looks once.
    it "reads what a click rendered, and not what its answer will" $ do
      (message, took) <- failing $ do
        ui <- loader m
        find ui ".load" >>= click
        status <- find ui ".status"
        status `shouldHaveText` "loading"
        status `shouldHaveText` "loaded"
      message `Hspec.shouldSatisfy` T.isPrefixOf "Read once, at rest: idle after "
      message `Hspec.shouldSatisfy` T.isInfixOf "expected text \"loaded\", found \"loading\""
      took `Hspec.shouldSatisfy` (< 1.5)

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

  -- The spinner starts a moment after it is mounted and then keeps its tree
  -- busy for a second: mountUnsettled gives the test the page meanwhile.
  it "mountUnsettled returns while the program is busy" $ runPage $ do
    ui <- mountUnsettled m (Waiting.spinner :: H.Component H.VoidF Double Void m) 1
    spins <- find ui ".spins"
    eventually (textContent spins >>= (`shouldNotBe` "0"))
    (.running) <$> activity >>= (`shouldSatisfy` (> 0))
