{-# LANGUAGE TemplateHaskell #-}

-- | The two settings the runner takes from its environment, set here in the
-- page for one test, where the runner sets them: the deadline of the
-- actions, 'settle' and 'waitQuiet' (@HSPEC_HALOGEN_DEADLINE_MS@), and the
-- report of the waits that retried (@HSPEC_HALOGEN_RETRIES@). The report's
-- lines are kept in a list for the test to read, rather than written to
-- standard error.
module Test.Settings (spec) where

import Data.IORef (atomicModifyIORef', newIORef)
import Data.Text qualified as T
import Example.Editing qualified as Editing
import Example.Waiting qualified as Waiting
import Halogen qualified as H
import Halogen.JSBits (JSText, Safety (..), browserJS, fromJSText)
import Halogen.JSBits.Promise (JSError (..))
import Protolude hiding (find)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec (around_, shouldThrow)
import Test.Hspec qualified as Hspec
import Test.Hspec.Halogen
import Test.InPage (withoutRunner)

$( browserJS
     ["test/jsbits/settings.js"]
     [ ("js_save", "halogen_settings_save", Safe, [t|IO ()|])
     , ("js_restore", "halogen_settings_restore", Safe, [t|IO ()|])
     , ("js_deadline", "halogen_settings_deadline", Safe, [t|Int -> IO ()|])
     , ("js_retries", "halogen_settings_retries", Safe, [t|Bool -> IO ()|])
     , ("js_report", "halogen_settings_report", Safe, [t|IO JSText|])
     , ("js_to_runner", "halogen_settings_to_runner", Safe, [t|IO ()|])
     , ("js_sent", "halogen_settings_sent", Safe, [t|IO JSText|])
     ]
 )

-- | The message of a page that fails on purpose.
failure :: (forall s. PageM s ()) -> IO Text
failure test =
  try (runPage test) >>= \case
    Left (HUnitFailure _ (Reason message)) -> pure (toS message)
    Left other -> panic ("another failure: " <> show other)
    Right () -> panic "the test passed"

-- | A deadline far below the five seconds the other tests have.
deadline :: Int
deadline = 400

-- | The report's lines of one kind about the waits of this module, each cut
-- into its fields. The harness's own waits (a 'find' in 'mount') report
-- too.
reported :: Text -> IO [[Text]]
reported kind = filter (\fields -> take 1 fields == [kind]) . map (T.splitOn "\t") . filter (T.isInfixOf "Test/Settings.hs:") . T.lines . fromJSText <$> js_report

-- | A read that fails twice and then succeeds.
thirdTime :: PageM s ()
thirdTime = do
  tries <- unsafeIOToPageM (newIORef (0 :: Int))
  eventually (unsafeIOToPageM (atomicModifyIORef' tries (\n -> (n + 1, n))) >>= (`shouldSatisfy` (>= 2)))

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = around_ (bracket_ js_save js_restore) $ do
  describe "the deadline (HSPEC_HALOGEN_DEADLINE_MS)" $ do
    it "is settle's" $ do
      message <- failure $ do
        ui <- mount m (Waiting.spinner :: H.Component H.VoidF Double Void m) 1.5
        spins <- find ui ".spins"
        eventually (textContent spins >>= (`shouldNotBe` "0"))
        unsafeIOToPageM (js_deadline deadline)
        settle
      message `Hspec.shouldSatisfy` T.isPrefixOf ("settle: the program was still busy after " <> show deadline <> " ms")

    it "is waitQuiet's" $ do
      message <- failure $ do
        ui <- mount m (Waiting.component :: H.Component H.VoidF () Void m) ()
        find ui ".load-slowly" >>= click
        unsafeIOToPageM (js_deadline deadline)
        waitQuiet
      message `Hspec.shouldSatisfy` T.isPrefixOf ("waitQuiet: the program was not quiet after " <> show deadline <> " ms")

    -- Playwright's: the page names its deadline with each action.
    it "is an action's" $ hiddenClick m

    -- The page's own, with synthetic events.
    withoutRunner $ it "is an action's without the runner" $ hiddenClick m

  describe "the retry report (HSPEC_HALOGEN_RETRIES)" $ do
    it "says that a wait retried and then succeeded" $ do
      js_retries True
      runPage thirdTime
      reported "HSPEC_HALOGEN_RETRY" >>= \case
        [["HSPEC_HALOGEN_RETRY", example, function, site, _reading, outcome, attempts, _ms, firstFailure]] -> do
          example `Hspec.shouldSatisfy` T.isSuffixOf "/says that a wait retried and then succeeded"
          function `Hspec.shouldBe` "eventually"
          site `Hspec.shouldSatisfy` T.isPrefixOf "test/Test/Settings.hs:"
          outcome `Hspec.shouldBe` "ok"
          attempts `Hspec.shouldBe` "3"
          firstFailure `Hspec.shouldSatisfy` (not . T.null)
        other -> Hspec.expectationFailure ("not one line of the report: " <> show other)

    it "says that a wait ran out" $ do
      js_retries True
      _ <- failure (eventuallyWithin 200 (expectationFailure "never"))
      reported "HSPEC_HALOGEN_RETRY" >>= \case
        [["HSPEC_HALOGEN_RETRY", _example, function, _site, _reading, outcome, attempts, _ms, _firstFailure]] -> do
          function `Hspec.shouldBe` "eventuallyWithin"
          outcome `Hspec.shouldBe` "timeout"
          attempts `Hspec.shouldSatisfy` maybe False (> (1 :: Int)) . readMaybe
        other -> Hspec.expectationFailure ("not one line of the report: " <> show other)

    it "says that a settle ran out" $ do
      js_retries True
      _ <- failure $ do
        ui <- mount m (Waiting.component :: H.Component H.VoidF () Void m) ()
        find ui ".load-slowly" >>= click
        waitQuietWithin 200
      reported "HSPEC_HALOGEN_SETTLE" >>= \case
        [["HSPEC_HALOGEN_SETTLE", _example, what, _site, outcome, _ms, _reading]] -> do
          what `Hspec.shouldBe` "waitQuiet"
          outcome `Hspec.shouldBe` "deadline"
        other -> Hspec.expectationFailure ("not one line of the report: " <> show other)

    -- The runner writes them to its standard error: the suite's own is
    -- the console on the JavaScript backend, which the runner prints as
    -- standard output (its own test checks where the lines end up).
    it "hands its lines to the runner" $ do
      js_retries True
      js_to_runner
      runPage thirdTime
      sent <- filter (T.isInfixOf "Test/Settings.hs:") . T.lines . fromJSText <$> js_sent
      map (take 1 . T.splitOn "\t") sent `Hspec.shouldBe` [["HSPEC_HALOGEN_RETRY"]]

    it "says nothing of a wait that succeeded at once" $ do
      js_retries True
      runPage (eventually (True `shouldBe` True))
      reported "HSPEC_HALOGEN_RETRY" `Hspec.shouldReturn` []

    -- What is checked absent is shown present above, with the same read.
    it "writes nothing without the setting" $ do
      js_retries False
      runPage thirdTime
      _ <- failure (eventuallyWithin 200 (expectationFailure "never"))
      (fromJSText <$> js_report) `Hspec.shouldReturn` ""

-- | A click on an element that never becomes visible, failing at the
-- deadline with Playwright's words for it.
hiddenClick :: forall m -> (MonadBrowserTest m) => IO ()
hiddenClick m =
  runPage
    ( do
        button <- mount m Editing.component () >>= (`find` "button.invisible")
        unsafeIOToPageM (js_deadline deadline)
        click button
    )
    `shouldThrow` \(JSError _ message) -> ("Timeout " <> show deadline <> "ms exceeded") `T.isInfixOf` message
