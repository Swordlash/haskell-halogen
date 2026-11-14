{-# LANGUAGE TemplateHaskell #-}

-- | Events the browser fires later than the action that causes them
-- ("Example.Late"): a dialog's @close@, with the next animation frame, and a
-- @popstate@, once the browser has gone through the history. The program
-- is idle when the action's handler is over, so only a 'settle' that waits
-- for the event as well sees what it did. Each case is read once after its
-- action, never retried, in a loop of runs, and every run that read the old
-- state is counted: the count is in the failure.
module Test.Late
  ( spec
  , inPageSpec
  )
where

import Control.Monad.UUID (MonadUUID)
import Data.Text qualified as T
import Example.Late qualified as Late
import GHC.Clock (getMonotonicTime)
import Halogen qualified as H
import Halogen.JSBits (JSVal, Safety (..), browserJS)
import Halogen.JSBits.Promise (Promise)
import Halogen.JSBits.Promise qualified as Promise
import Halogen.VDom.DOM.Monad (MonadBrowserDOM)
import Protolude hiding (find)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec (after_)
import Test.Hspec qualified as Hspec
import Test.Hspec.Halogen

$( browserJS
     ["test/jsbits/frames.js"]
     [ ("js_stop_frames", "halogen_frames_stop", Safe, [t|IO ()|])
     , ("js_restore_frames", "halogen_frames_restore", Safe, [t|IO ()|])
     , ("js_hide", "halogen_frames_hide", Safe, [t|IO ()|])
     , ("js_unhide", "halogen_frames_unhide", Safe, [t|IO ()|])
     , ("js_wrap_back", "halogen_frames_wrap_back", Safe, [t|IO ()|])
     , ("js_unwrap_back", "halogen_frames_unwrap_back", Safe, [t|IO ()|])
     , ("js_back_calls", "halogen_frames_back_calls", Safe, [t|IO Int|])
     , ("js_restless_start", "halogen_frames_restless_start", Safe, [t|IO ()|])
     , ("js_restless_stop", "halogen_frames_restless_stop", Safe, [t|IO (Promise JSVal)|])
     ]
 )

-- | Runs of a case: 40 without the runner, where a settle that waits only
-- for a task misses the late event in most runs (38 of 40 closes, about 20
-- of 40 Backs); 20 under it, where Playwright's own waits around an action
-- happen to cover the event even then, so that these guard against a
-- regression at half the cost (each action is a round trip to Playwright).
data Mode = Runner | InPage

runsIn :: Mode -> Int
runsIn = \case
  Runner -> 20
  InPage -> 40

-- | Run a case's runs, each given its number from 1 and saying whether it
-- read what it should have, and fail with how many did not.
noneMissed :: (HasCallStack) => Int -> (Int -> PageM s Bool) -> PageM s ()
noneMissed runs run = do
  missed <- foldM (\m i -> (\ok -> if ok then m else m + 1) <$> run i) (0 :: Int) [1 .. runs]
  when (missed > 0) $ expectationFailure ("read the old state in " <> show missed <> " of " <> show runs <> " runs")

-- | Seconds an action takes.
timed :: PageM s a -> PageM s Double
timed action = do
  started <- unsafeIOToPageM getMonotonicTime
  void action
  subtract started <$> unsafeIOToPageM getMonotonicTime

late :: forall m -> (MonadBrowserDOM m, MonadBrowserTest m, MonadUUID m) => PageM s (Mounted s H.VoidF Void m)
late m = mount m (Late.component :: H.Component H.VoidF () Void m) ()

-- | What holds under the runner and without it.
cases :: forall m -> (MonadBrowserDOM m, MonadBrowserTest m, MonadUUID m) => Mode -> Spec
cases m mode = do
  it "settles a click whose handler closes a dialog after its close" $ runPage $ do
    ui <- late m
    [open, dismiss, closes] <- traverse (find ui) [".open", ".dismiss", ".closes"]
    noneMissed (runsIn mode) $ \i -> do
      click open
      click dismiss
      (== show i) <$> textContent closes

  it "settles a click whose handler goes back after its popstate" $ runPage $ do
    ui <- late m
    [push, back, entry] <- traverse (find ui) [".push", ".back", ".entry"]
    start <- textContent entry
    noneMissed (runsIn mode) $ \_ -> do
      click push
      click back
      (== start) <$> textContent entry

  -- Two moves asked for at once: each is waited for, and each has its
  -- popstate.
  it "settles a click whose handler goes back twice after both popstates" $ runPage $ do
    ui <- late m
    [push, backTwice, entry] <- traverse (find ui) [".push", ".back-twice", ".entry"]
    start <- textContent entry
    noneMissed (runsIn mode) $ \_ -> do
      click push
      click push
      click backTwice
      (== start) <$> textContent entry

spec :: forall m -> (MonadBrowserDOM m, MonadBrowserTest m, MonadUUID m) => Spec
spec m = do
  cases m Runner

  -- Escape closes a modal dialog only when it is a user's (trusted), so
  -- this one needs the runner.
  it "settles Escape on a modal dialog after its close" $ runPage $ do
    ui <- late m
    [open, closes] <- traverse (find ui) [".open", ".closes"]
    noneMissed (runsIn Runner) $ \i -> do
      click open
      press "Escape"
      (== show i) <$> textContent closes

  -- A page driven by haskell-halogen-playwright too has the driver's wrap
  -- of History's methods as well as the harness's: each calls the one it
  -- replaced, so both see every move. (The driver's comes first, from an
  -- init script; here the other one is put over the harness's.)
  after_ js_unwrap_back $
    it "keeps waiting for a Back under another harness's wrap of History, which still sees it" $
      runPage $ do
        unsafeIOToPageM js_wrap_back
        ui <- late m
        [push, back, entry] <- traverse (find ui) [".push", ".back", ".entry"]
        start <- textContent entry
        replicateM_ 10 $ do
          click push
          click back
          textContent entry `shouldReturn` start
        unsafeIOToPageM js_back_calls `shouldReturn` 10

  -- A page whose popstate listener asks for another move each time always
  -- has a move under way: the wait for the moves is part of the deadline,
  -- so the test fails then, saying so, rather than waiting for ever.
  after_ (void (Promise.wait =<< js_restless_stop)) $
    it "fails at its deadline, and does not hang, when the page never stops moving through its history" $ do
      started <- getMonotonicTime
      outcome <- try $ runPage $ do
        unsafeIOToPageM js_restless_start
        waitQuietWithin 400
      took <- subtract started <$> getMonotonicTime
      case outcome of
        Left (HUnitFailure _ (Reason message)) -> do
          take 1 (T.lines (toS message))
            `Hspec.shouldBe` ["waitQuiet: the program was not quiet after 400 ms: 0 of 0 trees running or with work queued, 0 fibers waiting in liftIO, and a move through the history it asked for had not arrived."]
          took `Hspec.shouldSatisfy` (< 2)
        Left other -> Hspec.expectationFailure ("another failure: " <> show other)
        Right () -> Hspec.expectationFailure "the wait ended"

  -- Forward from the last entry goes nowhere, and no popstate comes: it is
  -- not waited for (half a second, were it counted).
  it "does not wait for a move through the history that has nowhere to go" $ runPage $ do
    ui <- late m
    [push, forward, entry] <- traverse (find ui) [".push", ".forward", ".entry"]
    click push
    pushed <- textContent entry
    took <- timed (click forward)
    took `shouldSatisfy` (< 0.4)
    textContent entry `shouldReturn` pushed
    find ui ".back" >>= click

  it "settles with nothing mounted, and after what was mounted is gone" $ runPage $ do
    timed settle >>= (`shouldSatisfy` (< 0.4))
    ui <- late m
    find ui ".push" >>= click
    unmount ui
    timed settle >>= (`shouldSatisfy` (< 0.4))

  -- A hidden page draws no frames, and is not waited for; a visible page
  -- that draws none (requestAnimationFrame taken away here, as Chromium
  -- under --enable-begin-frame-control would) is waited for once, a bounded
  -- while (100 ms), and then only briefly (20 ms) until it draws one. Were
  -- each settle to wait out the bound, five would take half a second.
  after_ (js_restore_frames >> js_unhide) $
    it "does not wait for frames a page does not draw, beyond one bounded wait" $
      runPage $ do
        ui <- late m
        [open, dismiss, closes] <- traverse (find ui) [".open", ".dismiss", ".closes"]
        unsafeIOToPageM js_hide
        hidden <- replicateM 5 (timed settle)
        sum hidden `shouldSatisfy` (< 0.4)
        unsafeIOToPageM js_unhide
        unsafeIOToPageM js_stop_frames
        once <- timed settle
        rest <- replicateM 5 (timed settle)
        once `shouldSatisfy` (< 1)
        sum rest `shouldSatisfy` (< 0.4)
        -- With no frame to wait for, a dialog's close still comes (Chromium
        -- dispatches it with the frame it does not draw, a moment later), and
        -- settle still waits for the program: the count is read with an
        -- 'eventually', since no frame tells when it has come.
        click open
        click dismiss
        eventually (textContent closes `shouldReturn` "1")
        unsafeIOToPageM js_restore_frames

-- | The cases without the runner, with synthetic events.
inPageSpec :: forall m -> (MonadBrowserDOM m, MonadBrowserTest m, MonadUUID m) => Spec
inPageSpec m = cases m InPage
