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
import Example.Late qualified as Late
import GHC.Clock (getMonotonicTime)
import Halogen qualified as H
import Halogen.JSBits (Safety (..), browserJS)
import Halogen.VDom.DOM.Monad (MonadBrowserDOM)
import Protolude hiding (find)
import Test.Hspec (after_)
import Test.Hspec.Halogen

$( browserJS
     ["test/jsbits/frames.js"]
     [ ("js_stop_frames", "halogen_frames_stop", Safe, [t|IO ()|])
     , ("js_restore_frames", "halogen_frames_restore", Safe, [t|IO ()|])
     , ("js_hide", "halogen_frames_hide", Safe, [t|IO ()|])
     , ("js_unhide", "halogen_frames_unhide", Safe, [t|IO ()|])
     ]
 )

-- | Runs of a case.
runs :: Int
runs = 40

-- | Run a case's runs, each given its number from 1 and saying whether it
-- read what it should have, and fail with how many did not.
noneMissed :: (HasCallStack) => (Int -> PageM s Bool) -> PageM s ()
noneMissed run = do
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
cases :: forall m -> (MonadBrowserDOM m, MonadBrowserTest m, MonadUUID m) => Spec
cases m = do
  it "settles a click whose handler closes a dialog after its close" $ runPage $ do
    ui <- late m
    [open, dismiss, closes] <- traverse (find ui) [".open", ".dismiss", ".closes"]
    noneMissed $ \i -> do
      click open
      click dismiss
      (== show i) <$> textContent closes

  it "settles a click whose handler goes back after its popstate" $ runPage $ do
    ui <- late m
    [push, back, entry] <- traverse (find ui) [".push", ".back", ".entry"]
    start <- textContent entry
    noneMissed $ \_ -> do
      click push
      click back
      (== start) <$> textContent entry

  -- Two moves asked for at once: each is waited for, and each has its
  -- popstate.
  it "settles a click whose handler goes back twice after both popstates" $ runPage $ do
    ui <- late m
    [push, backTwice, entry] <- traverse (find ui) [".push", ".back-twice", ".entry"]
    start <- textContent entry
    noneMissed $ \_ -> do
      click push
      click push
      click backTwice
      (== start) <$> textContent entry

spec :: forall m -> (MonadBrowserDOM m, MonadBrowserTest m, MonadUUID m) => Spec
spec m = do
  cases m

  -- Escape closes a modal dialog only when it is a user's (trusted), so
  -- this one needs the runner.
  it "settles Escape on a modal dialog after its close" $ runPage $ do
    ui <- late m
    [open, closes] <- traverse (find ui) [".open", ".closes"]
    noneMissed $ \i -> do
      click open
      press "Escape"
      (== show i) <$> textContent closes

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
inPageSpec m = cases m
