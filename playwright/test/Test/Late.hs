-- | Events the browser dispatches later than the action that causes them
-- (test/web/late.html): a dialog's @close@, with the next animation frame,
-- and a @popstate@, once the browser has gone through the history. A
-- program that handles them is idle at the moment the action returns, so
-- only a settle that waits for the event sees what it did. Each case is
-- read once after its action, never retried, in a loop of runs, and every
-- run that read the old state is counted: the count is in the failure.
-- And 'waitForEvent', for what the page does later by itself.
module Test.Late (spec) where

import Control.Exception (try)
import Control.Monad (foldM, replicateM)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTime)
import Halogen.Playwright
import Halogen.Playwright.Hspec

-- | Seconds an action takes.
timed :: IO () -> IO Double
timed action = do
  started <- getMonotonicTime
  action
  subtract started <$> getMonotonicTime

-- | Runs of a case.
runs :: Int
runs = 40

-- | Run a case's runs, each a number from 1 and whether it read what it
-- should have, and fail with how many did not.
noneMissed :: (Int -> IO Bool) -> Expectation
noneMissed = noneMissedIn runs

-- | 'noneMissed' in this many runs.
noneMissedIn :: Int -> (Int -> IO Bool) -> Expectation
noneMissedIn n run = do
  missed <- foldM (\m i -> (\ok -> if ok then m else m + 1) <$> run i) (0 :: Int) [1 .. n]
  if missed == 0 then pure () else expectationFailure ("read the old state in " <> show missed <> " of " <> show n <> " runs")

shown :: Int -> Text
shown = Text.pack . show

spec :: Text -> SpecWith Playwright
spec base = describe "events the browser dispatches late" $ do
  let at path = base <> path

  it "settles Escape on a modal dialog after its close" $ \pw -> withPage pw $ \page -> do
    visit page (at "late.html")
    noneMissed $ \i -> do
      click (locator page "#open")
      press page "Escape"
      (== shown i) <$> textContent (locator page "#closes")

  it "settles a click whose handler closes a dialog after its close" $ \pw -> withPage pw $ \page -> do
    visit page (at "late.html")
    noneMissed $ \i -> do
      click (locator page "#open")
      click (locator page "#dismiss")
      (== shown i) <$> textContent (locator page "#closes")

  it "settles a click whose handler goes back after its popstate" $ \pw -> withPage pw $ \page -> do
    visit page (at "late.html")
    noneMissed $ \_ -> do
      click (locator page "#push")
      click (locator page "#back")
      (== "0") <$> textContent (locator page "#state")

  -- Forward from the last entry goes nowhere, and no popstate comes: the
  -- settle does not wait for one (half a second, were it counted).
  -- A program gives the probe its answer when it starts, which may be in
  -- the very task a settle begins with.
  it "settles a program that starts in the task the settle begins with" $ \pw -> withPage pw $ \page -> do
    goto page (at "starting.html")
    click (locator page "#start")
    textContent (locator page "#status") `shouldReturn` "started"

  -- A frame has a document, a probe and a program of its own: an action on
  -- an element in it settles that program, not only the page's.
  it "settles the frame an action's element is in" $ \pw -> withPage pw $ \page -> do
    goto page (at "framing.html")
    let inner = inFrame page "iframe#inner"
    click (locator inner "#slow")
    textContent (locator inner "#state") `shouldReturn` "done"

  it "does not wait for a move through the history that has nowhere to go" $ \pw -> withPage pw $ \page -> do
    visit page (at "late.html")
    click (locator page "#push")
    took <- timed (click (locator page "#forward"))
    took `shouldSatisfy` (< 0.4)
    url page `shouldReturn` at "late.html?state=1"

  -- --enable-begin-frame-control: Chromium draws a frame only when asked,
  -- which nothing here does, so the page's requestAnimationFrame never
  -- fires. Playwright's click waits for frames (an element is stable over
  -- two), so the actions here are the keyboard's.
  it "settles a page that draws no frames, after at most one bounded wait, and after a Back" $ \_ -> do
    options <- optionsFromEnvironment
    withPlaywright options {browserArgs = ["--enable-begin-frame-control", "--run-all-compositor-stages-before-draw"]} $ \pw -> withPage pw $ \page -> do
      visit page (at "late.html")
      evaluate page "new Promise((resolve) => { requestAnimationFrame(() => resolve('frame')); setTimeout(() => resolve('none'), 300); })"
        `shouldReturn` ("none" :: Text)
      first' <- timed (press page "a")
      rest <- replicateM 5 (timed (press page "a"))
      first' `shouldSatisfy` (< 1)
      sum rest `shouldSatisfy` (< 0.4)
      -- With no frame to wait for, a Back the program asked for is still
      -- waited for: the driver saw the call. (Enter on a focused button
      -- clicks it.) Each action waits there for a background task, which
      -- Chromium holds for about 50 ms when no frame comes, so fewer runs:
      -- without the driver noting the Back, 39 of 40 read the old state.
      noneMissedIn 20 $ \_ -> do
        focus (locator page "#push")
        press page "Enter"
        focus (locator page "#back")
        press page "Enter"
        (== "0") <$> textContent (locator page "#state")

  -- A page that drew no frame for a while (here its requestAnimationFrame
  -- is taken away while a key is pressed, so that a settle finds none) and
  -- draws them again: the first action after that still waits for its
  -- frame. Were a page that missed a frame not waited for at all until one
  -- came, this click would be read before the close it causes.
  it "waits for the frame again as soon as a page that drew none draws them" $ \pw -> withPage pw $ \page -> do
    visit page (at "late.html")
    noneMissedIn 20 $ \i -> do
      click (locator page "#open")
      () <- evaluate page "(() => { window.keptFrames = window.requestAnimationFrame; window.requestAnimationFrame = () => 0; })()"
      press page "a"
      () <- evaluate page "(() => { window.requestAnimationFrame = window.keptFrames; })()"
      click (locator page "#dismiss")
      (== shown i) <$> textContent (locator page "#closes")

  describe "waitForEvent" $ do
    it "waits for an element's event that the page's own timer dispatches" $ \pw -> withPage pw $ \page -> do
      visit page (at "late.html")
      waitForEvent (OnElement (locator page "#later")) "later" (click (locator page "#later"))
      textContent (locator page "#later-state") `shouldReturn` "later"

    it "waits for the document's event" $ \pw -> withPage pw $ \page -> do
      visit page (at "late.html")
      waitForEvent (OnDocument page) "later" (click (locator page "#later"))
      textContent (locator page "#later-state") `shouldReturn` "later"

    -- The page goes back by a timer of its own, when nothing runs: only the
    -- popstate tells; its handler is the program's, settled after it.
    it "waits for the window's popstate after a Back the page makes later" $ \pw -> withPage pw $ \page -> do
      visit page (at "late.html")
      click (locator page "#push")
      textContent (locator page "#state") `shouldReturn` "1"
      waitForEvent (OnWindow page) "popstate" (click (locator page "#back-later"))
      textContent (locator page "#state") `shouldReturn` "0"
      url page `shouldReturn` at "late.html"

    it "settles the frame of the element it listened on" $ \pw -> withPage pw $ \page -> do
      goto page (at "framing.html")
      let slow = locator (inFrame page "iframe#inner") "#slow"
      -- A click from script, which no action settles: the wait does.
      waitForEvent (OnElement slow) "click" (evaluateOn slow "(e) => { e.click(); return null; }" () :: IO ())
      textContent (locator (inFrame page "iframe#inner") "#state") `shouldReturn` "done"

    it "gives the action's result" $ \pw -> withPage pw $ \page -> do
      visit page (at "late.html")
      waitForEvent (OnElement (locator page "#later")) "later" (click (locator page "#later") >> pure (42 :: Int)) `shouldReturn` 42

    it "fails after the page's timeout, naming the event" $ \pw -> withPage pw $ \page -> do
      visit page (at "late.html")
      try (waitForEvent (OnWindow (withTimeout 300 page)) "nothing-comes" (click (locator page "#push"))) >>= \case
        Left (e :: PlaywrightError) -> do
          e.name `shouldBe` "TimeoutError"
          Text.unpack e.message `shouldContain` "nothing-comes"
        Right () -> expectationFailure "an event came"

    it "fails when the page navigates before the event" $ \pw -> withPage pw $ \page -> do
      visit page (at "late.html")
      try (waitForEvent (OnElement (locator page "#later")) "later" (click (locator page "#away"))) >>= \case
        Left (e :: PlaywrightError) -> e.name `shouldBe` "NavigatedAway"
        Right () -> expectationFailure "the event came"
