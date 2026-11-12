-- | The waits that ask the page's program, against a page whose plain
-- JavaScript plays the program's side of the probe under the test's
-- control (test/web/program.html): what it reports is in @window.fake@.
module Test.Probe (spec) where

import Control.Exception (try)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTime)
import Halogen.Playwright
import Test.Hspec

set :: Page -> Text -> IO ()
set page js = do
  _ :: Maybe Value <- evaluate page ("(" <> js <> ", null)")
  pure ()

spec :: Text -> SpecWith Playwright
spec base = describe "the waits" $ do
  let at path = base <> path

  it "settles an action only once the program is idle" $ \pw -> withPage pw $ \page -> do
    visit page (at "program.html")
    started <- getMonotonicTime
    click (locator page "#slow")
    took <- subtract started <$> getMonotonicTime
    -- No eventually: the program was busy for 300ms after the click, and
    -- the click returned once it was not.
    textContent (locator page "#state") `shouldReturn` "done"
    took `shouldSatisfy` (>= 0.29)
    activity page `shouldReturn` Just (Activity 1 0 0)

  it "settles at once where there is no program" $ \pw -> withPage pw $ \page -> do
    goto page (at "plain.html")
    click (locator page "#go")
    textContent (locator page "#go") `shouldReturn` "went"
    activity page `shouldReturn` Nothing
    settle page
    waitQuiet page

  it "waits for the waiting fibers with waitQuiet, and its timeout says how many" $ \pw -> withPage pw $ \page -> do
    visit page (at "program.html")
    click (locator page "#wait")
    -- Idle, so settled, though two fibers wait.
    activity page `shouldReturn` Just (Activity 1 0 2)
    try (waitQuiet page) >>= \case
      Left (e :: PlaywrightError) -> do
        e.name `shouldBe` "TimeoutError"
        Text.unpack e.message `shouldContain` "2 fiber(s) still waiting"
      Right () -> expectationFailure "waitQuiet returned while fibers waited"
    set page "setTimeout(() => { fake.waiting = 0; }, 200)"
    waitQuiet page
    activity page `shouldReturn` Just (Activity 1 0 0)

  it "waits for a program to start, and fails cleanly where none answers" $ \pw -> withPage pw $ \page -> do
    goto page (at "program.html")
    set page "fake.running = 1, setTimeout(() => { fake.running = 0; }, 200)"
    waitStarted page
    activity page `shouldReturn` Just (Activity 1 0 0)
    goto page (at "program.html?unstarted")
    try (waitStarted page) >>= \case
      Left (e :: PlaywrightError) -> do
        e.method `shouldBe` "waitStarted"
        Text.unpack e.message `shouldContain` "no Halogen program"
      Right () -> expectationFailure "waitStarted returned"
    goto page (at "plain.html")
    try (visit page (at "plain.html")) >>= \case
      Left (e :: PlaywrightError) -> Text.unpack e.message `shouldContain` "no Halogen program"
      Right () -> expectationFailure "visit returned"

  it "fails, and does not hang, when the program stops answering" $ \pw -> withPage pw $ \page -> do
    visit page (at "program.html")
    -- The click lands; the settle after it asks a program that never
    -- answers again, and the driver gives up for it.
    try (click (locator page "#silent")) >>= \case
      Left (e :: PlaywrightError) -> do
        e.name `shouldBe` "TimeoutError"
        Text.unpack e.message `shouldContain` "did not answer"
      Right () -> expectationFailure "the click settled though the program was silent"
    try (activity page) >>= \case
      Left (e :: PlaywrightError) -> Text.unpack e.message `shouldContain` "did not answer"
      Right a -> expectationFailure ("activity answered " <> show a)
    try (waitStarted page) >>= \case
      Left (e :: PlaywrightError) -> Text.unpack e.message `shouldContain` "did not answer"
      Right () -> expectationFailure "waitStarted returned"

  it "settles a click that navigates away" $ \pw -> withPage pw $ \page -> do
    visit page (at "program.html")
    -- The program says it is running, and the document goes before it
    -- says anything else.
    click (locator page "#away")
    shouldHaveURL page (at "second.html")
    locator page "h1" `shouldHaveText` "Second"
