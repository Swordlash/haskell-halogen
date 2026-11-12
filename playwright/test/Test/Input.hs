-- | The keyboard and the mouse where no locator is, where things are drawn,
-- and the waits for what is not the program's.
module Test.Input (spec) where

import Control.Exception (try)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTime)
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))

-- | That the call fails with Playwright's timeout, soon after the 300 ms
-- the page handle gives it.
timesOut :: (Show a) => IO a -> IO PlaywrightError
timesOut action = do
  started <- getMonotonicTime
  outcome <- try action
  took <- subtract started <$> getMonotonicTime
  took `shouldSatisfy` (< 1.5)
  case outcome of
    Left (e :: PlaywrightError) -> do
      e.name `shouldBe` "TimeoutError"
      pure e
    Right a -> expectationFailure ("it returned " <> show a) >> fail "returned"

spec :: Text -> SpecWith Playwright
spec base = describe "input, geometry and waits" $ do
  let at path = base <> path

  -- smoke-test.mjs: search from the keyboard ("/", then the words, then
  -- Enter); test-learn.mjs: an editor's text replaced as if pasted.
  it "types where the focus is, and pastes" $ \pw -> withPage pw $ \page -> do
    goto page (at "input.html")
    press page "/"
    typeKeys page "handbook"
    locator page "#search" `shouldHaveValue` "handbook"
    focus (locator page "#editor")
    insertText page "main = pure ()"
    locator page "#editor" `shouldHaveValue` "main = pure ()"
    -- One input event, as a paste makes (a line break in the text would be
    -- a second).
    textContent (locator page "#inputs") `shouldReturn` "1"

  -- The playground's test: a canvas, which no locator sees into, clicked at
  -- its middle from its box; smoke-test.mjs: a dialog's box within the
  -- phone's screen; test-learn.mjs: the pointer moved off what it showed.
  it "clicks at a point from an element's box, and moves the pointer away" $ \pw -> withPage pw $ \page -> do
    goto page (at "input.html")
    Just box <- boundingBox (locator page "#canvas")
    box `shouldBe` Box {x = 100, y = 50, width = 200, height = 100}
    mouseClick page (box.x + box.width / 2 + 40) (box.y + box.height / 2 + 20)
    textContent (locator page "#clicked") `shouldReturn` "140,70"
    boundingBox (locator page "#outline") `shouldReturn` Nothing
    -- What :hover shows is the browser's to update, not a program's: the
    -- retrying expectations.
    hover (locator page "#hovered")
    shouldBeVisible (locator page "#outline")
    mouseMove page 5 5
    shouldBeHidden (locator page "#outline")

  -- test-learn.mjs: after a click that a router follows without loading a
  -- page; smoke-test.mjs: a slide's step in the address.
  it "waits for the address, exactly or by a glob" $ \pw -> withPage pw $ \page -> do
    goto page (at "input.html")
    click (locator page "#push")
    waitForURL page (at "pushed/?slide=7&step=2")
    click (locator page "#push")
    waitForURL page "**/pushed/**"
    e <- timesOut (waitForURL (withTimeout 300 page) (at "elsewhere/"))
    Text.unpack e.message `shouldContain` "elsewhere"

  it "waits for a function of an argument to hold in the page" $ \pw -> withPage pw $ \page -> do
    goto page (at "input.html")
    click (locator page "#start")
    waitForFunctionWith page "(expected) => document.getElementById('status').textContent === expected" ("done" :: Text)
    _ <- timesOut (waitForFunctionWith (withTimeout 300 page) "(n) => document.querySelectorAll('p').length > n" (100 :: Int))
    pure ()

  -- test-learn.mjs: the compiler's "ready" line in the console, which the
  -- page writes when it is, waited for after the action that starts it.
  it "waits for a console message, which may already be there" $ \pw -> withPage pw $ \page -> do
    goto page (at "input.html")
    click (locator page "#later-log")
    ready <- waitForConsole page (\m -> "learn: compiler ready" `Text.isPrefixOf` m.text)
    ready `shouldBe` ConsoleMessage "log" "learn: compiler ready in 12 s"
    -- Written before the call: found at once.
    waitForConsole (withTimeout 300 page) (\m -> "learn:" `Text.isPrefixOf` m.text) `shouldReturn` ready
    _ <- timesOut (waitForConsole (withTimeout 300 page) (\m -> m.kind == "error"))
    pure ()

  it "reads an element's text once it satisfies a test, and says what it last was" $ \pw -> withPage pw $ \page -> do
    goto page (at "input.html")
    click (locator page "#start")
    awaitText (locator page "#status") (`elem` ["done", "failed"]) `shouldReturn` "done"
    try (awaitText (locator (withTimeout 300 page) "#status") (== "never")) >>= \case
      Left (HUnitFailure _ (Reason message)) -> message `shouldContain` "\"done\""
      Left other -> expectationFailure ("another failure: " <> show other)
      Right t -> expectationFailure ("it returned " <> show t)

  it "calls a function with a locator's element" $ \pw -> withPage pw $ \page -> do
    goto page (at "input.html")
    evaluateOn (locator page "#canvas") "(canvas, scale) => canvas.width * scale" (2 :: Int) `shouldReturn` (400 :: Int)
    _ <- timesOut (evaluateOn (locator (withTimeout 300 page) "#absent") "(e) => e.id" () :: IO Value)
    pure ()
