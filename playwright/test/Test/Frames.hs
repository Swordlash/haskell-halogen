-- | Inside frames: found by their element or by their address.
module Test.Frames (spec) where

import Control.Exception (try)
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen.Playwright
import Halogen.Playwright.Hspec

spec :: Text -> SpecWith Playwright
spec base = describe "frames" $ do
  let at path = base <> path

  -- smoke-test-server.mjs: interactive SVGs in sandboxed frames; a click
  -- in the flame graph changes it, read through its iframe element.
  it "acts and reads inside a frame, found by its element" $ \pw -> withPage pw $ \page -> do
    goto page (at "frames.html")
    let flame = inFrame page "iframe[title='Where the time goes']"
    locator flame "#status" `shouldHaveText` "ready"
    click (locator flame "#child")
    locator flame "#status" `shouldHaveText` "zoomed child"
    -- From a locator too, and a function on an element in it.
    let inBody = inFrame (locator page "body") "iframe.flame"
    evaluateOn (locator inBody "#status") "(e) => e.textContent" () `shouldReturn` ("zoomed child" :: Text)

  -- smoke-test-server.mjs: the frame found by its address, wherever it has
  -- navigated since; the frames a page shows, by their addresses.
  it "finds a frame by its address, and lists the frames" $ \pw -> withPage pw $ \page -> do
    goto page (at "frames.html")
    -- The wanderer goes to second.html by itself, a moment after it loads.
    locator (frameAt page "**/second.html") "h1" `shouldHaveText` "Second"
    innerText (locator (frameAt page "**/framed.html?interactive") "#status") `shouldReturn` "ready"
    shown <- frames page
    shown `shouldBe` [at "frames.html", at "framed.html?interactive", at "second.html"]

  it "fails in a frame that is not there, naming its address" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    try (textContent (locator (frameAt page "**/nowhere.html") "p")) >>= \case
      Left (e :: PlaywrightError) -> do
        e.name `shouldBe` "NoFrame"
        Text.unpack e.message `shouldContain` "**/nowhere.html"
      Right t -> expectationFailure ("it read " <> show t)
