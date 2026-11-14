-- | Back and forward, for every kind of entry the page's history has: a
-- state the page pushed, a fragment, another document, a frame's own
-- navigation; and none at all. Each returns once the page has gone there
-- and settled, so what the page shows is read at once, never retried: a
-- traversal that returned early would read the old state, and one that
-- waited for a @load@ that does not come would time out.
module Test.History (spec) where

import Control.Exception (try)
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen.Playwright
import Halogen.Playwright.Hspec

-- | The call fails with no entry to go to.
noEntry :: IO () -> IO ()
noEntry action =
  try action >>= \case
    Left (e :: PlaywrightError) -> do
      e.name `shouldBe` "NoHistoryEntry"
      Text.unpack e.message `shouldContain` "no entry"
    Right () -> expectationFailure "it went somewhere"

spec :: Text -> SpecWith Playwright
spec base = describe "history" $ do
  let at path = base <> path

  -- history.html's popstate is handled as a program handles it: busy for
  -- 200 ms before it shows the state, so the reads below see it only if
  -- the traversal settled through the probe.
  it "goes back and forward through states the page pushed" $ \pw -> withPage pw $ \page -> do
    goto page (at "history.html")
    click (locator page "#push1")
    click (locator page "#push2")
    url page `shouldReturn` at "history.html?state=2"
    goBack page
    url page `shouldReturn` at "history.html?state=1"
    textContent (locator page "#state") `shouldReturn` "1"
    goBack page
    url page `shouldReturn` at "history.html"
    textContent (locator page "#state") `shouldReturn` "0"
    goForward page
    url page `shouldReturn` at "history.html?state=1"
    textContent (locator page "#state") `shouldReturn` "1"
    -- One popstate each, in the same document.
    textContent (locator page "#pops") `shouldReturn` "3"

  it "goes back from a fragment" $ \pw -> withPage pw $ \page -> do
    goto page (at "history.html")
    click (locator page "#part")
    url page `shouldReturn` at "history.html#part"
    textContent (locator page "#hash") `shouldReturn` "#part"
    goBack page
    url page `shouldReturn` at "history.html"
    textContent (locator page "#hash") `shouldReturn` ""
    goForward page
    textContent (locator page "#hash") `shouldReturn` "#part"

  it "goes back to another document, once it has loaded" $ \pw -> withPage pw $ \page -> do
    goto page (at "program.html")
    goto page (at "basics.html")
    goBack page
    url page `shouldReturn` at "program.html"
    -- Its script has run: the program answers.
    activity page `shouldReturn` Just Activity {trees = 1, running = 0, waiting = 0}
    goForward page
    textContent (locator page "h1") `shouldReturn` "Basics"

  -- The wiki's editor in an iframe: Back undoes the frame's navigation
  -- alone, and no load of the page's comes.
  it "goes back and forward in a frame that navigated" $ \pw -> withPage pw $ \page -> do
    goto page (at "history.html")
    click (locator page "#frame-link")
    locator (frameAt page "**/second.html") "h1" `shouldHaveText` "Second"
    goBack page
    url page `shouldReturn` at "history.html"
    frames page `shouldReturn` [at "history.html", at "framed.html"]
    textContent (locator (frameAt page "**/framed.html") "#status") `shouldReturn` "ready"
    goForward page
    frames page `shouldReturn` [at "history.html", at "second.html"]
    textContent (locator (frameAt page "**/second.html") "h1") `shouldReturn` "Second"

  it "says there is nowhere to go" $ \pw -> withPage pw $ \page -> do
    -- A page that has gone nowhere has nothing behind it.
    noEntry (goBack page)
    goto page (at "basics.html")
    noEntry (goForward page)
    title page `shouldReturn` "Basics"
