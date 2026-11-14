-- | Pages a page opens: by @window.open@, by a link to a new tab, as a
-- sign-in window that tells its opener who signed in and closes. Each is a
-- page of the context like any other.
module Test.Popups (spec) where

import Control.Exception (try)
import Data.Text (Text)
import Halogen.Playwright
import Halogen.Playwright.Hspec

-- | Milliseconds the page takes over a fixed computation, by its own clock.
computation :: Page -> IO Double
computation page =
  evaluate
    page
    "(() => { const t = performance.now(); let x = 0; for (let i = 0; i < 3e7; i++) x = (x + i * 7) % 1000003; window.sink = x; return performance.now() - t; })()"

spec :: Text -> SpecWith Playwright
spec base = describe "popups" $ do
  let at path = base <> path

  it "gives the page a click opens, loaded, and lists the context's pages" $ \pw -> withContext pw defaultContextOptions $ \ctx -> do
    page <- newPage ctx
    goto page (at "popup.html")
    ((), opened) <- waitForPopup page (click (locator page "#open"))
    url opened `shouldReturn` at "second.html"
    -- Loaded: read at once.
    textContent (locator opened "h1") `shouldReturn` "Second"
    listed <- pages ctx
    traverse url listed `shouldReturn` [at "popup.html", at "second.html"]
    closePage opened
    length <$> pages ctx `shouldReturn` 1

  -- A new tab gets what every page of the context gets: the probe, so its
  -- program answers, and the context's init scripts.
  it "gives a new tab the probe and the context's init scripts" $ \pw -> withContext pw defaultContextOptions $ \ctx -> do
    addInitScript ctx "window.initialised = 'yes'"
    page <- newPage ctx
    goto page (at "popup.html")
    ((), tab) <- waitForPopup page (click (locator page "#tab"))
    waitStarted tab
    click (locator tab "#slow")
    textContent (locator tab "#state") `shouldReturn` "done"
    evaluate tab "window.initialised" `shouldReturn` ("yes" :: Text)

  -- An OAuth window: filled in, it posts back to its opener and closes as
  -- the click is handled.
  it "drives a sign-in window that closes itself" $ \pw -> withContext pw defaultContextOptions $ \ctx -> do
    page <- newPage ctx
    goto page (at "popup.html")
    ((), signIn) <- waitForPopup page (click (locator page "#signin"))
    fill (byLabel signIn "Name") "ada"
    click (byRoleNamed signIn "button" "Sign in")
    locator page "#signed" `shouldHaveText` "ada"
    eventually page (length <$> pages ctx `shouldReturn` 1)

  it "fails when no page is opened, after the timeout" $ \pw -> withPage pw $ \page -> do
    goto page (at "popup.html")
    try (waitForPopup (withTimeout 300 page) (click (locator page "#signed"))) >>= \case
      Left (e :: PlaywrightError) -> e.name `shouldBe` "TimeoutError"
      Right _ -> expectationFailure "a popup came"

  it "slows a popup down as every page when the options say so" $ \_ -> do
    options <- optionsFromEnvironment
    withPlaywright options {throttle = Just 6} $ \pw -> withPage pw $ \page -> do
      goto page (at "popup.html")
      ((), opened) <- waitForPopup page (click (locator page "#open"))
      _ <- computation opened
      slowed <- computation opened
      throttleCPU opened 1
      _ <- computation opened
      full <- computation opened
      slowed `shouldSatisfy` (> full * 2.5)
