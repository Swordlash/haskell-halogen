-- | Pages under a Content Security Policy without @'unsafe-eval'@, as a
-- real server sends: every wait of the API, the probe, and the test's own
-- JavaScript work there. Playwright's own @waitForFunction@ compiles its
-- predicate in the page with @eval@, which such a policy refuses; the
-- driver's does not.
module Test.Policy (spec) where

import Control.Exception (try)
import Data.Text (Text)
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Test.Server (withServer)

spec :: SpecWith Playwright
spec = describe "a Content Security Policy" $ do
  mapM_ underPolicy [("in a header", "csp.html?csp"), ("in a meta element", "csp-meta.html")]
  where
    underPolicy (how, path) = it ("lets every wait work under a policy " <> how) $ \pw -> withServer $ \base -> withPage pw $ \page -> do
      -- The probe is an init script, which the policy does not govern.
      visit page (base <> path)
      -- The policy is in force: the page's inline script was refused, and
      -- so is eval in the page's own code. (Not in what the test evaluates:
      -- the DevTools protocol's evaluation may eval.)
      evaluate page "window.inline === undefined" `shouldReturn` True
      evaluate page "window.evalRefused" `shouldReturn` ("EvalError" :: Text)
      -- An action settles through the probe.
      click (locator page "#slow")
      textContent (locator page "#state") `shouldReturn` "done"
      click (locator page "#wait")
      waitQuiet page
      textContent (locator page "#state") `shouldReturn` "answered"
      -- The page's own timer: each wait, in turn.
      click (locator page "#later")
      waitForFunction page "document.getElementById('later').textContent === 'later'"
      waitForFunctionWith page "(expected) => document.getElementById('later').textContent === expected" ("later" :: Text)
      waitFor (locator page "#shown") Visible
      locator page "#later" `shouldHaveText` "later"
      _ <- awaitText (locator page "#later") (== "later")
      eventually page (isVisible (locator page "#shown") `shouldReturn` True)
      _ <- waitForConsole page (\m -> m.text == "later")
      evaluateOn (locator page "#later") "(e, suffix) => e.textContent + suffix" ("!" :: Text) `shouldReturn` ("later!" :: Text)
      evaluateWith page "(n) => n + 1" (41 :: Int) `shouldReturn` (42 :: Int)
      -- An event the page's timer dispatches, waited for: read at once.
      waitForEvent (OnElement (locator page "#later")) "later" (click (locator page "#later"))
      getAttribute (locator page "#later") "data-count" `shouldReturn` Just "2"
      -- A wait that does not come true times out, rather than failing on
      -- the policy.
      try (waitForFunction (withTimeout 300 page) "false") >>= \case
        Left (e :: PlaywrightError) -> e.name `shouldBe` "TimeoutError"
        Right () -> expectationFailure "waitForFunction returned"
      errors <- pageErrors page
      errors `shouldBe` []
      click (locator page "#away")
      waitForURL page (base <> "second.html")
