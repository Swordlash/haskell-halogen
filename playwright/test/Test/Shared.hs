-- | A group of examples on one page ('onOnePage'), and the console printed
-- as it arrives.
module Test.Shared (spec) where

import Control.Exception (finally)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Halogen.Playwright
import Halogen.Playwright.Hspec
import System.Environment (setEnv, unsetEnv, withArgs)
import System.IO (Handle, hClose, hFlush, stderr, stdout)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec.Runner (Summary (..), defaultConfig, hspecWithResult)

-- | What the action wrote to a handle (stdout, stderr), which is put back
-- afterwards.
capturing :: Handle -> IO a -> IO (a, String)
capturing handle action = withSystemTempFile "captured" $ \path h -> do
  saved <- hDuplicate handle
  hDuplicateTo h handle
  a <- action `finally` (hFlush handle >> hDuplicateTo saved handle)
  hClose h
  written <- readFile path
  length written `seq` pure (a, written)

spec :: Text -> SpecWith Playwright
spec base = do
  let at path = base <> path

  -- examples/worker/test: each group's examples one walk on one page, an
  -- init script of the test's own added before it starts.
  describe "a group on one page" $
    onOnePage defaultContextOptions (\page -> addInitScript (pageContext page) "window.prepared = 'yes';") $ do
      it "starts the walk, the setup done" $ \page -> do
        goto page (at "basics.html")
        evaluate page "window.prepared" `shouldReturn` ("yes" :: Text)
        click (locator page "#count")
      it "goes on from where the example before left the page" $ \page ->
        textContent (locator page "#count") `shouldReturn` "1"

  describe "a group on one page, failing" $
    it "says what the page said in its examples' failures" $ \_ -> do
      let failing = playwright $ onOnePage defaultContextOptions (\_ -> pure ()) $ do
            it "starts" $ \(page :: Page) -> goto page (at "plain.html") :: IO ()
            it "fails" $ \(page :: Page) -> do
              click (locator page "#complain")
              locator page "#go" `shouldHaveText` "gone" :: IO ()
      -- Not the outer run's arguments (a --match, say).
      (summary, out) <- capturing stdout (withArgs [] (hspecWithResult defaultConfig failing))
      summary.summaryFailures `shouldBe` 1
      out `shouldSatisfy` (("The page: " <> Text.unpack (at "plain.html")) `isInfixOf`)
      out `shouldSatisfy` ("something is wrong" `isInfixOf`)

  -- examples/worker/test's WORKER_DEBUG: what the page and its workers
  -- log, printed as it arrives.
  describe "the console relayed" $
    it "prints what a page writes to its console as it arrives, when the options or the environment say so" $ \_ -> do
      setEnv "HALOGEN_PLAYWRIGHT_CONSOLE" "1"
      options <- optionsFromEnvironment `finally` unsetEnv "HALOGEN_PLAYWRIGHT_CONSOLE"
      options.relayConsole `shouldBe` True
      ((), written) <- capturing stderr $ withPlaywright options $ \pw -> withPage pw $ \page -> do
        goto page (at "basics.html")
        click (locator page "#log")
        _ <- waitForConsole page (\m -> m.text == "an error")
        pure ()
      written `shouldSatisfy` ("] [log] logged" `isInfixOf`)
      written `shouldSatisfy` ("] [error] an error" `isInfixOf`)
      optionsFromEnvironment >>= \o -> o.relayConsole `shouldBe` False
