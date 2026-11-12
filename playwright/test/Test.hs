-- | The driver against pages in test/web/, served on a free port. It needs
-- node, the npm package playwright and Chromium; without them it fails
-- with the driver's message.
module Main (main) where

import Halogen.Playwright (Options (..), optionsFromEnvironment, withPlaywright)
import Halogen.Playwright.Serve (serveDirectory)
import System.Environment (lookupEnv)
import Test.DevTools qualified
import Test.Driver qualified
import Test.Failures qualified
import Test.Files qualified
import Test.Frames qualified
import Test.Input qualified
import Test.Hspec (aroundAll, hspec)
import Test.Probe qualified
import Test.Routes qualified
import Test.Serve qualified
import Test.Shared qualified
import Test.State qualified

main :: IO ()
main = serveDirectory "test/web" $ \base -> do
  -- A short timeout, so that the tests of timeouts are quick, unless the
  -- environment says otherwise.
  timeoutSet <- lookupEnv "HALOGEN_PLAYWRIGHT_TIMEOUT"
  options <- optionsFromEnvironment
  let options' = maybe options {timeout = 2000} (const options) timeoutSet
  hspec $ do
    aroundAll (withPlaywright options') $ do
      Test.Driver.spec base
      Test.Probe.spec base
      Test.Routes.spec base
      Test.Files.spec base
      Test.State.spec base
      Test.Input.spec base
      Test.Frames.spec base
      Test.DevTools.spec base
      Test.Shared.spec base
      Test.Serve.spec
    Test.Failures.spec base
