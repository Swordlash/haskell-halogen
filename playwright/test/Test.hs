-- | The driver against pages in test/web/, served on a free port. It needs
-- node, the npm package playwright and Chromium; without them it fails
-- with the driver's message.
module Main (main) where

import Data.Text qualified as Text
import Halogen.Playwright (Options (..), optionsFromEnvironment, withPlaywright)
import System.Environment (lookupEnv)
import Test.Driver qualified
import Test.Hspec (aroundAll, hspec)
import Test.Failures qualified
import Test.Probe qualified
import Test.Server (withDirectory)

main :: IO ()
main = withDirectory "test/web" $ \port -> do
  let base = Text.pack ("http://127.0.0.1:" <> show port <> "/")
  -- A short timeout, so that the tests of timeouts are quick, unless the
  -- environment says otherwise.
  timeoutSet <- lookupEnv "HALOGEN_PLAYWRIGHT_TIMEOUT"
  options <- optionsFromEnvironment
  let options' = maybe options {timeout = 2000} (const options) timeoutSet
  hspec $ do
    aroundAll (withPlaywright options') $ do
      Test.Driver.spec base
      Test.Probe.spec base
    Test.Failures.spec base
