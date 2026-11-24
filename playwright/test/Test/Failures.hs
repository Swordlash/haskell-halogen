-- | What 'inPage' adds to a failure: the page's address, its console
-- errors, its uncaught errors and a screenshot.
module Test.Failures (spec) where

import Control.Exception (throwIO, try)
import Data.List (isInfixOf)
import Data.Text (Text)
import Halogen.Playwright
import Halogen.Playwright.Hspec (withFailureContext)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec

reasonText :: FailureReason -> String
reasonText = \case
  Reason r -> r
  ExpectedButGot preface expected actual -> maybe "" id preface <> "\n" <> expected <> "\n" <> actual

spec :: Text -> Spec
spec base = describe "inPage" $ do
  let at path = base <> path

  it "adds the page's state and a screenshot to a failure" $
    withSystemTempDirectory "playwright-artifacts" $ \dir -> do
      options <- optionsFromEnvironment
      withPlaywright options {artifacts = Just dir} $ \pw -> withPage pw $ \page -> do
        failure <- try $ withFailureContext page $ do
          goto page (at "plain.html")
          click (locator page "#complain")
          eventually page (pageErrors page >>= (`shouldNotBe` []))
          locator page "#go" `shouldHaveText` "gone"
        case failure of
          Left (HUnitFailure location reason) -> do
            let message = reasonText reason
            message `shouldSatisfy` (("The page: " <> show' (at "plain.html")) `isInfixOf`)
            message `shouldSatisfy` ("something is wrong" `isInfixOf`)
            message `shouldSatisfy` ("and it broke" `isInfixOf`)
            message `shouldSatisfy` ("Its screenshot: " `isInfixOf`)
            -- What was expected and what came are still there.
            message `shouldSatisfy` ("\"gone\"" `isInfixOf`)
            -- And where: this file.
            fmap (\l -> "Test/Failures.hs" `isInfixOf` show l) location `shouldBe` Just True
            files <- listDirectory dir
            length files `shouldBe` 1
            mapM_ (\f -> doesFileExist (dir </> f) `shouldReturn` True) files
          Right () -> throwIO (userError "the body did not fail")

  -- A page whose own address is answered with a 404 (a static host's
  -- 404.html) says only "Failed to load resource" in its console; the
  -- failure names the address, and says it is the page's own, so that it
  -- is not taken for a missing file. A missing file the page asked for is
  -- named too, and not as the page's.
  it "names the address of every console error, saying which is the page's own" $ do
    options <- optionsFromEnvironment
    withPlaywright options $ \pw -> withPage pw $ \page -> do
      let own = at "no-such-page.html"
          other = at "no-such-file.txt"
          from address m = m.kind == "error" && m.url == Just address
      failure <- try $ withFailureContext page $ do
        gotoStatus page own `shouldReturn` Just 404
        () <- evaluate page "fetch('no-such-file.txt').then(() => null)"
        _ <- waitForConsole page (from own)
        _ <- waitForConsole page (from other)
        expectationFailure "the page was not found"
      case failure of
        Left (HUnitFailure _ reason) -> do
          let errorAbout address = filter (\l -> "404" `isInfixOf` l && show' address `isInfixOf` l) (lines (reasonText reason))
          case errorAbout own of
            [l] -> l `shouldSatisfy` ((": the page's own address, " <> show' own) `isInfixOf`)
            ls -> expectationFailure ("not one console error names the page's address: " <> show ls)
          case errorAbout other of
            [l] -> do
              l `shouldSatisfy` (("(" <> show' other) `isInfixOf`)
              l `shouldNotSatisfy` ("the page's own address" `isInfixOf`)
            ls -> expectationFailure ("not one console error names the file's address: " <> show ls)
        Right () -> throwIO (userError "the body did not fail")

  it "makes a failed action a failure with the page's state" $ do
    options <- optionsFromEnvironment
    withPlaywright options {timeout = 500} $ \pw -> withPage pw $ \page -> do
      failure <- try $ withFailureContext page $ do
        goto page (at "plain.html")
        click (locator page "#absent")
      case failure of
        Left (HUnitFailure _ reason) -> do
          let message = reasonText reason
          message `shouldSatisfy` ("click: TimeoutError" `isInfixOf`)
          message `shouldSatisfy` ("#absent" `isInfixOf`)
          message `shouldSatisfy` ("The page: " `isInfixOf`)
        Right () -> throwIO (userError "the body did not fail")
  where
    show' = filter (/= '"') . show
