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
