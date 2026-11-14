-- | A page calling the test: functions of the test's exposed to a page or
-- a context, each call answered on a thread of its own, a failure
-- rejecting the page's promise and kept for the test's report.
module Test.Exposed (spec) where

import Control.Concurrent.MVar
import Control.Exception (throwIO, try)
import Data.Aeson (FromJSON (..), Value, withObject, (.:))
import Data.List (isInfixOf)
import Data.Text (Text)
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Halogen.Playwright.Internal (Context (..), exposedFailures)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))

-- | What a page signs in with.
data SignIn = SignIn {user :: Text, remember :: Bool}

instance FromJSON SignIn where
  parseJSON = withObject "SignIn" $ \o -> SignIn <$> o .: "user" <*> o .: "remember"

spec :: Text -> SpecWith Playwright
spec base = describe "functions the page calls" $ do
  let at path = base <> path

  it "answers a page's call with the test's function" $ \pw -> withPage pw $ \page -> do
    goto page (at "exposed.html")
    exposeFunction page "double" (\(n :: Int) -> pure (n * 2))
    click (locator page "#ask")
    locator page "#answer" `shouldHaveText` "42"
    -- JSON both ways, and no argument is null.
    exposeFunction page "signIn" (\(s :: SignIn) -> pure (if s.remember then "welcome back, " <> s.user else "welcome, " <> s.user))
    evaluate page "window.signIn({user: 'ada', remember: true})" `shouldReturn` ("welcome back, ada" :: Text)
    exposeFunction page "nothing" (\(v :: Value) -> pure v)
    evaluate page "window.nothing()" `shouldReturn` (Nothing :: Maybe Int)

  it "runs each call on a thread of its own, so a call may wait while another is answered" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    letGo <- newEmptyMVar
    exposeFunction page "slow" (\(_ :: Value) -> readMVar letGo >> pure ("slow" :: Text))
    exposeFunction page "quick" (\(_ :: Value) -> pure ("quick" :: Text))
    _ :: Value <- evaluate page "(window.slowAnswer = window.slow(null), null)"
    evaluate page "window.quick(null)" `shouldReturn` ("quick" :: Text)
    putMVar letGo ()
    evaluate page "window.slowAnswer" `shouldReturn` ("slow" :: Text)

  it "rejects the page's promise with what the function threw, and keeps it for the failure" $ \pw -> withPage pw $ \page -> do
    goto page (at "exposed.html")
    exposeFunction page "double" (\(_ :: Int) -> throwIO (userError "the test's function broke") :: IO Int)
    click (locator page "#ask")
    locator page "#answer" `shouldContainText` "the test's function broke"
    -- So does an argument that does not decode.
    exposeFunction page "typed" (\(n :: Int) -> pure n)
    evaluate page "window.typed('not a number').then(() => 'answered', (e) => e.message)" >>= (`shouldSatisfy` ("DecodeError" `isInfixOf`) . (\(t :: String) -> t))
    kept <- exposedFailures pw (pageContext page).handle
    length kept `shouldBe` 2
    failure <- try (withFailureContext page (locator page "#answer" `shouldHaveText` "42"))
    case failure of
      Left (HUnitFailure _ (ExpectedButGot (Just preface) _ _)) -> do
        preface `shouldSatisfy` ("Exposed functions that failed:" `isInfixOf`)
        preface `shouldSatisfy` ("double: user error (the test's function broke)" `isInfixOf`)
      other -> expectationFailure ("another outcome: " <> show (either Just (const Nothing) other))

  it "gives a context's function to every page of it, after a navigation too" $ \pw -> withContext pw defaultContextOptions $ \ctx -> do
    calls <- newMVar (0 :: Int)
    exposeFunction ctx "double" (\(n :: Int) -> modifyMVar_ calls (pure . (+ 1)) >> pure (n * 2))
    page <- newPage ctx
    goto page (at "exposed.html")
    click (locator page "#ask")
    locator page "#answer" `shouldHaveText` "42"
    goto page (at "second.html")
    goto page (at "exposed.html")
    click (locator page "#ask")
    locator page "#answer" `shouldHaveText` "42"
    other <- newPage ctx
    goto other (at "basics.html")
    evaluate other "window.double(5)" `shouldReturn` (10 :: Int)
    readMVar calls `shouldReturn` 3
