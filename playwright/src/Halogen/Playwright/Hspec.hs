-- | "Halogen.Playwright" in hspec: one driver for a group of tests, a
-- fresh page for each test, and a failure that says what the page said.
--
-- @
-- main :: IO ()
-- main = hspec $ playwright $ do
--   it "starts" $ inPage $ \\page -> do
--     visit page "http://127.0.0.1:8080/"
--     locator page "h1" \`shouldHaveText\` "Hello"
-- @
module Halogen.Playwright.Hspec
  ( playwright
  , inPage
  , withFailureContext
  )
where

import Control.Exception (catch, throwIO, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (intercalate)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack, callStack, getCallStack)
import Halogen.Playwright
import Halogen.Playwright.Internal (Page (..), Playwright (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec (Spec, SpecWith, aroundAll)

-- | One node and Chromium for the group, with 'optionsFromEnvironment'.
playwright :: SpecWith Playwright -> Spec
playwright = aroundAll (\body -> optionsFromEnvironment >>= \o -> withPlaywright o body)

-- | A fresh context and page for an example. When it fails, the failure
-- says the page's address, its console errors and its uncaught errors,
-- and a screenshot is saved under the artifacts directory if one is set
-- (@HALOGEN_PLAYWRIGHT_ARTIFACTS@).
inPage :: (HasCallStack) => (Page -> IO a) -> Playwright -> IO a
inPage body pw = withPage pw (\page -> withFailureContext page (body page))

-- | Run the body; if it fails, add the page's state to the failure.
--
-- An expectation's failure (an @HUnitFailure@) keeps its location and gets
-- the page's state after its message; a 'PlaywrightError' (an action that
-- timed out, say) becomes a failure with the error and the page's state,
-- located where 'inPage' was called.
withFailureContext :: (HasCallStack) => Page -> IO a -> IO a
withFailureContext page body =
  ( body `catch` \(HUnitFailure loc reason) -> do
      section <- pageState page
      throwIO (HUnitFailure loc (append section reason))
  )
    `catch` \(e :: PlaywrightError) -> do
      section <- pageState page
      throwIO (HUnitFailure here (Reason (Text.unpack (e.method <> ": " <> e.name <> ": " <> e.message) <> "\n\n" <> section)))
  where
    here = case reverse (getCallStack callStack) of
      (_, loc) : _ -> Just loc
      [] -> Nothing
    append section = \case
      Reason message -> Reason (message <> "\n\n" <> section)
      -- hspec prints the preface above what was expected and what came.
      ExpectedButGot preface expected actual ->
        ExpectedButGot (Just (maybe "" (<> "\n\n") preface <> section)) expected actual

-- | The page's address, what it said, and where its screenshot went.
pageState :: Page -> IO String
pageState page = do
  address <- either (\(e :: PlaywrightError) -> "unknown (" <> e.message <> ")") id <$> try (url page)
  consoleErrors <- filter (\m -> m.kind == "error") <$> consoleMessages page
  errors <- pageErrors page
  shot <- saveScreenshot page
  pure . intercalate "\n" $
    ["The page: " <> Text.unpack address]
      <> section "Its console errors:" (map (.text) consoleErrors)
      <> section "Its uncaught errors:" errors
      <> maybe [] (\s -> ["Its screenshot: " <> s]) shot
  where
    section heading items =
      if null items then [] else heading : map (("  " <>) . Text.unpack) items

-- | A screenshot under the artifacts directory, if one is set; its path,
-- or why there is none.
saveScreenshot :: Page -> IO (Maybe String)
saveScreenshot page = case page.playwright.options.artifacts of
  Nothing -> pure Nothing
  Just dir -> do
    n <- atomicModifyIORef' shots (\k -> (k + 1, k))
    createDirectoryIfMissing True dir
    let path = dir </> ("failure-" <> show n <> ".png")
    try (screenshot page path) >>= \case
      Right () -> pure (Just path)
      Left (e :: PlaywrightError) -> pure (Just ("none (" <> Text.unpack e.message <> ")"))

-- | How many screenshots this program has saved, for their names.
shots :: IORef Int
shots = unsafePerformIO (newIORef 1)
{-# NOINLINE shots #-}
