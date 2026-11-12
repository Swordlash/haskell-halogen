-- | Files in and out of the page: downloads and uploads.
module Test.Files (spec) where

import Control.Exception (try)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTime)
import Halogen.Playwright
import Halogen.Playwright.Hspec
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

spec :: Text -> SpecWith Playwright
spec base = describe "files" $ do
  let at path = base <> path

  -- smoke-test.mjs: exporting the wiki downloads a zip file, saved from a
  -- blob: URL under a name of the page's, whose bytes the test reads.
  it "downloads what a click saves, with its name and its bytes" $ \pw -> withPage pw $ \page -> do
    goto page (at "files.html")
    exported <- download page (click (locator page "#export"))
    exported.suggestedFilename `shouldBe` "wiki-export.zip"
    Text.unpack exported.url `shouldStartWith'` "blob:"
    downloadBytes exported `shouldReturn` "PK exported by the page"
    withSystemTempDirectory "playwright-download" $ \dir -> do
      saveDownload exported (dir </> "kept.zip")
      ByteString.readFile (dir </> "kept.zip") `shouldReturn` "PK exported by the page"

  -- smoke-test-server.mjs: an interactive SVG's address opened by itself
  -- in a tab is a download; there Playwright's goto fails ("Download is
  -- starting"), which the script swallowed.
  it "downloads what an address is, from a goto that does not fail" $ \pw -> withPage pw $ \page -> do
    _ <- route page "**/hostile.svg" $ \_ ->
      pure (Fulfill Response {status = 200, headers = [("content-disposition", "attachment; filename=hostile.svg")], contentType = Just "image/svg+xml", body = "<svg/>"})
    opened <- download page (goto page (at "hostile.svg"))
    opened.suggestedFilename `shouldBe` "hostile.svg"
    opened.url `shouldBe` at "hostile.svg"
    downloadBytes opened `shouldReturn` "<svg/>"

  it "fails, after the timeout, when the action downloads nothing" $ \pw -> withPage pw $ \page -> do
    goto page (at "files.html")
    started <- getMonotonicTime
    try (download (withTimeout 300 page) (click (locator page "#upload"))) >>= \case
      Left (e :: PlaywrightError) -> e.name `shouldBe` "TimeoutError"
      Right _ -> expectationFailure "something was downloaded"
    took <- subtract started <$> getMonotonicTime
    took `shouldSatisfy` (< 1.5)
    -- An action that fails is that failure, and the wait is forgotten.
    try (download page (click (locator (withTimeout 300 page) "#absent"))) >>= \case
      Left (e :: PlaywrightError) -> e.method `shouldBe` "click"
      Right _ -> expectationFailure "something was downloaded"

  -- smoke-test.mjs and smoke-test-server.mjs: the export imported back, an
  -- attachment and an icon uploaded, each from bytes the test has.
  it "chooses files for an input, from bytes and from disk" $ \pw -> withPage pw $ \page -> do
    goto page (at "files.html")
    setInputFiles (locator page "#upload") [UploadBytes {name = "notes.txt", mimeType = "text/plain", bytes = "attached by the test"}]
    -- Its change event was handled by the time setInputFiles returned;
    -- reading the file is the page's own promise.
    locator page "#uploaded" `shouldHaveText` "notes.txt text/plain attached by the test"
    withSystemTempDirectory "playwright-upload" $ \dir -> do
      ByteString.writeFile (dir </> "page.md") "# A page"
      setInputFiles (locator page "#upload") [UploadFile (dir </> "page.md"), UploadBytes "smoke.png" "image/png" "png"]
      locator page "#uploaded" `shouldHaveText` "page.md text/markdown # A page | smoke.png image/png png"
      doesFileExist (dir </> "page.md") `shouldReturn` True
    setInputFiles (locator page "#upload") []
    locator page "#uploaded" `shouldHaveText` "nothing"
  where
    shouldStartWith' actual prefix = take (length prefix) actual `shouldBe` prefix
