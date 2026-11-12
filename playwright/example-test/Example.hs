-- | A built WebAssembly example in Chromium: where the probe's two halves,
-- the driver's and the program's, meet in a real program, which
-- @cabal test@ cannot build. It serves @dist-newstyle/wasm/public/vanilla@
-- from the repository's root (@HALOGEN_EXAMPLE_DIR@ names another, built
-- the same way), a counter whose page is prerendered and adopted.
--
-- Without the directory the examples are pending, with the command that
-- builds it, unless @HALOGEN_EXAMPLE_REQUIRED=1@ (as
-- @toolchain/test-example.sh@ and CI set it), when they fail.
module Main (main) where

import Control.Monad (forM_, unless)
import Data.Text qualified as Text
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Halogen.Playwright.Serve (serveDirectory)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

main :: IO ()
main = do
  dir <- maybe ("../dist-newstyle/wasm/public/vanilla") id <$> lookupEnv "HALOGEN_EXAMPLE_DIR"
  required <- (== Just "1") <$> lookupEnv "HALOGEN_EXAMPLE_REQUIRED"
  built <- doesFileExist (dir </> "app.wasm")
  if built
    then serveDirectory dir (hspec . spec)
    else
      hspec
        $ describe "the vanilla example, built for WebAssembly"
        $ it "is served"
        $ if required
          then expectationFailure (dir </> "app.wasm" <> " is missing: build it with sh toolchain/build-wasm.sh vanilla")
          else pendingWith ("no " <> dir <> "/app.wasm: build it with sh toolchain/build-wasm.sh vanilla, or run npm run test-example")

-- | The count the counter shows: the one element of the app whose text is
-- this number.
counted :: Page -> Int -> Locator
counted page n = locator page ("#app div:text-is('" <> Text.pack (show n) <> "')")

spec :: Text.Text -> Spec
spec base = playwright $ describe "the vanilla example, built for WebAssembly" $ do
  it "starts: the prerendered page adopted and the program idle" $ inPage $ \page -> do
    goto page base
    -- Prerendered: the count is there before the program has started.
    count (counted page 0) `shouldReturn` 1
    visit page base
    count (counted page 0) `shouldReturn` 1
    activity page >>= \case
      Just a -> do
        a.trees `shouldSatisfy` (>= 1)
        a.running `shouldBe` 0
      Nothing -> expectationFailure "the program did not answer the probe"

  it "shows what a click rendered as soon as the click returns" $ inPage $ \page -> do
    visit page base
    let plus = byRoleNamed page "button" "+"
    forM_ [1 .. 6 :: Int] $ \n -> do
      click plus
      -- Read at once: no eventually, no waitFor.
      count (counted page n) `shouldReturn` 1
      count (counted page (n - 1)) `shouldReturn` 0
    -- Past five the counter renders two more buttons.
    count (byRoleNamed page "button" "++") `shouldReturn` 1
    click (byRoleNamed page "button" "--")
    count (counted page 4) `shouldReturn` 1
    count (byRoleNamed page "button" "++") `shouldReturn` 0

  it "says nothing wrong on its console and throws nothing" $ inPage $ \page -> do
    visit page base
    click (byRoleNamed page "button" "+")
    waitQuiet page
    errors <- filter (\m -> m.kind == "error") <$> consoleMessages page
    errors `shouldBe` []
    thrown <- pageErrors page
    unless (null thrown) $ expectationFailure ("the page threw: " <> show thrown)
