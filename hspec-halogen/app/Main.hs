{-# LANGUAGE TemplateHaskell #-}

-- | The hspec-halogen executable: the host side of a browser test suite.
--
-- > hspec-halogen test <suite.wasm> [hspec args...]
--
-- runs a wasm test suite, and is meant as cabal's @--test-wrapper@: a suite
-- built with hspec-halogen goes to headless Chromium, any other runs under
-- Node. It post-links the suite with the wasm GHC's post-linker first, as any
-- wasm test wrapper has to.
--
-- > hspec-halogen open <url>
--
-- opens a page in a Chromium it controls, for a suite run in browser GHCi.
--
-- Both run the embedded JavaScript with @node@, which needs the npm packages
-- @playwright@ and @\@bjorn3/browser_wasi_shim@ in the project, and Chromium
-- installed for Playwright. See the package's README.
module Main (main) where

import Data.Maybe (fromMaybe)
import Embed (embedText)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (IOMode (..), hPutStrLn, hSetEncoding, stderr, utf8, withFile)
import System.IO qualified as IO
import System.IO.Temp (withSystemTempDirectory)
import System.Process (rawSystem, readProcess)

runnerScript :: String
runnerScript = $(embedText "js/runner.mjs")

openScript :: String
openScript = $(embedText "js/open.mjs")

main :: IO ()
main =
  getArgs >>= \case
    "test" : suite : hspecArgs -> test suite hspecArgs
    ["open", url] -> open url
    [flag] | flag `elem` ["-h", "--help"] -> putStr usage
    _ -> hPutStrLn stderr usage >> exitWith (ExitFailure 2)

usage :: String
usage =
  unlines
    [ "Usage: hspec-halogen test <suite.wasm> [hspec args...]"
    , "       hspec-halogen open <url>"
    , ""
    , "  test   Run a wasm test suite: one built with hspec-halogen in headless"
    , "         Chromium, any other under Node. Use it as cabal's --test-wrapper."
    , "  open   Open a page in a Chromium that Playwright controls, for a suite"
    , "         run in browser GHCi."
    , ""
    , "Environment:"
    , "  HSPEC_HALOGEN_WASM_GHC  the wasm GHC to post-link with (wasm32-wasi-ghc)"
    , "  HSPEC_HALOGEN_NODE      the node to run with (node)"
    , "  HSPEC_HALOGEN_HEADED    show the browser while testing"
    , "  HSPEC_HALOGEN_HEADLESS  open no window for 'open'"
    , "  HSPEC_HALOGEN_SLOWMO    milliseconds to wait before every browser action"
    , "  HSPEC_HALOGEN_TIMEOUT   seconds the whole suite may take (600)"
    ]

test :: FilePath -> [String] -> IO ()
test suite hspecArgs = withSystemTempDirectory "hspec-halogen" $ \dir -> do
  wasmGhc <- fromEnv "HSPEC_HALOGEN_WASM_GHC" "wasm32-wasi-ghc"
  node <- fromEnv "HSPEC_HALOGEN_NODE" "node"
  libdir <- trim <$> readProcess wasmGhc ["--print-libdir"] ""
  let jsffi = dir </> "ghc_wasm_jsffi.mjs"
      runner = dir </> "runner.mjs"
  postLinked <- rawSystem node [libdir </> "post-link.mjs", "--input", suite, "--output", jsffi]
  case postLinked of
    ExitSuccess -> do
      writeUtf8 runner runnerScript
      exitWith =<< rawSystem node (runner : suite : jsffi : hspecArgs)
    failure -> exitWith failure

open :: String -> IO ()
open url = withSystemTempDirectory "hspec-halogen" $ \dir -> do
  node <- fromEnv "HSPEC_HALOGEN_NODE" "node"
  let opener = dir </> "open.mjs"
  writeUtf8 opener openScript
  exitWith =<< rawSystem node [opener, url]

fromEnv :: String -> String -> IO String
fromEnv name fallback = fromMaybe fallback <$> lookupEnv name

trim :: String -> String
trim = reverse . dropWhile (`elem` "\r\n ") . reverse

writeUtf8 :: FilePath -> String -> IO ()
writeUtf8 path contents = withFile path WriteMode $ \handle -> hSetEncoding handle utf8 >> IO.hPutStr handle contents
