{-# LANGUAGE CPP #-}

-- | The browser suite: built as a reactor exporting @hs_start@ on
-- WebAssembly, and run in headless Chromium by the hspec-halogen executable
-- on both browser backends, which serves test/web next to the page (its
-- bundle.sh bundles Three.js from node_modules). Under browser GHCi
-- (toolchain/dev-test.sh) it is run with @:main@ instead. Natively it only
-- reports that it skipped.
module Main (main) where

import Halogen.VDom.DOM.Monad (BrowserDOM)
import Protolude
import Test.Browser qualified
import Test.Hspec.Halogen (describe, runBrowserTests)

main :: IO ()
main = runBrowserTests $ describe "a scene" (Test.Browser.spec BrowserDOM)

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" main :: IO ()
#endif
