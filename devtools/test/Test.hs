{-# LANGUAGE CPP #-}

-- | The browser suite: built as a reactor exporting @hs_start@ on
-- WebAssembly, and run in headless Chromium by the hspec-halogen executable
-- on both browser backends. Under browser GHCi (toolchain/dev-test.sh) it is
-- run with @:main@ instead. Natively it only reports that it skipped.
module Main (main) where

import Halogen.VDom.DOM.Monad (BrowserDOM)
import Test.Hspec.Halogen (describe, runBrowserTests)
import Test.Panel qualified

main :: IO ()
main = runBrowserTests $ describe "DevTools panel" (Test.Panel.spec BrowserDOM)

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" main :: IO ()
#endif
