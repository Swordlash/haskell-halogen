{-# LANGUAGE CPP #-}

-- | The browser suite: built as a reactor exporting @hs_start@ on
-- WebAssembly, and run in headless Chromium by the hspec-halogen executable
-- on both browser backends, which loads test/web/tauri-double.js into the
-- page, a stand-in for Tauri's global API. Under browser GHCi
-- (toolchain/dev-test.sh) it is run with @:main@ instead. Natively it only
-- reports that it skipped.
module Main (main) where

import Halogen.VDom.DOM.Monad (BrowserDOM)
import Test.Component qualified
import Test.Double qualified
import Test.Events qualified
import Test.Hspec.Halogen (describe, runBrowserTests)
import Test.Invoke qualified

-- The double is reset first, since under browser GHCi the page keeps what it
-- saw in the run before.
main :: IO ()
main = do
  Test.Double.reset
  runBrowserTests $ do
    describe "invoke" Test.Invoke.spec
    describe "events" Test.Events.spec
    describe "in a component" (Test.Component.spec BrowserDOM)

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" main :: IO ()
#endif
