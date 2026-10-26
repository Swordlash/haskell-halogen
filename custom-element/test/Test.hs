{-# LANGUAGE CPP #-}

-- | The browser suite: built as a reactor exporting @hs_start@ on
-- WebAssembly, and run in headless Chromium by the hspec-halogen executable
-- on both browser backends. Under browser GHCi (toolchain/dev-test.sh) it is
-- run with @:main@ instead. Natively it only reports that it skipped.
--
-- Every test defines element names of its own: a name, once defined, is the
-- page's for good, and the suite's tests share one page.
module Main (main) where

import Halogen.VDom.DOM.Monad (BrowserDOM)
import Test.Hspec.Halogen (describe, runBrowserTests)
import Test.Using qualified
#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Test.Define qualified
import Test.Lifecycle qualified
import Test.Methods qualified
#endif

main :: IO ()
main = runBrowserTests $ do
  describe "CustomElement.onEvent" (Test.Using.spec BrowserDOM)
#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
  -- These define elements, which needs the browser's DOM
  -- ('MonadBrowserDOM'), which 'BrowserDOM' has only in a browser.
  describe "CustomElement.define" (Test.Define.spec BrowserDOM)
  describe "CustomElement methods" (Test.Methods.spec BrowserDOM)
  describe "CustomElement lifecycle" (Test.Lifecycle.spec BrowserDOM)
#endif

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" main :: IO ()
#endif
