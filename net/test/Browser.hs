{-# LANGUAGE CPP #-}

-- | The browser suite: built as a reactor exporting @hs_start@ on
-- WebAssembly, and run in headless Chromium by the hspec-halogen executable
-- on both browser backends, which serves test/web next to the page and loads
-- its network double. Under browser GHCi (toolchain/dev-test.sh) it is run
-- with @:main@ instead. Natively it only reports that it skipped.
module Main (main) where

import Halogen.VDom.DOM.Monad (BrowserDOM)
import Test.Component qualified
import Test.Fetch qualified
import Test.Hspec.Halogen (describe, runBrowserTests)

main :: IO ()
main = runBrowserTests $ do
  describe "fetch" Test.Fetch.spec
  describe "in a component" (Test.Component.spec BrowserDOM)

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" main :: IO ()
#endif
