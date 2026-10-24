{-# LANGUAGE CPP #-}

-- | The browser suite: built as a reactor exporting @hs_start@ on
-- WebAssembly, and run in headless Chromium by the hspec-halogen executable
-- on both browser backends, which serves test/web next to the page and loads
-- its doubles, of the network and of WebSocket and event-stream servers. Under browser GHCi (toolchain/dev-test.sh) it is run
-- with @:main@ instead. Natively it only reports that it skipped.
module Main (main) where

import Halogen.VDom.DOM.Monad (BrowserDOM)
import Test.Component qualified
import Test.EventSource qualified
import Test.Fetch qualified
import Test.Hspec.Halogen (describe, runBrowserTests)
import Test.Network qualified
import Test.SocketComponent qualified
import Test.Sockets qualified
import Test.WebSocket qualified

-- The doubles are reset first, since under browser GHCi the page keeps what
-- they noted in the run before.
main :: IO ()
main = do
  Test.Network.reset
  Test.Sockets.reset
  runBrowserTests $ do
    describe "fetch" Test.Fetch.spec
    describe "fetch in a component" (Test.Component.spec BrowserDOM)
    describe "WebSocket" Test.WebSocket.spec
    describe "WebSocket in a component" (Test.SocketComponent.spec BrowserDOM)
    describe "EventSource" Test.EventSource.spec

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" main :: IO ()
#endif
