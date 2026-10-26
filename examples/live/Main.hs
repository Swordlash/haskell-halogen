{-# LANGUAGE CPP #-}

-- | The loopback: the board through the wire, in one page, with the
-- traffic beside it (npm run serve-wasm -- live). The board served to many
-- pages is the native halogen-example-live-server (Server.hs).
module Main where

import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Example.Live (newBoard)
import Example.Live.Wire (page)
import Halogen (HalogenSocket (..))
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))
#endif

-- | Mount the page, and give back how to end it.
mount :: BrowserDOM (BrowserDOM ())

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
mount = do
  shared <- liftIO newBoard
  app <- HA.selectElement (QuerySelector "#app") >>= maybe (panic "index.html has no #app to mount the example into") pure
  socket <- runUI (page shared) () app
  pure socket.dispose
#else
mount = panic "This page runs in a browser: build it with the JavaScript or wasm backend, or run halogen-example-live-server."
#endif

main :: IO ()
main = runBrowserDOM $ do
  -- Browser GHCi (toolchain/dev.sh) keeps the page between runs of :main:
  -- end the last run's tree, and keep how to end this run's.
  HA.endPreviousRun
  dispose <- mount
  HA.keepForNextRun dispose

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = main
#endif
