{-# LANGUAGE CPP #-}

-- | The same program twice over: on the page it mounts the example, and in
-- each Web Worker the pool starts (another instance of this binary) it
-- serves the jobs instead, which is what 'Worker.program' decides.
module Main where

import Example.Worker (jobs)
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.Worker qualified as Worker
import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Example.Worker (component)
import Halogen (HalogenSocket (..))
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))
#endif

-- | Mount the example with a pool of two workers, and give back how to end
-- both.
mount :: BrowserDOM (BrowserDOM ())

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
mount = do
  pool <- Worker.newPool 2
  app <- HA.selectElement (QuerySelector "#app") >>= maybe (panic "index.html has no #app to mount the example into") pure
  socket <- runUI (component pool) () app
  pure (socket.dispose >> Worker.terminate pool)
#else
mount = panic "This example runs in a browser: build it with the JavaScript or wasm backend."
#endif

main :: IO ()
main = Worker.program jobs $ runBrowserDOM $ do
  -- Browser GHCi (toolchain/dev.sh) keeps the page between runs of :main:
  -- end the last run's tree and workers, and keep how to end this run's.
  -- There the program is GHCi's, with no binary of its own for a worker to
  -- load, so the jobs run on the page.
  HA.endPreviousRun
  dispose <- mount
  HA.keepForNextRun dispose

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = main
#endif
