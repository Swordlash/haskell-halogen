module Main (main) where

import Halogen (HalogenSocket, VoidF)
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Example.Quotes (component)
import Halogen.VDom.Driver (runUI)
#endif

attach :: BrowserDOM (HalogenSocket VoidF () BrowserDOM)
#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
attach = HA.awaitBody >>= runUI component ()
#else
attach = panic "This program runs in a browser only"
#endif

main :: IO ()
main = runBrowserDOM $ do
  -- Browser GHCi (toolchain/dev.sh) keeps the page between runs of :main,
  -- with the last run's tree still mounted: end it, and keep this one for
  -- the next run to end.
  HA.endPreviousRun
  HA.keepForNextRun =<< attach

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = main
#endif
