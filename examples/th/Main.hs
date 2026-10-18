module Main (main) where

import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Example.Quotes (component)
import Halogen.IO.Util as HA
import Halogen.VDom.Driver (runUI)
#endif

attach :: BrowserDOM ()
#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
attach = HA.awaitBody >>= void . runUI component ()
#else
attach = panic "This program runs in a browser only"
#endif

main :: IO ()
main = runBrowserDOM attach

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = main
#endif
