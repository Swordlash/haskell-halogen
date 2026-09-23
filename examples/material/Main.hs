-- Fourmolu rewrites the CPP block inside 'main' below into invalid code.
{- FOURMOLU_DISABLE -}
module Main (main) where

import Halogen (HalogenSocket, VoidF)
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Example.Material (component)
import Halogen.IO.Util as HA
import Halogen.VDom.Driver (runUI)
#endif

attachComponent :: BrowserDOM (HalogenSocket VoidF () BrowserDOM)
#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
attachComponent = HA.awaitBody >>= runUI component ()
#else
attachComponent = panic "This module can only be run in a browser"
#endif

main :: IO ()
main = do
#if defined(INTERACTIVE)
  clearHotReloadTarget
#endif
  runBrowserDOM $ void attachComponent

#if defined(WASM)
foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = main
#endif

#if defined(INTERACTIVE)
foreign import javascript unsafe "document.querySelectorAll('body > :not(script)').forEach((node) => node.remove())" clearHotReloadTarget :: IO ()
#endif
