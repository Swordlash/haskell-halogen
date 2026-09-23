{-# LANGUAGE CPP #-}

-- | Every example in one single-page app; see "Gallery".
module Main where

import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Gallery (component)
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (runBrowserDOM)
import Halogen.VDom.Driver (runUI)
#endif

main :: IO ()

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
main = runBrowserDOM $ void $ HA.awaitBody >>= runUI component ()
#else
main = putStrLn ("The examples gallery can only run in a JavaScript or wasm browser target." :: Text)
#endif

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = main
#endif
