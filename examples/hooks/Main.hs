{-# LANGUAGE CPP #-}

module Main where

import Example.Hooks (Output (..))
import Halogen qualified as H
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Example.Hooks (app)
import Halogen.IO.Util qualified as HA
import Halogen.VDom.Driver (runUI)
#endif

attachComponent :: BrowserDOM (H.HalogenSocket H.VoidF Output BrowserDOM)

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
attachComponent = HA.awaitBody >>= runUI app ()
#else
attachComponent = panic "This example runs in a browser: build it with the JavaScript or wasm backend."
#endif

main :: IO ()
main = runBrowserDOM $ do
  H.HalogenSocket {messages} <- attachComponent
  void $ HS.subscribe messages $ \(Counted n) ->
    liftIO $ putStrLn ("[output] the counter reached " <> show n :: Text)

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = main
#endif
