{-# LANGUAGE CPP #-}

module Main where

import Example.Vanilla (Query)
import Halogen (HalogenSocket (..))
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Example.Vanilla (component)
import Halogen.IO.Util as HA
import Halogen.VDom.Driver (runUI)
#endif

-- | The component monad is the browser backend itself. An application with
-- effects of its own would stack them on it — @ReaderT Config BrowserDOM@ —
-- and derive the DOM classes through.
attachComponent :: BrowserDOM (HalogenSocket Query Int BrowserDOM)
logStr :: Text -> IO ()
logStr = putStrLn

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
attachComponent = HA.awaitBody >>= runUI component ()
#else
attachComponent = panic "This module can only be run on JavaScript"
#endif

main :: IO ()
main = runBrowserDOM $ do
  HalogenSocket {messages} <- attachComponent

  void $ HS.subscribe messages $ \st ->
    liftIO $ logStr $ "State changed: " <> show st

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = main
#endif

{-
forever $ do
  threadDelay 5_000_000
  void $ query (IncrementQ ())
  threadDelay 5_000_000
  void $ query (DecrementQ ())
  -}
