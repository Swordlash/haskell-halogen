{-# LANGUAGE CPP #-}

module Main where

import Example.Vanilla (Query)
import Halogen (HalogenSocket (..))
import Halogen.IO.Util qualified as HA
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Example.Vanilla (component)
import Halogen.VDom.Driver (hydrateUI)
import Web.DOM.ParentNode (QuerySelector (..))
#endif

-- | The component monad is the browser backend itself. An application with
-- effects of its own would stack them on it — @ReaderT Config BrowserDOM@ —
-- and derive the DOM classes through.
attachComponent :: BrowserDOM (HalogenSocket Query Int BrowserDOM)
logStr :: Text -> IO ()
logStr = putStrLn

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
attachComponent = do
  HA.awaitLoad
  app <- HA.selectElement (QuerySelector "#app") >>= maybe (panic "index.html has no #app to mount the example into") pure
  -- The page arrives with the component already rendered into #app (see
  -- Prerender.hs), so that it shows before this binary has loaded. The
  -- component adopts that HTML rather than render its own over it.
  hydrateUI component () app
#else
attachComponent = panic "This module can only be run on JavaScript"
#endif

main :: IO ()
main = runBrowserDOM $ do
  -- Browser GHCi (toolchain/dev.sh) keeps the page between runs of :main,
  -- with the last run's tree still mounted: end it, and keep this one for
  -- the next run to end.
  HA.endPreviousRun
  socket@HalogenSocket {messages} <- attachComponent
  HA.keepForNextRun socket

  void $ HS.subscribe messages $ \st ->
    liftIO $ logStr $ "State changed: " <> show st

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = main
#endif
