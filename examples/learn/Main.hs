{-# LANGUAGE CPP #-}

-- | The tutorial; see "Learn.App".
module Main where

import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Halogen (HalogenSocket (..))
import Halogen.IO.Util qualified as HA
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (runBrowserDOM)
import Halogen.VDom.Driver (hydrateUI)
import Learn.App (component)
import Learn.Route (Route (..), route)
import Web.DOM.ParentNode (QuerySelector (..))
#endif

main :: IO ()

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
main = runBrowserDOM $ do
  -- Browser GHCi (toolchain/dev.sh) keeps the page between runs of :main,
  -- with the last run's tree still mounted: end it, and keep this one for
  -- the next run to end.
  HA.endPreviousRun
  HA.awaitLoad
  app <- HA.selectElement (QuerySelector "#app") >>= maybe (panic "index.html has no #app to mount the tutorial into") pure
  -- The page arrives with the route's page already rendered into #app (see
  -- Prerender.hs), which the program adopts.
  base <- Router.pageBase
  router <- Router.newRouter base Home route
  socket <- hydrateUI component router app
  HA.keepForNextRun (socket.dispose >> Router.stop router)
#else
main = putStrLn ("The tutorial runs in a browser; its pages are written by halogen-example-learn-prerender." :: Text)
#endif

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = main
#endif
