{-# LANGUAGE CPP #-}

-- | Every example in one single-page app; see "Gallery".
module Main where

import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Gallery (component)
import Gallery.Shell (Route (..), route)
import Halogen (HalogenSocket (..))
import Halogen.IO.Util qualified as HA
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (runBrowserDOM)
import Halogen.VDom.Driver (hydrateUI)
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
  app <- HA.selectElement (QuerySelector "#app") >>= maybe (panic "index.html has no #app to mount the gallery into") pure
  -- The page arrives with the gallery's shell already rendered into #app
  -- (see Prerender.hs). The gallery adopts it, and builds the example its
  -- route shows into the place the shell leaves for it.
  base <- Router.pageBase
  router <- Router.newRouter base Home route
  socket <- hydrateUI component router app
  -- The router's popstate listener is the window's, not the tree's, so it
  -- is stopped with the tree.
  HA.keepForNextRun (socket.dispose >> Router.stop router)
#else
main = putStrLn ("The examples gallery can only run in a JavaScript or wasm browser target; its pages are written by halogen-example-all-prerender." :: Text)
#endif

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = main
#endif
