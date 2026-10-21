{-# LANGUAGE CPP #-}

-- | Every example in one single-page app; see "Gallery".
module Main where

import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Gallery (component)
import Gallery.Shell (Route (..), route)
import Halogen.IO.Util qualified as HA
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (runBrowserDOM, setTextContent)
import Halogen.VDom.Driver (runUI)
import Web.DOM.Internal.Types (toNode)
import Web.DOM.ParentNode (QuerySelector (..))
#endif

main :: IO ()

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
main = runBrowserDOM $ do
  HA.awaitLoad
  app <- HA.selectElement (QuerySelector "#app") >>= maybe (panic "index.html has no #app to mount the gallery into") pure
  -- The page arrives with the gallery's shell already rendered into #app
  -- (see Prerender.hs). It is replaced rather than adopted: the gallery
  -- renders afresh in its place.
  setTextContent "" (toNode app)
  base <- Router.pageBase
  router <- Router.newRouter base Home route
  void $ runUI component router app
#else
main = putStrLn ("The examples gallery can only run in a JavaScript or wasm browser target; its pages are written by halogen-example-all-prerender." :: Text)
#endif

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = main
#endif
