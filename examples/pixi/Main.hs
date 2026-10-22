{-# LANGUAGE CPP #-}

module Main where

import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Example.Pixi (parent)
import Halogen (HalogenSocket (..))
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad qualified as DOM
import Halogen.VDom.Driver (runUI)
#endif

main :: IO ()

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
main = DOM.runBrowserDOM $ do
  -- Browser GHCi (toolchain/dev.sh) keeps the page between runs of :main,
  -- with the last run's tree still mounted: end it, and keep this one for
  -- the next run to end.
  HA.endPreviousRun
  socket <- runUI parent () =<< HA.awaitBody
  HA.keepForNextRun socket.dispose
#else
main = putStrLn ("The Pixi example can only run in a JavaScript or wasm browser target." :: Text)
#endif

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = main
#endif
