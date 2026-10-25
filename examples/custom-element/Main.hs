{-# LANGUAGE CPP #-}

-- | Defines @\<x-counter>@ and @\<x-greeting>@, and nothing else: there is no
-- Halogen app on this page. The page's HTML has the elements already, and
-- its own JavaScript drives them (web/index.html).
module Main where

import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Example.CustomElement (counter, counterDefinition, greeting, greetingDefinition)
import Halogen.CustomElement (Registration (..))
import Halogen.CustomElement qualified as CE
#endif

-- | Define the elements, and give back how to take them back.
defineElements :: BrowserDOM (BrowserDOM ())

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
defineElements = do
  counters <- CE.define "x-counter" counterDefinition counter
  greetings <- CE.define "x-greeting" greetingDefinition greeting
  pure (counters.dispose >> greetings.dispose)
#else
defineElements = panic "This example runs in a browser: build it with the JavaScript or wasm backend."
#endif

main :: IO ()
main = runBrowserDOM $ do
  -- Browser GHCi (toolchain/dev.sh) keeps the page between runs of :main:
  -- end the last run's elements, define them again (which swaps the
  -- component of every element on the page) and keep how to end this run.
  HA.endPreviousRun
  dispose <- defineElements
  HA.keepForNextRun dispose

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = main
#endif
