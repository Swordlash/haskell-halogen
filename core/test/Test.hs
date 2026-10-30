{-# LANGUAGE CPP #-}

module Main (main) where

import Prelude

#if !defined(HALOGEN_WASI) && (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
import Test.GHCJS qualified as GHCJS
#endif
import Test.Canvas qualified as Canvas
import Test.CanvasMachine qualified as CanvasMachine
import Test.Cookie qualified as Cookie
import Test.DriverContract qualified as DriverContract
import Test.DriverLifecycleRaces qualified as DriverLifecycleRaces
import Test.DriverReentrancy qualified as DriverReentrancy
import Test.Fork qualified as Fork
import Test.HTML qualified as HTML
import Test.Hashable qualified as Hashable
import Test.Hspec (hspec)
import Test.Hydrate qualified as Hydrate
import Test.IndexedDB qualified as IndexedDB
import Test.IndexedRows qualified as IndexedRows
import Test.NativeDom qualified as NativeDom
import Test.NativeEvents qualified as NativeEvents
import Test.Refs qualified as Refs
import Test.RenderRecovery qualified as RenderRecovery
import Test.Runtime qualified as Runtime
import Test.Static qualified as Static
import Test.Storage qualified as Storage
import Test.SvgAttributes qualified as SvgAttributes
import Test.Trace qualified as Trace
import Test.WindowEvents qualified as WindowEvents

main :: IO ()
main = hspec $ do
#if !defined(HALOGEN_WASI) && (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
  GHCJS.spec
#endif
  Canvas.spec
  CanvasMachine.spec
  Cookie.spec
  DriverContract.spec
  DriverReentrancy.spec
  DriverLifecycleRaces.spec
  Fork.spec
  Hashable.spec
  HTML.spec
  Hydrate.spec
  IndexedDB.spec
  IndexedRows.spec
  NativeDom.spec
  NativeEvents.spec
  Refs.spec
  RenderRecovery.spec
  Runtime.spec
  Static.spec
  Storage.spec
  SvgAttributes.spec
  Trace.spec
  WindowEvents.spec
