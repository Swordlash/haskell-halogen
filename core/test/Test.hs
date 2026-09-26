{-# LANGUAGE CPP #-}

module Main (main) where

import Prelude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Test.GHCJS qualified as GHCJS
#endif
import Test.Canvas qualified as Canvas
import Test.Cookie qualified as Cookie
import Test.DriverContract qualified as DriverContract
import Test.DriverReentrancy qualified as DriverReentrancy
import Test.DriverLifecycleRaces qualified as DriverLifecycleRaces
import Test.DriverStateAtomic qualified as DriverStateAtomic
import Test.Fork qualified as Fork
import Test.Hspec (hspec)
import Test.NativeDom qualified as NativeDom
import Test.RenderRecovery qualified as RenderRecovery
import Test.Runtime qualified as Runtime
import Test.Storage qualified as Storage
import Test.SvgAttributes qualified as SvgAttributes

main :: IO ()
main = hspec $ do
#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
  GHCJS.spec
#endif
  Canvas.spec
  Cookie.spec
  DriverContract.spec
  DriverReentrancy.spec
  DriverStateAtomic.spec
  DriverLifecycleRaces.spec
  Fork.spec
  NativeDom.spec
  RenderRecovery.spec
  Runtime.spec
  Storage.spec
  SvgAttributes.spec
