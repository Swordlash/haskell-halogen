{-# LANGUAGE CPP #-}

module Main (main) where

import Prelude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Test.GHCJS qualified as GHCJS
#endif
import Test.Canvas qualified as Canvas
import Test.Cookie qualified as Cookie
import Test.DriverReentrancy qualified as DriverReentrancy
import Test.Fork qualified as Fork
import Test.Hspec (hspec)
import Test.NativeDom qualified as NativeDom
import Test.SvgAttributes qualified as SvgAttributes

main :: IO ()
main = hspec $ do
#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
  GHCJS.spec
#endif
  Canvas.spec
  Cookie.spec
  DriverReentrancy.spec
  Fork.spec
  NativeDom.spec
  SvgAttributes.spec
