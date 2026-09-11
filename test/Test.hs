{-# LANGUAGE CPP #-}

module Main (main) where

import Prelude

#if defined(javascript_HOST_ARCH)
import Test.GHCJS qualified as GHCJS
#endif
import Test.DriverReentrancy qualified as DriverReentrancy

main :: IO ()
main = do
#if defined(javascript_HOST_ARCH)
  GHCJS.test
#endif
  DriverReentrancy.test
