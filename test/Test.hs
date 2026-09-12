{-# LANGUAGE CPP #-}

module Main (main) where

import Prelude

#if defined(javascript_HOST_ARCH)
import Test.GHCJS qualified as GHCJS
#endif
import Test.DriverReentrancy qualified as DriverReentrancy
import Test.SvgAttributes qualified as SvgAttributes
import Test.Utils (runTests)

main :: IO ()
main = runTests $ ghcjsTests <> commonTests

ghcjsTests :: [(String, IO ())]
#if defined(javascript_HOST_ARCH)
ghcjsTests = [("GHCJS FFI", GHCJS.test)]
#else
ghcjsTests = []
#endif

commonTests :: [(String, IO ())]
commonTests =
  [ ("driver re-entrancy", DriverReentrancy.test)
  , ("SVG attributes", SvgAttributes.test)
  ]
