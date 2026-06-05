module Main (main) where

import Prelude

import Test.Hspec

import Test.GHCJS qualified as GHCJS
import Test.Native qualified as Native

main :: IO ()
main = hspec $ do
  Native.spec
  GHCJS.spec
