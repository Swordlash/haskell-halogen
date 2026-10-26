module Main (main) where

import Protolude
import Test.Demo qualified as Demo
import Test.Dyld qualified as Dyld
import Test.Hspec (hspec)
import Test.Lazy qualified as Lazy

main :: IO ()
main = hspec $ do
  Lazy.spec
  Dyld.spec
  Demo.spec
