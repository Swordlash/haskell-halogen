module Main (main) where

import Protolude
import Test.Extra qualified as Extra
import Test.Hooks qualified as Hooks
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  Hooks.spec
  Extra.spec
