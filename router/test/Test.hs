module Main (main) where

import Protolude
import Test.Duplex qualified as Duplex
import Test.Generic qualified as Generic
import Test.Hspec (hspec)
import Test.Router qualified as Router

main :: IO ()
main = hspec $ do
  Duplex.spec
  Generic.spec
  Router.spec
