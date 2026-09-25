module Main (main) where

import Prelude
import Test.Hspec (hspec)
import Test.Order qualified as Order
import Test.Player qualified as Player

main :: IO ()
main = hspec $ do
  Order.spec
  Player.spec
