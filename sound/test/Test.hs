module Main (main) where

-- Nothing from core is used, but on the javascript backend its polyfills
-- (jsbits) are what lets hspec start under Node at all.
import Data.Foreign ()
import Prelude
import Test.Hspec (hspec)
import Test.Order qualified as Order
import Test.Player qualified as Player

main :: IO ()
main = hspec $ do
  Order.spec
  Player.spec
