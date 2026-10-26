module Main (main) where

import Protolude
import Test.Connect qualified as Connect
import Test.Hspec (hspec)
import Test.Monad qualified as Monad
import Test.Native qualified as Native
import Test.Select qualified as Select

main :: IO ()
main = hspec $ do
  Monad.spec
  Select.spec
  Connect.spec
  Native.spec
