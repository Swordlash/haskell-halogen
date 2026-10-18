module Main (main) where

import Test.Clay qualified
import Test.Haskell qualified
import Test.Hspec
import Test.Html qualified
import Test.Property qualified

main :: IO ()
main = hspec $ do
  describe "hh" Test.Html.spec
  describe "clay" Test.Clay.spec
  describe "Haskell" Test.Haskell.spec
  describe "properties" Test.Property.spec
