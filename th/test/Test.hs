module Main (main) where

import Test.Check qualified
import Test.Clay qualified
import Test.Haskell qualified
import Test.Hspec
import Test.Html qualified
import Test.Loop qualified
import Test.Overloaded qualified
import Test.Property qualified

main :: IO ()
main = hspec $ do
  describe "hh" Test.Html.spec
  describe "clay" Test.Clay.spec
  describe "for over any Foldable" Test.Loop.spec
  describe "for over a list under OverloadedLists and OverloadedStrings" Test.Overloaded.spec
  describe "Haskell" Test.Haskell.spec
  describe "properties" Test.Property.spec
  describe "check" Test.Check.spec
