module Main (main) where

import Test.Hspec (hspec)
import Test.Outside qualified

main :: IO ()
main = hspec Test.Outside.spec
