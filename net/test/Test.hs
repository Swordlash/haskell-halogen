module Main (main) where

-- Nothing from core is used, but on the JavaScript backend its polyfills
-- (jsbits) are what lets hspec start under Node at all.
import Data.Foreign ()
import Test.Hspec (hspec)
import Test.Request qualified

main :: IO ()
main = hspec Test.Request.spec
