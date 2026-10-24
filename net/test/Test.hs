module Main (main) where

-- On the JavaScript backend core's polyfills (jsbits) are what lets hspec
-- start under Node at all, whether or not a test uses core.
import Data.Foreign ()
import Test.Hspec (describe, hspec)
import Test.Inert qualified
import Test.Request qualified

main :: IO ()
main = hspec $ do
  describe "fetch" Test.Request.spec
  describe "sockets and event sources" Test.Inert.spec
