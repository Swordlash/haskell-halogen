module Main (main) where

import Control.Exception
import Data.Foreign
import Prelude

-- import Test.Hspec

foreign import javascript unsafe "(() => { return true; })" js_true :: Foreign Bool

foreign import javascript unsafe "(() => { return false; })" js_false :: Foreign Bool

assertWith :: String -> Bool -> IO ()
assertWith msg b = if b then pure () else throwIO $ AssertionFailed msg

main :: IO ()
main = do
  assertWith "should convert true to true" $ foreignToBool js_true
  assertWith "should convert false to false" $ not $ foreignToBool js_false
