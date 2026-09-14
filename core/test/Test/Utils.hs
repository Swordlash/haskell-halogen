module Test.Utils (assertEqual, assertWith) where

import Control.Exception (AssertionFailed (..), throwIO)
import Control.Monad (unless)
import Prelude

assertWith :: String -> Bool -> IO ()
assertWith message condition =
  unless condition $ throwIO $ AssertionFailed message

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual message expected actual =
  assertWith
    (message <> ": expected " <> show expected <> ", got " <> show actual)
    (actual == expected)
