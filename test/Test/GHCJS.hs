-- | Minimal JavaScript-backend test runner. Keep this module and its Cabal
-- component free of test-framework dependencies so their JavaScript stubs are
-- not required at runtime.
module Test.GHCJS (test) where

import Control.Exception (AssertionFailed (..), throwIO)
import Data.Foreign (Foreign, foreignToBool)
import Prelude

foreign import javascript unsafe "(() => { return true; })" js_true :: Foreign Bool

foreign import javascript unsafe "(() => { return false; })" js_false :: Foreign Bool

assertWith :: String -> Bool -> IO ()
assertWith message condition =
  if condition
    then pure ()
    else throwIO (AssertionFailed message)

test :: IO ()
test = do
  assertWith "foreignToBool should convert true to True" (foreignToBool js_true)
  assertWith "foreignToBool should convert false to False" (not (foreignToBool js_false))
