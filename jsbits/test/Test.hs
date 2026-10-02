{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad (unless)
import GHC.Wasm.Prim (JSVal)
import Halogen.JSBits (Safety (..), wasmJS)

$( wasmJS
     ["test/fixture.js", "test/boundary-a.js", "test/boundary-b.js"]
     [ ("advance", "advance", Unsafe, [t|Int -> IO Int|])
     , ("identityBool", "identity", Unsafe, [t|Bool -> IO Bool|])
     , ("nullValue", "nullValue", Unsafe, [t|IO JSVal|])
     , ("privateAPI", "privateAPI", Unsafe, [t|IO Bool|])
     , ("boundaryResult", "boundaryResult", Unsafe, [t|IO Int|])
     ]
 )

foreign import javascript unsafe "$1 === null" isNull :: JSVal -> Bool

main :: IO ()
main = do
  check "first call" . (== 2) =<< advance 2
  check "API initializes once" . (== 5) =<< advance 3
  check "true crosses the FFI" =<< identityBool True
  check "false crosses the FFI" . not =<< identityBool False
  check "JSVal result crosses the FFI" . isNull =<< nullValue
  check "API stays private" =<< privateAPI
  check "scripts have separate statement boundaries" . (== 42) =<< boundaryResult
  putStrLn "Shared jsbits FFI checks passed"
  where
    check message result = unless result (ioError (userError message))
