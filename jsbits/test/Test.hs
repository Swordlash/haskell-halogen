{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import GHC.Wasm.Prim (JSVal)
import Halogen.JSBits (Safety (..), fromJSBytes, toJSBytes, wasmJS)

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

foreign import javascript unsafe "$1 instanceof Uint8Array ? $1.length : -1" uint8Length :: JSVal -> IO Int

foreign import javascript unsafe "$1[$2]" byteAt :: JSVal -> Int -> IO Int

main :: IO ()
main = do
  check "first call" . (== 2) =<< advance 2
  check "API initializes once" . (== 5) =<< advance 3
  check "true crosses the FFI" =<< identityBool True
  check "false crosses the FFI" . not =<< identityBool False
  check "JSVal result crosses the FFI" . isNull =<< nullValue
  check "API stays private" =<< privateAPI
  check "scripts have separate statement boundaries" . (== 42) =<< boundaryResult
  let everyByte = ByteString.pack [0 .. 255]
      large = ByteString.concat (replicate 4096 everyByte)
  array <- toJSBytes everyByte
  check "bytes become a Uint8Array of their length" . (== 256) =<< uint8Length array
  check "a byte keeps its value" . (== 200) =<< byteAt array 200
  check "bytes come back as they went" . (== everyByte) =<< fromJSBytes array
  check "a megabyte comes back as it went" . (== large) =<< (fromJSBytes =<< toJSBytes large)
  check "no bytes come back as none" . ByteString.null =<< (fromJSBytes =<< toJSBytes ByteString.empty)
  putStrLn "Shared jsbits FFI checks passed"
  where
    check message result = unless result (ioError (userError message))
