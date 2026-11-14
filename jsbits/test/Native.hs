{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Natively, the fields "Halogen.JSBits.Fields" writes ask a value that
-- answers for itself ('answering'), and give every other value their inert
-- answers, as before.
module Main (main) where

import Control.Monad (unless)
import Data.Dynamic (fromDynamic)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen.JSBits (Inert, JSVal, NativeObject (..), answering, jsValText, toJSInt)
import Halogen.JSBits.Fields (Member (..), jsCast, jsFields)
import Halogen.JSBits.Native (JSVal (JSVal))

newtype Thing = Thing JSVal
  deriving newtype (Inert)

newtype Other = Other JSVal
  deriving newtype (Inert)

$( jsFields
     ''Thing
     [ Method "poke" [t|Int -> Text -> IO ()|]
     , Method "count" [t|IO Int|]
     , Writable "label" ''Text
     , Fixed "other" ''Other
     , Fixed "size" ''Int
     ]
 )

$(jsCast "toOther" ''Thing ''Other)

main :: IO ()
main = do
  asked <- newIORef []
  let note what = atomicModifyIORef' asked (\as -> (as <> [what], ()))
      object =
        NativeObject
          { call = \name arguments -> note (name <> " " <> Text.unwords (map shown arguments))
          , set = \name value -> note (name <> " = " <> jsValText value)
          , get = \name -> if name == "other" then Just (toJSInt 2) else Nothing
          , instanceOf = (== "Other")
          }
      shown value = let t = jsValText value in if Text.null t then "a value" else t
      answers = Thing (answering object (toJSInt 1))
      plain = Thing (toJSInt 1)

  answers.poke 3 "x"
  answers.label_set "hello"
  readIORef asked >>= check "a call and an assignment are asked of the value" . (== ["poke a value x", "label = hello"])
  check "a method with a result is not asked" . (== 0) =<< answers.count
  check "a read is not asked" . Text.null =<< answers.label
  check "nor is a fixed member that is not an object" (answers.size == 0)
  check "a fixed object is the value's to give" (isTwo answers.other)
  check "a cast asks the value" (maybe False (const True) (toOther answers))

  plain.poke 3 "x"
  plain.label_set "hello"
  readIORef asked >>= check "a value that does not answer is asked nothing" . (== 2) . length
  check "and its fixed object is the inert one" (not (isTwo plain.other))
  check "and it is no cast" (maybe True (const False) (toOther plain))
  putStrLn "Native answers checked"
  where
    isTwo (Other value) = heldInt value == Just 2
    check message result = unless result (ioError (userError message))

-- | The number a value holds, if it holds one.
heldInt :: JSVal -> Maybe Int
heldInt (JSVal held) = fromDynamic held
