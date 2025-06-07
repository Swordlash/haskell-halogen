{-# LANGUAGE MagicHash #-}

{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Data.Foreign.JS (module Data.Foreign.JS) where

#if defined(javascript_HOST_ARCH)

import HPrelude
import GHC.Base (Int#, isTrue#)
import GHC.JS.Prim

type Foreign tag = JSVal

newtype Nullable tag = Nullable (Foreign tag)

foreign import javascript unsafe "(($1) => { return (!!$1 ? 1 : 0); })" foreignToBool' :: Foreign tag -> Int#

nullableToMaybe :: Nullable tag -> Maybe (Foreign tag)
nullableToMaybe (Nullable o) = if isNull o || isUndefined o then Nothing else Just o

toForeign :: a -> Foreign tag
toForeign = unsafeCoerce

unsafeFromForeign :: Foreign tag -> a
unsafeFromForeign = unsafeCoerce

readProp :: Text -> (Foreign tag -> Maybe a) -> Foreign tag' -> Maybe a
readProp key f o = nullableToMaybe (Nullable $ unsafeGetProp o (toS key)) >>= f

foreignToString :: Foreign tag -> Text
foreignToString = toS . fromJSString

foreignToInt :: Foreign tag -> Int
foreignToInt = fromJSInt

foreignToBool :: Foreign tag -> Bool
foreignToBool x = isTrue# (foreignToBool' x)
#endif