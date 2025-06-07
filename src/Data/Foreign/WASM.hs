{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module Data.Foreign.WASM (module Data.Foreign.WASM) where

#if defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim
import HPrelude

type Foreign a = JSVal
newtype Nullable tag = Nullable (Foreign tag)

foreign import javascript unsafe "(!!$1 ? 1 : 0)" foreignToBool :: Foreign tag -> Bool

foreign import javascript unsafe "$1 === null" isNull :: Foreign tag -> Bool
foreign import javascript unsafe "$1 === undefined" isUndefined :: Foreign tag -> Bool
foreign import javascript unsafe "$1[$2]" unsafeGetProp :: JSVal -> JSString -> JSVal
foreign import javascript unsafe "$1" foreignToInt :: Foreign tag -> Int

nullableToMaybe :: Nullable tag -> Maybe (Foreign tag)
nullableToMaybe (Nullable o) = if isNull o || isUndefined o then Nothing else Just o

toForeign :: a -> Foreign tag
toForeign = unsafeCoerce

unsafeFromForeign :: Foreign tag -> a
unsafeFromForeign = unsafeCoerce

readProp :: Text -> (Foreign tag -> Maybe a) -> Foreign tag' -> Maybe a
readProp key f o = nullableToMaybe (Nullable $ unsafeGetProp o (toJSString $ toS key)) >>= f

foreignToString :: Foreign tag -> Text
foreignToString = toS . fromJSString . JSString

foreign import javascript unsafe "$1" toJSInt :: Int -> JSVal
#endif