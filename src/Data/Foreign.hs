{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module Data.Foreign where

#if !defined(javascript_HOST_ARCH)
import GHC.Base (Int (..), reallyUnsafePtrEquality, reallyUnsafePtrEquality#)
#endif

import HPrelude
import Unsafe.Coerce (unsafeCoerce)

#if defined(javascript_HOST_ARCH)
import GHC.JS.Prim
type Foreign tag = JSVal

newtype Nullable tag = Nullable (Foreign tag)

nullableToMaybe :: Nullable tag -> Maybe (Foreign tag)
nullableToMaybe (Nullable o) = if isNull o then Nothing else Just o

toForeign :: a -> Foreign tag
toForeign = unsafeCoerce

unsafeFromForeign :: Foreign tag -> a
unsafeFromForeign = unsafeCoerce

foreign import javascript unsafe "js_unsafe_ref_eq" js_unsafe_ref_eq :: JSVal -> JSVal -> Bool

unsafeRefEq' :: a -> b -> Bool
unsafeRefEq' a b = js_unsafe_ref_eq (unsafeCoerce a) (unsafeCoerce b)

unsafeRefEq :: a -> a -> Bool
unsafeRefEq a b = unsafeRefEq' a b

#else

newtype Foreign tag = Foreign Any

type Nullable tag = Maybe (Foreign tag)

nullableToMaybe :: Nullable tag -> Maybe (Foreign tag)
nullableToMaybe = identity

toForeign :: a -> Foreign tag
toForeign = Foreign . unsafeCoerce

unsafeFromForeign :: Foreign tag -> a
unsafeFromForeign (Foreign o) = unsafeCoerce o

unsafeRefEq :: a -> a -> Bool
unsafeRefEq p q = I# (reallyUnsafePtrEquality p q) == 1

unsafeRefEq' :: a -> b -> Bool
unsafeRefEq' p q = I# (reallyUnsafePtrEquality# p q) == 1
#endif
