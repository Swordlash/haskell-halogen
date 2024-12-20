{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module Data.Foreign where

import GHC.Base (Int (..), reallyUnsafePtrEquality, reallyUnsafePtrEquality#)
import Protolude
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
#else

data Foreign tag = forall a. Foreign a

type Nullable tag = Maybe (Foreign tag)

nullableToMaybe :: Nullable tag -> Maybe (Foreign tag)
nullableToMaybe = identity

toForeign :: a -> Foreign tag
toForeign = Foreign

unsafeFromForeign :: Foreign tag -> a
unsafeFromForeign (Foreign o) = unsafeCoerce o

#endif

unsafeRefEq :: a -> a -> Bool
unsafeRefEq p q = I# (reallyUnsafePtrEquality p q) == 1

unsafeRefEqHet :: a -> b -> Bool
unsafeRefEqHet p q = I# (reallyUnsafePtrEquality# p q) == 1
