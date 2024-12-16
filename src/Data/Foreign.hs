{-# LANGUAGE CPP, MagicHash #-}

module Data.Foreign where

import Protolude
import Unsafe.Coerce (unsafeCoerce)
import GHC.Base (reallyUnsafePtrEquality, Int (..))

#if defined(javascript_HOST_ARCH)
import GHC.JS.Prim
type Foreign tag = JSVal

#else

data Foreign tag = forall a. Foreign a

toForeign :: a -> Foreign tag
toForeign = Foreign

unsafeFromForeign :: Foreign tag -> a
unsafeFromForeign (Foreign o) = unsafeCoerce o

#endif

unsafeRefEq :: a -> a -> Bool
unsafeRefEq p q = I# (reallyUnsafePtrEquality p q) == 1