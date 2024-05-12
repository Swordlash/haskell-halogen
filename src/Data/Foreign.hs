{-# LANGUAGE CPP #-}

module Data.Foreign where

import Unsafe.Coerce (unsafeCoerce)

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
