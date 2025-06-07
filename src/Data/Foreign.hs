{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module Data.Foreign (module Data.Foreign, module X) where

#if defined(javascript_HOST_ARCH)
import Data.Foreign.JS as X
#elif defined(wasm32_HOST_ARCH)
import Data.Foreign.WASM as X
#else
import Data.Foreign.Stub as X
#endif

import Prelude (Bool)
import GHC.Base (isTrue#, reallyUnsafePtrEquality, reallyUnsafePtrEquality#)

unsafeRefEq :: a -> a -> Bool
unsafeRefEq p q = isTrue# (reallyUnsafePtrEquality p q)

unsafeRefEq' :: a -> b -> Bool
unsafeRefEq' p q = isTrue# (reallyUnsafePtrEquality# p q)
