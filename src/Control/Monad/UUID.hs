{-# LANGUAGE CPP #-}

module Control.Monad.UUID where

import Control.Monad.Trans
import Data.Type.Equality
import HPrelude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

#if defined(javascript_HOST_ARCH)
import GHC.JS.Prim
#else
import GHC.Wasm.Prim
#endif


import Data.UUID.Types (UUID, fromText)
#else
import System.Random
import Data.UUID.Types (UUID, fromWords64)
#endif

class (Monad m) => MonadUUID m where
  generateV4 :: m UUID
  default generateV4 :: (MonadTrans t, m ~ t n, MonadUUID n) => m UUID
  generateV4 = lift generateV4

#if defined(javascript_HOST_ARCH)

foreign import javascript unsafe "js_crypto_random_uuid" js_crypto_random_uuid :: IO JSVal
instance MonadUUID IO where
  generateV4 = fromMaybe (panic "Failed to generate UUID") . fromText . toS . coerce . fromJSString <$> js_crypto_random_uuid

#elif defined(wasm32_HOST_ARCH)

foreign import javascript unsafe "js_crypto_random_uuid" js_crypto_random_uuid :: IO JSString
instance MonadUUID IO where
  generateV4 = fromMaybe (panic "Failed to generate UUID") . fromText . toS . fromJSString <$> js_crypto_random_uuid

#else
instance MonadUUID IO where
  generateV4 = fromWords64 <$> randomIO <*> randomIO
#endif
