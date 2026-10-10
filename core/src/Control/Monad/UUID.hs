{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.UUID where

import Control.Monad.Trans
import Data.Type.Equality
import Data.UUID.Types (UUID, fromText, fromWords64)
import Halogen.JSBits (JSText, Safety (..), browserJS, fromJSText, inBrowser)
import HPrelude
import System.Random

class (Monad m) => MonadUUID m where
  generateV4 :: m UUID
  default generateV4 :: (MonadTrans t, m ~ t n, MonadUUID n) => m UUID
  generateV4 = lift generateV4

$(browserJS ["jsbits/web_browser.js"]
  [ ("js_crypto_random_uuid", "js_crypto_random_uuid", Unsafe, [t| IO JSText |])
  ])

-- | The browser's own generator in a browser, the random package natively.
instance MonadUUID IO where
  generateV4
    | inBrowser = fromMaybe (panic "Failed to generate UUID") . fromText . fromJSText <$> js_crypto_random_uuid
    | otherwise = fromWords64 <$> randomIO <*> randomIO
