module Control.Monad.UUID where

import Control.Monad.Trans
import Data.UUID (UUID)
import Data.UUID.V4
import Protolude

class Monad m => MonadUUID m where
  generateV4 :: m UUID
  default generateV4 :: (MonadTrans t, m ~ t n, MonadUUID n) => m UUID
  generateV4 = lift generateV4

instance MonadUUID IO where
  generateV4 = nextRandom
