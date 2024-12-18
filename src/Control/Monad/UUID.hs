module Control.Monad.UUID where

import Control.Monad.Trans
import Data.UUID.Types (UUID, fromWords64)
import Protolude
import Data.Type.Equality
import System.Random

class (Monad m) => MonadUUID m where
  generateV4 :: m UUID
  default generateV4 :: (MonadTrans t, m ~ t n, MonadUUID n) => m UUID
  generateV4 = lift generateV4

instance MonadUUID IO where
  generateV4 = fromWords64 <$> randomIO <*> randomIO
