module Control.Monad.Fork where

import Control.Exception.Safe
import HPrelude

class (Monad m, Functor (Fork m)) => MonadFork m where
  type Fork m :: Type -> Type

  -- suspend :: m a -> m (f a)
  fork :: m a -> m (Fork m a)
  join :: Fork m a -> m a

class (MonadFork m, MonadThrow m) => MonadKill m where
  kill :: (Exception e) => e -> Fork m a -> m ()

instance MonadFork IO where
  type Fork IO = Async
  fork = async
  join = wait

instance MonadKill IO where
  kill = flip cancelWith
