{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.Fork where

import Protolude
import Control.Exception.Safe

class (Monad m, Functor f) => MonadFork f m | m -> f where
  --suspend :: m a -> m (f a)
  fork :: m a -> m (f a)
  join :: f a -> m a

class (MonadFork f m, MonadThrow m) => MonadKill f m | m -> f where
  kill :: Exception e => e -> f a -> m ()

instance MonadFork Async IO where
  fork = async
  join = wait

instance MonadKill Async IO where
  kill = flip cancelWith