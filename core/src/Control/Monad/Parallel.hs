module Control.Monad.Parallel where

import HPrelude

class (Applicative (Parallel m), Monad m) => MonadParallel m where
  type Parallel m :: Type -> Type
  parallel :: m a -> Parallel m a
  sequential :: Parallel m a -> m a

instance MonadParallel IO where
  type Parallel IO = Concurrently IO
  parallel = Concurrently
  sequential = runConcurrently

parSequence :: (MonadParallel m, Traversable t) => t (m a) -> m (t a)
parSequence = sequential . traverse parallel

parSequence_ :: (MonadParallel m, Traversable t) => t (m a) -> m ()
parSequence_ = void . sequential . traverse parallel
