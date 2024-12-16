{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.Parallel where

import Protolude

newtype (~>) m n = NT (forall a. m a -> n a) 

runNT :: (m ~> n) -> m a -> n a
runNT (NT f) = f

class (Applicative f, Monad m) => Parallel f m | m -> f, f -> m where
  parallel :: m a -> f a
  sequential :: f a -> m a

instance Parallel Concurrently IO where
  parallel = Concurrently
  sequential = runConcurrently

parSequence :: (Parallel f m, Traversable t) => t (m a) -> m (t a)
parSequence = sequential . traverse parallel

parSequence_ :: (Parallel f m, Traversable t) => t (m a) -> m ()
parSequence_ = void . sequential . traverse parallel