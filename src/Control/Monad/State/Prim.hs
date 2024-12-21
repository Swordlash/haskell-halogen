{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FieldSelectors #-}

module Control.Monad.State.Prim where

import Control.Exception.Safe
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Primitive
import Protolude hiding (StateT (..), evalStateT, execStateT)
import UnliftIO (MonadUnliftIO (..))

newtype StateT s m a = StateT {runStateT' :: MutVar (PrimState m) s -> m a}
  deriving
    (Functor, Applicative, Monad, MonadThrow, MonadCatch, PrimMonad, MonadIO, MonadUnliftIO, MonadError e, MonadFail)
    via ReaderT (MutVar (PrimState m) s) m

instance MonadTrans (StateT s) where
  lift = StateT . const

instance (PrimMonad m) => MonadState s (StateT s m) where
  state f = StateT $ flip atomicModifyMutVar' f'
    where
      f' x = let (!x', !v') = f x in (v', x')

instance (MonadReader r m) => MonadReader r (StateT s m) where
  ask = lift ask
  local f m = StateT $ local f . runStateT' m

runStateT :: (PrimMonad m) => StateT s m a -> s -> m (a, s)
runStateT (StateT f) s = do
  var <- newMutVar s
  val <- f var
  (val,) <$> readMutVar var

evalStateT :: (PrimMonad m) => StateT s m a -> s -> m a
evalStateT m s = fst <$> runStateT m s

execStateT :: (PrimMonad m) => StateT s m a -> s -> m s
execStateT m s = snd <$> runStateT m s

-- | Inject "normal" state transformer into our StateT. Not atomic.
stateM :: (PrimMonad m) => (s -> m (a, s)) -> StateT s m a
stateM f = do
  s <- get
  (a, s') <- lift $ f s
  put s'
  pure a
