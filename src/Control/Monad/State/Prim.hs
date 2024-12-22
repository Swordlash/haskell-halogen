module Control.Monad.State.Prim where

import Control.Exception.Safe
import HPrelude hiding (StateT (..), evalStateT, execStateT)

type StateT :: Type -> (Type -> Type) -> Type -> Type
newtype StateT s m a = StateT (IORef s -> m a)
  deriving
    (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadIO, MonadUnliftIO, MonadError e, MonadFail)
    via ReaderT (IORef s) m

instance MonadTrans (StateT s) where
  lift = StateT . const

instance (MonadIO m) => MonadState s (StateT s m) where
  state f = StateT $ flip atomicModifyIORef' f'
    where
      f' x = let (!x', !v') = f x in (v', x')

instance (MonadReader r m) => MonadReader r (StateT s m) where
  ask = lift ask
  local f (StateT m) = StateT $ local f . m

runStateT :: (MonadIO m) => StateT s m a -> s -> m (a, s)
runStateT (StateT f) s = do
  var <- newIORef s
  val <- f var
  (val,) <$> readIORef var

evalStateT :: (MonadIO m) => StateT s m a -> s -> m a
evalStateT m s = fst <$> runStateT m s

execStateT :: (MonadIO m) => StateT s m a -> s -> m s
execStateT m s = snd <$> runStateT m s

-- | Inject "normal" state transformer into our StateT. Not atomic.
stateM :: (MonadIO m) => (s -> m (a, s)) -> StateT s m a
stateM f = do
  s <- get
  (a, s') <- lift $ f s
  put s'
  pure a
