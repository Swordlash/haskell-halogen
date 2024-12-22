{-# LANGUAGE DerivingVia #-}

module Control.Monad.State.Delayed.Trans where

import Control.Monad.State.Delayed.Class
import Control.Monad.State.Delayed.Delayer
import Control.Monad.Trans
import Data.Foreign (unsafeRefEq)
import HPrelude

type DelayedStateT :: Type -> (Type -> Type) -> Type -> Type
newtype DelayedStateT s m a = DelayedStateT (Delayer s -> m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO) via ReaderT (Delayer s) m

instance MonadTrans (DelayedStateT s) where
  lift = DelayedStateT . const

instance (MonadUnliftIO m, MonadState s m) => MonadState s (DelayedStateT s m) where
  state f = DelayedStateT $ \(Delayer timeout var) -> do
    let handleStateChange newState = do
          newDelState <- withRunInIO
            $ \runInIO -> mkTimedOutDelayerState timeout newState
              $ \s -> modifyMVar var $ const $ runInIO (put s) $> (Nothing, ())
          pure (Just newDelState)

    modifyMVar var $ \ds'm -> case ds'm of
      -- no previous suspended state, check if f changes the state
      -- if it does, create a new suspended state
      Nothing -> do
        baseState <- get
        let (a, newState) = f baseState
        if unsafeRefEq baseState newState
          then pure (Nothing, a)
          else do
            nvar <- handleStateChange newState
            pure (nvar, a)

      -- there is a previous suspended state
      -- check if f changes the state, if it does, create a new suspended state and cancel the previous one
      Just (DelayerState delayedState fiber) -> do
        let (a, newState) = f delayedState

        if unsafeRefEq delayedState newState
          then pure (ds'm, a)
          else do
            liftIO $ cancelWith fiber TimeoutKilled
            nvar <- handleStateChange newState
            pure (nvar, a)

instance (MonadUnliftIO m, MonadState s m) => MonadDelayedState s (DelayedStateT s m) where
  commit = DelayedStateT $ \(Delayer _ var) -> withMVar var $ \case
    Nothing -> pass
    Just (DelayerState s fiber) -> do
      liftIO $ cancelWith fiber TimeoutKilled
      put s
