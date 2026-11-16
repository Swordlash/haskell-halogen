{-# LANGUAGE DerivingVia #-}

module Control.Monad.State.Delayed.Trans where

import Control.Monad.State.Delayed.Class
import Control.Monad.State.Delayed.Delayer
import Control.Monad.Trans
import Data.Foreign (unsafeRefEq)
import HPrelude
import Halogen.Query.HalogenM (HalogenM, asyncWith)

type DelayedStateT :: Type -> (Type -> Type) -> Type -> Type
newtype DelayedStateT s m a = DelayedStateT (Delayer s -> m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO) via ReaderT (Delayer s) m

runDelayedStateT :: Delayer s -> DelayedStateT s m a -> m a
runDelayedStateT d (DelayedStateT f) = f d

instance MonadTrans (DelayedStateT s) where
  lift = DelayedStateT . const

-- | The state of a component whose writes wait out the delay. It works in
-- 'HalogenM' only. Each read and each write is an 'asyncWith': the program
-- suspends while it takes the delayer's 'MVar'. The timer that writes the
-- delayed state holds the same 'MVar' while it runs @put@ on the loop, so
-- taking it in place could wait for the timer while the timer waits for
-- the loop.
instance (Functor m) => MonadState s (DelayedStateT s (HalogenM s action slots output m)) where
  state f = DelayedStateT $ \(Delayer timeout var) -> asyncWith $ \run -> do
    let handleStateChange newState = do
          newDelState <- mkTimedOutDelayerState timeout newState $ \s ->
            modifyMVar var $ const $ run (put s) $> (Nothing, ())
          pure (Just newDelState)

    modifyMVar var $ \ds'm -> case ds'm of
      -- no previous suspended state, check if f changes the state
      -- if it does, create a new suspended state
      Nothing -> do
        baseState <- run get
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
            cancelWith fiber TimeoutKilled
            nvar <- handleStateChange newState
            pure (nvar, a)

instance (Functor m) => MonadDelayedState s (DelayedStateT s (HalogenM s action slots output m)) where
  commit = DelayedStateT $ \(Delayer _ var) -> asyncWith $ \run -> withMVar var $ \case
    Nothing -> pass
    Just (DelayerState s fiber) -> do
      cancelWith fiber TimeoutKilled
      run (put s)
