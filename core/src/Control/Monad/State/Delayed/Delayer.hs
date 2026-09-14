{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Control.Monad.State.Delayed.Delayer where

import Data.Time
import HPrelude

data TimeoutKilled = TimeoutKilled
  deriving stock (Show)
  deriving anyclass (Exception)

data Delayer s = Delayer NominalDiffTime (MVar (Maybe (DelayerState s)))

data DelayerState s = DelayerState
  { delayedState :: s
  , commitFiber :: Async ()
  }

mkEmptyDelayer :: (MonadIO m) => NominalDiffTime -> m (Delayer s)
mkEmptyDelayer delay = do
  s <- liftIO $ newMVar Nothing
  pure $ Delayer delay s

mkTimedOutDelayerState :: NominalDiffTime -> s -> (s -> IO ()) -> IO (DelayerState s)
mkTimedOutDelayerState delay s commit = do
  fiber <- liftIO $ async $ liftIO (threadDelay $ floor $ realToFrac delay * 10 ^ 6) *> commit s
  pure $ DelayerState s fiber
