{-# LANGUAGE QualifiedDo #-}

-- | Act on the first value, then at most once per period after it.
--
-- Where a debouncer waits for quiet, a throttle keeps going but at a limited
-- rate: the action runs immediately, and any values that arrive during the
-- period are dropped except the last, which runs when the period ends.
module Halogen.Hooks.Extra.Hooks.UseThrottle
  ( UseThrottle
  , useThrottle
  )
where

import Data.IORef (IORef, atomicModifyIORef')
import Data.Time (NominalDiffTime)
import Halogen.Hooks (Hook, HookK (..), HookM)
import Halogen.Hooks qualified as Hooks
import Halogen.Hooks.Extra.Internal.Delay (delayFor)
import Protolude

-- | What the throttle is doing: nothing, or holding a period open with at most
-- one value waiting for it to end.
type Throttling a = Maybe (Maybe a)

-- | The hooks 'useThrottle' uses: a ref holding what it is doing.
type UseThrottle a hooks = UseRef (Throttling a) : hooks

-- | Wrap an action so that it runs at most once per period.
--
-- The waiting is a component fork, so it is killed along with the component.
useThrottle
  :: forall a q slots output m hooks
   . (MonadIO m)
  => NominalDiffTime
  -- ^ the shortest gap between two runs
  -> (a -> HookM slots output m ())
  -> Hook q slots output m (UseThrottle a hooks) hooks (a -> HookM slots output m ())
useThrottle period act = Hooks.do
  (_, throttle) <- Hooks.useRef Nothing

  Hooks.pure $ \a -> do
    busy <- liftIO $ atomicModifyIORef' throttle $ \case
      -- Idle: this value goes through, and the period starts.
      Nothing -> (Just Nothing, False)
      -- Mid-period: this value waits, replacing whatever was waiting.
      Just _ -> (Just (Just a), True)
    unless busy $ do
      act a
      void $ Hooks.fork $ drain throttle
  where
    drain :: IORef (Throttling a) -> HookM slots output m ()
    drain throttle = do
      delayFor period
      waiting <- liftIO $ atomicModifyIORef' throttle $ \case
        Just (Just a) -> (Just Nothing, Just a)
        _ -> (Nothing, Nothing)
      -- A value arrived during the period: run it, and keep the period open.
      for_ waiting $ \a -> act a *> drain throttle
