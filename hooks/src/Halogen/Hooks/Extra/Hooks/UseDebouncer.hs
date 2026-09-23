{-# LANGUAGE QualifiedDo #-}

-- | Act only once the values stop coming.
--
-- The usual case is a text input: search when the typing pauses, not on every
-- keystroke.
--
-- This debounces an /action/. "Halogen.Component.Debounced" in
-- @haskell-halogen-core@ debounces a component's /state writes/, coalescing
-- the renders of a component that is already written; it applies to any
-- component, hooks or not. Reach for that one to slow a whole component down,
-- and for this one to delay one thing a component does.
module Halogen.Hooks.Extra.Hooks.UseDebouncer
  ( UseDebouncer
  , useDebouncer
  )
where

import Data.IORef (readIORef, writeIORef)
import Data.Time (NominalDiffTime)
import Halogen.Hooks qualified as Hooks
import Halogen.Hooks.Extra.Internal.Delay (delayFor)
import Halogen.Hooks.Types (Hook, HookK (..), HookM)
import Halogen.Query.HalogenM (ForkId)
import Protolude

-- | The hooks 'useDebouncer' uses: a ref holding the pending timer, if any.
type UseDebouncer hooks = UseRef (Maybe ForkId) : hooks

-- | Wrap an action so that it runs only when the given quiet period has passed
-- without another value arriving. The action runs with the most recent value.
--
-- The waiting is a component fork, so it is killed along with the component.
useDebouncer
  :: forall a scope q slots output m hooks
   . (MonadIO m)
  => NominalDiffTime
  -- ^ how long the values have to stop for
  -> (a -> HookM scope slots output m ())
  -> Hook scope q slots output m (UseDebouncer hooks) hooks (a -> HookM scope slots output m ())
useDebouncer quiet act = Hooks.do
  (_, pending) <- Hooks.useRef Nothing

  Hooks.pure $ \a -> do
    -- Whatever was waiting is no longer the latest value.
    traverse_ Hooks.kill =<< liftIO (readIORef pending)
    forkId <- Hooks.fork $ do
      delayFor quiet
      liftIO $ writeIORef pending Nothing
      act a
    liftIO $ writeIORef pending (Just forkId)
