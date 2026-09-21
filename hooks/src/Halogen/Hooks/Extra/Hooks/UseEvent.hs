{-# LANGUAGE QualifiedDo #-}

-- | An event a hook can raise and its user can handle.
--
-- 'Halogen.Hooks.raise' sends a message to the component's parent, which is no
-- use to a hook: a hook is part of the component, and what it wants to tell is
-- whoever wrote the component. This gives it somewhere to push values, and the
-- caller somewhere to hook a handler on.
--
-- A handler can do everything any other handler can, including setting up
-- something in its first run and tearing it down again, since it is handed the
-- program that removes it.
--
-- The channel is a 'HS.Subscribe' opened when the component mounts, so a value
-- pushed before the first render has nowhere to go and is dropped, and the
-- handler runs as a component action of its own rather than inside the push.
-- Beware of loops: a handler that causes another push will not stop.
module Halogen.Hooks.Extra.Hooks.UseEvent
  ( UseEvent
  , EventApi (..)
  , useEvent
  )
where

import Data.IORef (IORef, readIORef, writeIORef)
import Halogen.Hooks qualified as Hooks
import Halogen.Hooks.Types (Hook, HookK (..), HookM)
import Halogen.Query.HalogenM (SubscriptionId)
import Halogen.Subscription qualified as HS
import Protolude

-- | What 'useEvent' hands back: one end for the hook, one for its caller.
data EventApi scope slots output m a = EventApi
  { push :: a -> HookM scope slots output m ()
  -- ^ Raise a value. Does nothing until a handler has been set.
  , setCallback
      :: (HookM scope slots output m () -> a -> HookM scope slots output m ())
      -> HookM scope slots output m (HookM scope slots output m ())
  -- ^ Install the handler, replacing any previous one. Returns the program
  -- that removes it — the same one the handler itself is given as its first
  -- argument, so that a handler can stop listening from inside itself.
  }

-- | The hooks 'useEvent' uses: the channel, whatever is listening on it, and
-- the effect that opens the channel.
--
-- None of them mentions the scope, which is what keeps a component that uses
-- this hook an ordinary one: a cell whose type named the scope could not be
-- part of a hook list, since the list is what a component's type is fixed by
-- before the scope exists.
type UseEvent a hooks =
  UseRef (Maybe (HS.Subscribe IO a))
    : UseRef (Maybe SubscriptionId)
    : UseEffect ()
    : hooks

-- | A place to push values from inside a hook to a handler outside it.
useEvent
  :: forall a scope q slots output m hooks
   . (MonadIO m)
  => Hook scope q slots output m (UseEvent a hooks) hooks (EventApi scope slots output m a)
useEvent = Hooks.do
  (_, channel) <- Hooks.useRef Nothing
  (_, listening) <- Hooks.useRef Nothing

  Hooks.useLifecycleEffect $ do
    liftIO $ writeIORef channel . Just =<< HS.create
    pure $ Just $ liftIO $ writeIORef channel Nothing

  Hooks.pure
    EventApi
      { push = \a -> do
          open <- liftIO $ readIORef channel
          for_ open $ \c -> liftIO $ HS.notify c.listener a
      , setCallback = \handler -> do
          remove listening
          open <- liftIO $ readIORef channel
          for_ open $ \c -> do
            sid <- Hooks.subscribe $ map (handler (remove listening)) c.emitter
            liftIO $ writeIORef listening (Just sid)
          pure (remove listening)
      }
  where
    remove :: IORef (Maybe SubscriptionId) -> HookM scope slots output m ()
    remove listening = do
      listener <- liftIO $ readIORef listening
      for_ listener Hooks.unsubscribe
      liftIO $ writeIORef listening Nothing
