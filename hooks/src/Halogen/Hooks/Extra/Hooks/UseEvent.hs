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
-- Beware of loops: a handler that causes another push will not stop.
module Halogen.Hooks.Extra.Hooks.UseEvent
  ( UseEvent
  , EventApi (..)
  , useEvent
  )
where

import Data.IORef (readIORef, writeIORef)
import Halogen.Hooks (Hook, HookK (..), HookM)
import Halogen.Hooks qualified as Hooks
import Protolude

-- | A handler for pushed values. Its first argument is the program that
-- removes it again, so a handler can stop listening from inside itself.
type Callback slots output m a = HookM slots output m () -> a -> HookM slots output m ()

-- | What 'useEvent' hands back: one end for the hook, one for its caller.
data EventApi slots output m a = EventApi
  { push :: a -> HookM slots output m ()
  -- ^ Raise a value. Does nothing until a handler has been set.
  , setCallback :: Callback slots output m a -> HookM slots output m (HookM slots output m ())
  -- ^ Install the handler, replacing any previous one. Returns the program
  -- that removes it — the same one the handler itself is given.
  }

-- | The hooks 'useEvent' uses: a ref holding the handler, if one is set.
type UseEvent slots output m a hooks = UseRef (Maybe (Callback slots output m a)) : hooks

-- | A place to push values from inside a hook to a handler outside it.
useEvent
  :: forall a q slots output m hooks
   . (MonadIO m)
  => Hook q slots output m (UseEvent slots output m a hooks) hooks (EventApi slots output m a)
useEvent = Hooks.do
  (_, callback) <- Hooks.useRef Nothing

  Hooks.pure
    EventApi
      { push = \a -> do
          handler <- liftIO $ readIORef callback
          for_ handler $ \h -> h (clear callback) a
      , setCallback = \h -> do
          liftIO $ writeIORef callback (Just h)
          pure (clear callback)
      }
  where
    clear ref = liftIO $ writeIORef ref Nothing
