{-# LANGUAGE QualifiedDo #-}

-- | What a value was on the render before this one.
--
-- Useful for the things a render can only know by comparison: which direction
-- a number moved, whether a selection changed, whether to animate.
module Halogen.Hooks.Extra.Hooks.UsePrevious
  ( UsePrevious
  , usePrevious
  )
where

import Data.IORef (writeIORef)
import Halogen.Hooks qualified as Hooks
import Halogen.Hooks.Types (Hook, HookK (..))
import Protolude

-- | The hooks 'usePrevious' uses: a ref holding the previous value, and the
-- effect that moves this render's value into it once the render is over.
type UsePrevious a hooks = UseRef (Maybe a) : UseEffect () : hooks

-- | The value this hook was called with on the previous render, or 'Nothing'
-- on the first one.
usePrevious
  :: forall a scope q slots output m hooks
   . (MonadIO m)
  => a
  -> Hook scope q slots output m (UsePrevious a hooks) hooks (Maybe a)
usePrevious value = Hooks.do
  (previous, ref) <- Hooks.useRef Nothing

  -- Dependencies that are never the same: an effect that runs after every
  -- render, which is what makes "previous" mean the render before this one
  -- rather than the last one that changed something.
  Hooks.useTickEffectBy (\_ _ -> False) () $ do
    liftIO $ writeIORef ref (Just value)
    pure Nothing

  Hooks.pure previous
