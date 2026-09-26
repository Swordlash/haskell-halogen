{-# LANGUAGE QualifiedDo #-}

-- | Reading a value that a handler was not rendered with.
--
-- A handler closes over the values of the render that produced it. That is
-- almost always what you want, and for state there is 'Halogen.Hooks.get',
-- which reads the cell rather than the closure. For everything else — the
-- component's input, or a value derived from it — a program that outlives the
-- render it was written in will see a stale value.
--
-- 'useGet' keeps the current value in a ref that is refreshed every time it
-- changes, and hands back a program that reads it. The typical caller is a
-- subscription handler or a forked program: created once, run much later.
module Halogen.Hooks.Extra.Hooks.UseGet
  ( UseGet
  , useGet
  )
where

import Data.IORef (readIORef, writeIORef)
import Halogen.Hooks qualified as Hooks
import Halogen.Hooks.Internal.HookM (effectIO)
import Halogen.Hooks.Types (Hook, HookK (..), HookM)
import Protolude

-- | The hooks 'useGet' uses: a ref holding the value, and the effect that
-- keeps it current.
type UseGet a hooks = UseRef a : UseEffect a : hooks

-- | A program that reads the latest value this hook was called with.
useGet
  :: forall a scope q slots output m hooks
   . (Eq a, MonadIO m)
  => a
  -> Hook scope q slots output m (UseGet a hooks) hooks (HookM scope slots output m a)
useGet value = Hooks.do
  (_, ref) <- Hooks.useRef value

  Hooks.useTickEffect value $ do
    effectIO $ writeIORef ref value
    pure Nothing

  Hooks.pure $ effectIO $ readIORef ref
