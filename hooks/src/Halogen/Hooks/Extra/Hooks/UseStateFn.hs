{-# LANGUAGE QualifiedDo #-}

-- | State as a function rather than a handle.
--
-- 'Halogen.Hooks.useState' hands back a 'StateId' and leaves the caller to
-- pair it with an operation every time. When a component only ever does one
-- thing to a piece of state, this pairs them once.
module Halogen.Hooks.Extra.Hooks.UseStateFn
  ( UseStateFn
  , useStateFn
  , useModifyState
  , useModifyState_
  , usePutState
  )
where

import Halogen.Hooks (Hook, HookK (..), HookM, StateId)
import Halogen.Hooks qualified as Hooks

-- | The hooks 'useStateFn' uses: one piece of state.
type UseStateFn s hooks = UseState s : hooks

-- | A piece of state, and one of its operations already applied to it.
--
-- @
-- (name, setName) <- usePutState ""
-- @
useStateFn
  :: forall s fn q slots output m hooks
   . (StateId s -> fn)
  -> s
  -> Hook q slots output m (UseStateFn s hooks) hooks (s, fn)
useStateFn fn initial = Hooks.do
  (value, stateId) <- Hooks.useState initial
  Hooks.pure (value, fn stateId)

-- | 'useStateFn' with 'Halogen.Hooks.modify': the function returns the new value.
useModifyState
  :: forall s q slots output m hooks
   . s
  -> Hook q slots output m (UseStateFn s hooks) hooks (s, (s -> s) -> HookM slots output m s)
useModifyState = useStateFn Hooks.modify

-- | 'useStateFn' with 'Halogen.Hooks.modify_'.
useModifyState_
  :: forall s q slots output m hooks
   . s
  -> Hook q slots output m (UseStateFn s hooks) hooks (s, (s -> s) -> HookM slots output m ())
useModifyState_ = useStateFn Hooks.modify_

-- | 'useStateFn' with 'Halogen.Hooks.put'.
usePutState
  :: forall s q slots output m hooks
   . s
  -> Hook q slots output m (UseStateFn s hooks) hooks (s, s -> HookM slots output m ())
usePutState = useStateFn Hooks.put
