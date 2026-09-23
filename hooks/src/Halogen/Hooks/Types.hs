-- | The types a hook signature is written in, without the operations.
--
-- "Halogen.Hooks" exports all of this as well; this module is here for the
-- places that only need the vocabulary — a composite hook's type synonym, a
-- written-out signature — and would rather not have @pure@ and @>>=@ in scope.
--
-- 'Hook', 'HookM' and 'StateId' are abstract. Their constructors are the
-- interpreter's business: forging a 'StateId' would let a program write state
-- the interpreter does not know about, and a hook program built by hand could
-- be one the cell store cannot be walked against. Use the hooks and the
-- operations in "Halogen.Hooks".
module Halogen.Hooks.Types
  ( -- * The vocabulary of hook lists
    HookK (..)
  , AtMostOneQuery

    -- * Programs
  , Hook
  , HookFn
  , HookHTML

    -- * Handlers
  , HookM
  , HookAction
  , StateId
  )
where

import Halogen.Hooks.Internal.Hook (Hook, HookFn)
import Halogen.Hooks.Internal.HookM (HookAction, HookHTML, HookM)
import Halogen.Hooks.Internal.Types (AtMostOneQuery, HookK (..), StateId)
