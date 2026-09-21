-- | The hooks from
-- <https://github.com/JordanMartinez/purescript-halogen-hooks-extra purescript-halogen-hooks-extra>,
-- ported alongside the library itself rather than as a package of their own.
--
-- None of them is primitive: each is written with the hooks in "Halogen.Hooks"
-- and nothing else, and each is worth reading as an example of a composite
-- hook — its type is a synonym listing the hooks it is made of.
module Halogen.Hooks.Extra.Hooks
  ( module Halogen.Hooks.Extra.Hooks.UseDebouncer
  , module Halogen.Hooks.Extra.Hooks.UseEvent
  , module Halogen.Hooks.Extra.Hooks.UseGet
  , module Halogen.Hooks.Extra.Hooks.UseStateFn
  , module Halogen.Hooks.Extra.Hooks.UseThrottle
  )
where

import Halogen.Hooks.Extra.Hooks.UseDebouncer
import Halogen.Hooks.Extra.Hooks.UseEvent
import Halogen.Hooks.Extra.Hooks.UseGet
import Halogen.Hooks.Extra.Hooks.UseStateFn
import Halogen.Hooks.Extra.Hooks.UseThrottle
