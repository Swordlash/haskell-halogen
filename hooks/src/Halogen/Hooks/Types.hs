-- | The vocabulary a hook program is indexed by, and the handle a hook hands
-- back for the state it owns.
--
-- The PureScript original tracks the hooks a program uses as a chain of
-- newtypes wrapped around a type variable, because PureScript has no
-- parameterised type synonyms to abbreviate one with. Here the chain is an
-- ordinary type-level list of 'HookK', which reads in the order the hooks are
-- written and lets a composite hook be named with a plain type synonym:
--
-- @
-- type UseCounter hooks = UseState Int : UseEffect Int : hooks
-- @
module Halogen.Hooks.Types
  ( HookK (..)
  , StateId (..)
  )
where

import Data.IORef (IORef)
import Data.Kind (Type)

-- | What a single hook contributes to the type of a hook program.
--
-- The payloads are the types the interpreter has to keep consistent between
-- renders: the type of the state a cell holds, of the dependencies an effect
-- or a memo is compared on, of the value a ref or a memo produces. They are
-- what makes the cell store in "Halogen.Hooks.Internal.Cells" typed, and so
-- what removes the @unsafeCoerce@ the PureScript interpreter needs to read a
-- cell back out of its array.
type data HookK
  = -- | A piece of component-local state of the given type.
    UseState Type
  | -- | An effect re-run whenever a value of the given type changes.
    UseEffect Type
  | -- | A value of the second type, recomputed when the first changes.
    UseMemo Type Type
  | -- | A mutable reference to a value of the given type.
    UseRef Type
  | -- | A handler for the component's query algebra.
    UseQuery

-- | A handle to the state a 'Halogen.Hooks.useState' call owns.
--
-- Handlers read and write state through this rather than closing over the
-- value they were rendered with, which is how a handler installed on one
-- render observes what a later render has done.
newtype StateId s = StateId (IORef s)
