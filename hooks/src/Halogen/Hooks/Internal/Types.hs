{-# LANGUAGE RoleAnnotations #-}

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
module Halogen.Hooks.Internal.Types
  ( HookK (..)
  , StateId (..)
  , AtMostOneQuery
  )
where

import Data.IORef (IORef)
import Data.Kind (Constraint, Type)
import GHC.TypeError (ErrorMessage (..), TypeError)

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
--
-- @scope@ is the component the state belongs to. A hook program is written for
-- every scope, so the one it is run at is a type no other component can name,
-- and a handle cannot be passed out of the component that owns it — raised as
-- an output, stored in a ref that outlives it — any more than an 'Data.STRef.STRef'
-- can escape 'Control.Monad.ST.runST'. Writing to a handle whose owner has
-- been finalized, or from a component that is not its owner, would change one
-- component's state while marking another's for re-rendering.
type StateId :: Type -> Type -> Type

type role StateId nominal representational

newtype StateId scope s = StateId (IORef s)

-- | Holds of a hook program that installs at most one query handler.
--
-- A program can list 'UseQuery' as often as it likes and the type checker has
-- no reason to mind, but only the last handler installed is the one a query
-- reaches, so a second one is a handler that silently does nothing — a
-- composite hook answering queries, say, dropped into a component that answers
-- them too. 'Halogen.Hooks.component' asks for this so that it is a type error
-- instead.
type AtMostOneQuery :: [HookK] -> Constraint
type family AtMostOneQuery hooks where
  AtMostOneQuery '[] = ()
  AtMostOneQuery (UseQuery : rest) = NoFurtherQuery rest
  AtMostOneQuery (_hook : rest) = AtMostOneQuery rest

type NoFurtherQuery :: [HookK] -> Constraint
type family NoFurtherQuery hooks where
  NoFurtherQuery '[] = ()
  NoFurtherQuery (UseQuery : _rest) =
    TypeError
      ( Text "This hook program installs more than one query handler."
          :$$: Text "Only the last useQuery in a program is the one queries reach,"
          :$$: Text "so the others would answer nothing. Keep one handler, and let"
          :$$: Text "it answer the whole of the component's query algebra."
      )
  NoFurtherQuery (_hook : rest) = NoFurtherQuery rest
