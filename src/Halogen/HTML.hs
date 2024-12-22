module Halogen.HTML
  ( ComponentHTML
  , PlainHTML
  , fromPlainHTML
  , slot
  , slot_
  , memoized
  , lazy
  -- , lazy2
  -- , lazy3
  , module Halogen.HTML.Core
  , module Halogen.HTML.Elements
  , module Halogen.HTML.Properties
  )
where

{- attrNS, -}

import Data.Foreign
import Data.Row
import HPrelude
import Halogen.Component
import Halogen.Data.Slot (Slot)
import Halogen.HTML.Core (AttrName (..), ClassName (..), ElemName (..), HTML (..), IsProp (..), Namespace (..), PropName (..), handler, text)
import Halogen.HTML.Core qualified as Core
import Halogen.HTML.Elements
import Halogen.HTML.Properties (IProp, attr, prop)
import Halogen.VDom.Thunk

-- | A convenience synonym for the output type of a `render` function for a
-- | component that renders HTML.
-- |
-- | - `action` is the type of actions, events internal to the component that can
-- |   be evaluated with the `handleAction` function
-- | - `slots` is the set of child component types that can be used in the HTML
-- | - `m` is the monad used by the child component during evaluation
type ComponentHTML action slots m = HTML (ComponentSlot slots m action) action

-- | A type useful for a chunk of HTML with no slot-embedding or query-raising.
-- |
-- | Often a polymorphic usage of `HTML` is good enough for this, but sometimes
-- | it's useful to have a type like this (and accompanying coercion) when doing
-- | things like creating components that accept a chunk of HTML as part of
-- | their configuration.
type PlainHTML = HTML Void Void

-- | Relaxes the type of `PlainHTML` to make it compatible with all `HTML`.
fromPlainHTML :: forall w i. PlainHTML -> HTML w i
fromPlainHTML = bimap absurd absurd

-- | Defines a slot for a child component. Takes:
-- | - the slot address label
-- | - the slot address index
-- | - the component for the slot
-- | - the input value to pass to the component
-- | - a function mapping outputs from the component to a query in the parent
slot
  :: forall query action input output slots m label slot
   . (HasType label (Slot query output slot) slots)
  => (KnownSymbol label)
  => (Ord slot)
  => Proxy label
  -> slot
  -> Component query input output m
  -> input
  -> (output -> action)
  -> ComponentHTML action slots m
slot label p component input outputQuery =
  Core.widget (ComponentSlot (componentSlot label p component input (Just . outputQuery)))

-- | Defines a slot for a child component, ignoring its output.
-- |
-- | This variant may be used when the component produces output, but it is not
-- | needed in the current context, or instead of passing `absurd` to `slot`
-- | when the output type is `Void`.
-- |
-- | Takes:
-- | - the slot address label
-- | - the slot address index
-- | - the component for the slot
-- | - the input value to pass to the component
slot_
  :: forall query action input output slots m label slot
   . (HasType label (Slot query output slot) slots)
  => (KnownSymbol label)
  => (Ord slot)
  => Proxy label
  -> slot
  -> Component query input output m
  -> input
  -> ComponentHTML action slots m
slot_ label p component input =
  Core.widget (ComponentSlot (componentSlot label p component input (const Nothing)))

-- | Optimizes rendering of a subtree given an equality predicate. If an argument
-- | is deemed equivalent to the previous value, rendering and diffing will be
-- | skipped. You should not use this function fully saturated, but instead
-- | partially apply it for use within a Component's scope. For example, to skip
-- | rendering for equal states, just wrap your `render` function.
-- |
-- | ```purescript
-- | myComponent = component
-- |  { render: memoized eq render
-- |  , ...
-- |  }
-- | ```
memoized
  :: forall a action slots m
   . (a -> a -> Bool)
  -> (a -> ComponentHTML action slots m)
  -> a
  -> ComponentHTML action slots m
memoized eqFn f =
  -- Note: This implementation must not be eta-expanded, as it relies on
  -- partial application to work.
  Core.widget . ThunkSlot <$> Thunk (unsafeThunkId f) eqFn f

-- | Skips rendering for referentially equal arguments. You should not use this
-- | function fully saturated, but instead partially apply it for use within a
-- | Component's scope.
lazy
  :: forall a action slots m
   . (a -> ComponentHTML action slots m)
  -> a
  -> ComponentHTML action slots m
lazy = memoized unsafeRefEq

{-
-- | Like `lazy`, but for a rendering function which takes 2 arguments.
lazy2
  :: forall a b action slots m
   . (a -> b -> ComponentHTML action slots m)
  -> a
  -> b
  -> ComponentHTML action slots m
lazy2 f a b = Core.widget (ThunkSlot (Fn.runFn3 thunk2 f a b))

-- | Like `lazy`, but for a rendering function which takes 3 arguments.
lazy3
  :: forall a b c action slots m
   . (a -> b -> c -> ComponentHTML action slots m)
  -> a
  -> b
  -> c
  -> ComponentHTML action slots m
lazy3 f a b c = Core.widget (ThunkSlot (Fn.runFn4 thunk3 f a b c))
-}
