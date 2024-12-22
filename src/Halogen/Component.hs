module Halogen.Component where

import Data.Functor.Coyoneda (Coyoneda (..))
import Data.NT
import Data.Row (HasType, Row)
import HPrelude hiding (get)
import Halogen.Data.Slot hiding (pop)
import Halogen.Data.Slot qualified as Slot
import Halogen.HTML.Core
import Halogen.Query.HalogenM (HalogenM)
import Halogen.Query.HalogenM qualified as HM
import Halogen.Query.HalogenQ
import Halogen.VDom.Thunk hiding (hoist)
import Halogen.VDom.Thunk qualified as Thunk

data ComponentSlotBox slots m msg = forall query input output. ComponentSlotBox
  { get :: forall slot. SlotStorage slots slot -> Maybe (slot query output)
  , pop :: forall slot. SlotStorage slots slot -> Maybe (slot query output, SlotStorage slots slot)
  , set :: forall slot. slot query output -> SlotStorage slots slot -> SlotStorage slots slot
  , component :: Component query input output m
  , input :: input
  , output :: output -> Maybe msg
  }

data ComponentSlot (slots :: Row Type) m msg
  = ComponentSlot (ComponentSlotBox slots m msg)
  | ThunkSlot (Thunk (HTML (ComponentSlot slots m msg)) msg)

data ComponentSpec state query action slots input output m = ComponentSpec
  { initialState :: input -> state
  , render :: state -> HTML (ComponentSlot slots m action) action
  , eval :: HalogenQ query action input ~> HalogenM state action slots output m
  }

data Component query input output m
  = forall model msg slots.
    Component (ComponentSpec model query msg slots input output m)

-- | Constructs a ComponentSlot
-- |
-- | Takes:
-- | - the slot address label
-- | - the slot address index
-- | - the component for the slot
-- | - the input value to pass to the component
-- | - a function mapping outputs from the component to a query in the parent
componentSlot
  :: forall query input output slots m action label slot
   . (HasType label (Slot query output slot) slots)
  => (KnownSymbol label)
  => (Ord slot)
  => Proxy label
  -> slot
  -> Component query input output m
  -> input
  -> (output -> Maybe action)
  -> ComponentSlotBox slots m action
componentSlot label p comp input output =
  ComponentSlotBox
    { get = Slot.lookup label p
    , pop = Slot.pop label p
    , set = Slot.insert label p
    , component = comp
    , input = input
    , output
    }

-- | Changes the Component's `m` type. A use case for this
-- | might be to interpret some `Free` monad as `Aff` so the component can be
-- | used with `runUI`.
hoist
  :: forall query input output m m'
   . (Functor m')
  => (m ~> m')
  -> Component query input output m
  -> Component query input output m'
hoist nat (Component ComponentSpec {render, eval, initialState}) =
  Component
    $ ComponentSpec
      { initialState = initialState
      , render = first (hoistSlot nat) . render
      , eval = NT $ HM.hoist nat . runNT eval
      }

-- | The spec record that `mkEval` accepts to construct a component `eval`
-- | function.
-- |
-- | It's not a requirement to use `mkEval`, and sometimes it's preferrable
-- | to write a component `eval` function from scratch, but often `mkEval` is
-- | more convenient for common cases.
-- |
-- | See below for more details about `mkEval` and `defaultEval`.
data EvalSpec state query action slots input output m = EvalSpec
  { handleAction :: action -> HalogenM state action slots output m ()
  , handleQuery :: forall a. query a -> HalogenM state action slots output m (Maybe a)
  , receive :: input -> Maybe action
  , initialize :: Maybe action
  , finalize :: Maybe action
  }

-- | A default value for `mkEval` that will result in an `eval` that nothing at
-- | all - all incoming actions and queries will be ignored, and no receiver,
-- | initializer, or finalizer will be specified.
-- |
-- | Usually this will be used with record update syntax to override fields to
-- | specify things as needed. If a component only needs to handle actions,
-- | for instance, a usage might be something like this:
-- |
-- | ```purescript
-- | H.mkComponent
-- |   { initialState
-- |   , render
-- |   , eval: H.mkEval (H.defaultEval { handleAction = ?handleAction })
-- |   }
-- | ```
defaultEval :: forall state query action slots input output m. EvalSpec state query action slots input output m
defaultEval =
  EvalSpec
    { handleAction = const (pure ())
    , handleQuery = const (pure Nothing)
    , receive = const Nothing
    , initialize = Nothing
    , finalize = Nothing
    }

-- | Accepts an `EvalSpec` to produce an `eval` function for a component. For
-- | example:
-- |
-- | ```purescript
-- | -- use `defaultEval` and override fields selectively
-- | H.mkEval (H.defaultEval { handleAction = ?handleAction })
-- |
-- | -- or specify all the fields in the `EvalSpec`
-- | H.mkEval
-- |   { handleAction: ?handleAction
-- |   , handleQuery: ?handleQuery
-- |   , receive: ?receive
-- |   , initialize: ?initialize
-- |   , finalize: ?finalize
-- |   }
-- | ```
mkEval
  :: forall state query action slots input output m
   . EvalSpec state query action slots input output m
  -> HalogenQ query action input
    ~> HalogenM state action slots output m
mkEval args@EvalSpec {handleQuery} = NT $ \case
  Initialize a ->
    traverse_ args.handleAction args.initialize $> a
  Finalize a ->
    traverse_ args.handleAction args.finalize $> a
  Receive i a ->
    traverse_ args.handleAction (args.receive i) $> a
  Action action a ->
    args.handleAction action $> a
  Query (Coyoneda req fct) f ->
    map (maybe (f ()) req) . handleQuery $ fct

-- | Changes the ComponentSlot's `m` type.
hoistSlot
  :: forall slots m m' action
   . (Functor m')
  => (m ~> m')
  -> ComponentSlot slots m action
  -> ComponentSlot slots m' action
hoistSlot nat = \case
  ComponentSlot ComponentSlotBox {..} -> ComponentSlot $ ComponentSlotBox {component = hoist nat component, ..}
  ThunkSlot t ->
    ThunkSlot $ Thunk.hoist (first (hoistSlot nat)) t
