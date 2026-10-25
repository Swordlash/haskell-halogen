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

deriving instance Functor (ComponentSlotBox slots' m)

data ComponentSlot (slots :: Row Type) m msg
  = ComponentSlot (ComponentSlotBox slots m msg)
  | ThunkSlot (Thunk (HTML (ComponentSlot slots m msg)) msg)

instance Functor (ComponentSlot slots' m) where
  fmap f = \case
    ComponentSlot box -> ComponentSlot $ map f box
    ThunkSlot t -> ThunkSlot $ mapThunkBy f (bimap (fmap f) f) t

data ComponentSpec' state query action slots input output m n = ComponentSpec
  { initialState :: input -> m state
  , render :: state -> HTML (ComponentSlot slots m action) action
  , eval :: HalogenQ query action input ~> n
  }

type ComponentSpec state query action slots input output m =
  ComponentSpec' state query action slots input output m (HalogenM state action slots output m)

data Component query input output m
  = forall model msg slots.
    Component (ComponentSpec model query msg slots input output m)

mkComponent :: forall state query action slots input output m. ComponentSpec state query action slots input output m -> Component query input output m
mkComponent = Component

-- | Constructs a ComponentSlot
-- |
-- | Takes:
-- | - the slot address label
-- | - the slot address index
-- | - the component for the slot
-- | - the input value to pass to the component
-- | - a function mapping outputs from the component to a query in the parent
componentSlot
  :: forall label
  ->forall query input output slots m action slot
   . (HasType label (Slot query output slot) slots)
  => (KnownSymbol label)
  => (Ord slot)
  => slot
  -> Component query input output m
  -> input
  -> (output -> Maybe action)
  -> ComponentSlotBox slots m action
componentSlot label p component input output =
  ComponentSlotBox
    { get = Slot.lookup label p
    , pop = Slot.pop label p
    , set = Slot.insert label p
    , component
    , input
    , output
    }

-- | Changes the Component's `m` type. A use case for this
-- | might be to interpret some `Free` monad as `IO` so the component can be
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
      { initialState = runNT nat . initialState
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

-- | A spec with the state and the render given and nothing handled yet: it
-- ignores every action and query, and nothing happens as the component
-- arrives, goes or is given new input. The handlers are added with
-- 'onAction', 'onQuery', 'onInitialize', 'onFinalize' and 'onReceive', in a
-- chain of @&@ ending in 'mkComponent', or as the list 'Halogen.component'
-- takes, which is the usual way.
componentSpec
  :: forall state query action slots input output m
   . (input -> m state)
  -> (state -> HTML (ComponentSlot slots m action) action)
  -> ComponentSpec state query action slots input output m
componentSpec initialState render = ComponentSpec {initialState, render, eval = mkEval defaultEval}

-- | A change to a spec's handlers, what 'onAction' and the other @on@
-- functions give. Each answers one of the five things a component is asked
-- to do and leaves the spec's answers to the other four as they were, so
-- the order handlers are applied in does not matter, and of two for the
-- same thing the one applied last wins.
type Handler state query action slots input output m =
  ComponentSpec state query action slots input output m -> ComponentSpec state query action slots input output m

-- | The spec's @eval@ with one case answered anew: @f@ is given the old
-- @eval@ for the cases it leaves alone.
answering
  :: forall state query action slots input output m
   . ( forall a
        . (HalogenQ query action input ~> HalogenM state action slots output m)
       -> HalogenQ query action input a
       -> HalogenM state action slots output m a
     )
  -> Handler state query action slots input output m
answering f spec = ComponentSpec {initialState = spec.initialState, render = spec.render, eval = NT (f spec.eval)}

-- | What the component does with an action: one its HTML raised (an
-- @HE.onClick@), one it sent itself from a subscription or a fork, or one a
-- parent's query asked for. Without it, actions are ignored.
onAction
  :: forall state query action slots input output m
   . (action -> HalogenM state action slots output m ())
  -> Handler state query action slots input output m
onAction act = answering $ \eval -> \case
  Action action a -> act action $> a
  q -> runNT eval q

-- | How the component answers a parent's query: @Nothing@ leaves the query
-- unanswered (a 'Halogen.Query.request' then gives @Nothing@). Without it,
-- every query is unanswered.
onQuery
  :: forall state query action slots input output m
   . (forall a. query a -> HalogenM state action slots output m (Maybe a))
  -> Handler state query action slots input output m
onQuery answer = answering $ \eval -> \case
  Query (Coyoneda req fct) f -> maybe (f ()) req <$> answer fct
  q -> runNT eval q

-- | What the component does once it is on the page, rendered for the first
-- time (see 'Halogen.Query.HalogenQ.Initialize' for what waiting there
-- holds up, and what it does not). A component that keeps it as an action
-- passes @handleAction Initialize@.
onInitialize
  :: forall state query action slots input output m
   . HalogenM state action slots output m ()
  -> Handler state query action slots input output m
onInitialize run = answering $ \eval -> \case
  Initialize a -> run $> a
  q -> runNT eval q

-- | What the component does as it is taken away: unsubscribing is done for
-- it, so this is for what else it holds.
onFinalize
  :: forall state query action slots input output m
   . HalogenM state action slots output m ()
  -> Handler state query action slots input output m
onFinalize run = answering $ \eval -> \case
  Finalize a -> run $> a
  q -> runNT eval q

-- | What the component does when its parent renders it with new input: the
-- input is not put into the state for it, so a component that follows its
-- input stores it here. Without it, new input is ignored.
onReceive
  :: forall state query action slots input output m
   . (input -> HalogenM state action slots output m ())
  -> Handler state query action slots input output m
onReceive receiveInput = answering $ \eval -> \case
  Receive i a -> receiveInput i $> a
  q -> runNT eval q

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
    ThunkSlot $ Thunk.mapThunkBy nat (first (hoistSlot nat)) t
