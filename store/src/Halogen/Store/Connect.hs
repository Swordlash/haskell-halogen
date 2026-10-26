-- | Components that follow a part of the store.
module Halogen.Store.Connect
  ( Connected (..)
  , connect
  , subscribe
  )
where

import Data.Row (type (.==))
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Store.Monad (MonadStore (..))
import Halogen.Store.Select (Selector (..))
import Protolude hiding (Selector)

-- | What a connected component is given as its input: the part of the store
-- its selector picks out, and the input its parent passed.
data Connected context input = Connected
  { context :: context
  , input :: input
  }
  deriving stock (Eq, Show)

data Action context input output
  = Receive input
  | Update context
  | Raise output

type Slots query output = ("inner" .== H.Slot query output ())

-- | A component that takes its part of the store with its input, made into
-- one that takes only the input: the part is read from the store and kept
-- up to date. It is rendered again with a new 'Connected' each time its
-- part changes, as the selector's @eq@ tells, and not when the store
-- changes elsewhere; it hears of it as of any new input, in 'H.onReceive'.
-- Its output is its own, and the queries sent to the connected component
-- reach it.
--
-- purescript-halogen-store reads the store in the wrapper's @Initialize@,
-- because a PureScript component's initial state is pure, and so renders
-- nothing until then. Here the initial state is made in the component's
-- monad, so the store is read there and the component is in the first
-- render already: a page rendered ahead of the program by
-- "Halogen.Static" and adopted by 'Halogen.VDom.Driver.hydrateUI' has it
-- in the same place from the start. The subscription is made in
-- @Initialize@, after which the store is read again, for an update made
-- in between.
connect
  :: forall action store context query input output m
   . (MonadStore action store m)
  => Selector store context
  -> H.Component query (Connected context input) output m
  -> H.Component query input output m
connect selector inner =
  -- Its state holds the context and the input, of any types: asking for
  -- 'Typeable' of them would ask every connected component's for it, so the
  -- trace counts what the wrapper does without carrying its states.
  H.Component H.ComponentInfo {label = "Halogen.Store.connect", stateType = Nothing} $
    H.componentSpec initialState render
      & H.onInitialize initialize
      & H.onAction handleAction
      & H.onReceive (handleAction . Receive)
      & H.onQuery (H.query "inner" ())
  where
    initialState :: input -> m (Connected context input)
    initialState input = do
      store <- getStore
      pure Connected {context = selector.select store, input}

    render :: Connected context input -> H.ComponentHTML (Action context input output) (Slots query output) m
    render connected = HH.slot "inner" () inner connected Raise

    initialize :: H.HalogenM (Connected context input) (Action context input output) (Slots query output) output m ()
    initialize = do
      emitter <- emitSelected selector
      void $ H.subscribe (Update <$> emitter)
      store <- getStore
      handleAction (Update (selector.select store))

    handleAction :: Action context input output -> H.HalogenM (Connected context input) (Action context input output) (Slots query output) output m ()
    handleAction = \case
      Receive input -> do
        connected <- get
        put Connected {context = connected.context, input}
      Update context -> do
        connected <- get
        unless (selector.eq connected.context context) $
          put Connected {context, input = connected.input}
      Raise output -> H.raise output

-- | Subscribe a component to a part of the store: each time it changes, as
-- the selector's @eq@ tells, the action made from it is handled. Usually
-- called in the component's @Initialize@, after which the component reads
-- the store's present value itself (with 'getStore'), as the emitter
-- carries updates only.
subscribe
  :: forall storeAction store context action state slots output m
   . (MonadStore storeAction store m)
  => Selector store context
  -> (context -> action)
  -> H.HalogenM state action slots output m ()
subscribe selector toAction = do
  emitter <- emitSelected selector
  void $ H.subscribe (toAction <$> emitter)
