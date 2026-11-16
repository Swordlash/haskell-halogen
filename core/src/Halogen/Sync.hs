-- | Code that cannot suspend.
--
-- A component's program can leave its tree's loop in two ways.
-- 'Halogen.Query.HalogenM.sync' runs an action in place, and
-- 'Halogen.Query.HalogenM.async' suspends the program while other events
-- run. Code that carries out a step's effects must not suspend in the
-- middle: the effects after the suspension would run after later events,
-- on a state the step never saw.
--
-- Write such code against 'MonadSync' instead of 'Halogen.HalogenM'. It
-- does not know its monad, so it cannot call @lift@, @liftIO@, @async@ or
-- a query that waits for its answer. The compiler checks that it does not
-- suspend.
--
-- > import Halogen.Sync qualified as Sync
-- >
-- > perform :: (Sync.MonadSync State Action Slots Output m h) => Effect -> h ()
-- > perform = \case
-- >   Notify message -> Sync.raise (Notified message)
-- >   Save -> Sync.send "storage" () Storage.Save
-- >   Focus -> Sync.getRef inputRef >>= traverse_ (Sync.sync . focus)
--
-- A handler calls it as it calls any other program: @perform effect@ in
-- 'Halogen.HalogenM' is one. 'Control.Monad.State.Class.get',
-- 'Control.Monad.State.Class.put' and 'Control.Monad.State.Class.modify'
-- work too.
--
-- GHC may suggest adding @MonadIO h@ to the context of such code. Do not:
-- that lets it suspend again. Run an action in place with 'sync' instead.
--
-- The names here are "Halogen"'s, so import this module qualified.
module Halogen.Sync
  ( MonadSync (sync, raise, subscribe, unsubscribe, getRef)
  , send
  )
where

import Data.Row (HasType, Row)
import HPrelude hiding (async, catch, finally, try)
import Halogen.Data.Slot (Slot)
import Halogen.Query qualified as H
import Halogen.Subscription qualified as HS
import Web.DOM.Element (Element)

-- | A monad that runs a component's program and cannot suspend. Its only
-- instance is 'Halogen.HalogenM': @h@ is
-- @HalogenM st act slots out m@, and the other parameters follow from it.
type MonadSync :: Type -> Type -> Row Type -> Type -> (Type -> Type) -> (Type -> Type) -> Constraint
class (MonadState st h, Functor m) => MonadSync st act slots out m h | h -> st act slots out m where
  -- | Run an action of the component's monad in place, as
  -- 'Halogen.Query.HalogenM.sync'. It must not wait.
  sync :: m a -> h a

  -- | Raise an output, as 'Halogen.Query.HalogenM.raise'.
  raise :: out -> h ()

  -- | Subscribe to an emitter, as 'Halogen.Query.HalogenM.subscribe'.
  subscribe :: HS.Emitter IO act -> h H.SubscriptionId

  -- | End a subscription, as 'Halogen.Query.HalogenM.unsubscribe'.
  unsubscribe :: H.SubscriptionId -> h ()

  -- | The element rendered with this ref, as 'Halogen.Query.HalogenM.getRef'.
  getRef :: H.RefLabel -> h (Maybe Element)

  -- | Start a program of the component that the caller does not wait for.
  -- Not exported: 'send' is the only use.
  start :: H.HalogenM st act slots out m () -> h ()

instance (Functor m) => MonadSync st act slots out m (H.HalogenM st act slots out m) where
  sync = H.sync
  raise = H.raise
  subscribe = H.subscribe
  unsubscribe = H.unsubscribe
  getRef = H.getRef
  start = void . H.fork

-- | Tell a child something, and go on without waiting for it, as
-- 'Halogen.Query.send'. When it returns, the child's handler has run up to
-- its first suspension, or to its end. Two 'send's reach the child in
-- order.
send
  :: forall label
  ->forall st act slots out m h query output' slot
   . (MonadSync st act slots out m h, HasType label (Slot query output' slot) slots)
  => (KnownSymbol label)
  => (Ord slot)
  => slot
  -> H.Tell query
  -> h ()
send label slot req = start (H.tell label slot req)
