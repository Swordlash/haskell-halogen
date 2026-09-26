module Halogen.Query.HalogenM where

import Control.Applicative.Free.Fast
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.Free.Church
import Control.Monad.Parallel
import Data.Map.Strict qualified as M
import Data.NT
import Data.Row
import HPrelude hiding (Ap)
import Halogen.Data.Slot as Slot
import Halogen.Query.ChildQuery qualified as CQ
import Halogen.Query.Input
import Halogen.Subscription hiding (Subscribe)
import Halogen.Subscription qualified as HS
import Web.DOM.Element

newtype SubscriptionId = SubscriptionId Int
  deriving newtype (Eq, Ord, Show)

newtype ForkId = ForkId Int
  deriving newtype (Eq, Ord, Show)

data HalogenF state action slots output m a
  = State (state -> (a, state))
  | Subscribe (SubscriptionId -> Emitter IO action) (SubscriptionId -> a)
  | Unsubscribe SubscriptionId a
  | Lift (m a)
  -- ^ Work that may wait: the component's other work goes on meanwhile.
  | LiftEffect (m a)
  -- ^ Work done at once, before anything else of the component's tree.
  | Unlift (UnliftIO (HalogenM state action slots output m) -> IO a)
  | ChildQuery (CQ.ChildQuery slots a)
  | Raise output a
  | Par (HalogenAp state action slots output m a)
  | Fork (HalogenM state action slots output m ()) (ForkId -> a)
  | Join ForkId a
  | Kill ForkId a
  | GetRef RefLabel (Maybe Element -> a)
  | Throw SomeException
  | forall x. Catch (HalogenM state action slots output m x) (SomeException -> HalogenM state action slots output m x) (x -> a)

instance (Functor m) => Functor (HalogenF state' action slots' output m) where
  fmap f = \case
    -- Strictly in the pair: the state has to come back as the pointer it
    -- was, for the driver's `unsafeRefEq` to see that it did not change.
    State g -> State (\s -> case g s of (a, s') -> (f a, s'))
    Subscribe es k -> Subscribe es (f . k)
    Unsubscribe sid a -> Unsubscribe sid (f a)
    Lift m -> Lift (fmap f m)
    LiftEffect m -> LiftEffect (fmap f m)
    Unlift q -> Unlift (fmap f . q)
    ChildQuery cq -> ChildQuery (fmap f cq)
    Raise o a -> Raise o (f a)
    Par p -> Par (fmap f p)
    Fork hm k -> Fork hm (f . k)
    Join fid a -> Join fid (f a)
    Kill fid a -> Kill fid (f a)
    GetRef l k -> GetRef l (f . k)
    Throw e -> Throw e
    Catch body handler k -> Catch body handler (f . k)

newtype HalogenM state action slots output m a
  = HalogenM (F (HalogenF state action slots output m) a)
  deriving (Functor, Applicative, Monad)

-- | 'lift' and 'liftIO' are where a component's program may wait, as
-- 'liftAff' is in purescript-halogen: the action runs on a thread of its own,
-- and until it is done the rest of the tree goes on (other events, other
-- components, this component's other programs); then the program goes on
-- where it left off. What has to happen at once, while nothing else runs --
-- @preventDefault@ in an event handler, focusing an element, reading the DOM
-- as it is now -- goes through 'liftEffect' instead.
instance MonadTrans (HalogenM state' action slots' output) where
  lift = HalogenM . liftF . Lift

instance (MonadIO m) => MonadIO (HalogenM state' action slots' output m) where
  liftIO = HalogenM . liftF . Lift . liftIO

-- | Run an action of the component's monad at once, as purescript-halogen's
-- 'liftEffect': the program does not let anything else of the tree run
-- until it is done. So it must not wait -- not on an 'MVar', a delay, a
-- JavaScript promise or a query to this tree -- or the whole tree waits
-- with it; use 'liftIO' for that.
liftEffect :: forall state action slots output m a. (Functor m) => m a -> HalogenM state action slots output m a
liftEffect = HalogenM . liftF . LiftEffect

instance (MonadUnliftIO m) => MonadUnliftIO (HalogenM state' action slots' output m) where
  withRunInIO inner =
    HalogenM $ liftF $ Unlift $ \(UnliftIO q) -> inner q

type HalogenIO state action slots output a = HalogenM state action slots output IO a

newtype HalogenAp state action slots output m a
  = HalogenAp (Ap (HalogenM state action slots output m) a)
  deriving (Functor, Applicative)

-- | A failure of a component's program: a thrown exception, or one of the
-- component's monad, synchronous or after a wait.
instance (Functor m) => MonadThrow (HalogenM state' action slots' output m) where
  throwM = HalogenM . liftF . Throw . toException

-- | Handle a failure of a program, wherever in it (and whenever) it happens.
instance (Functor m) => MonadCatch (HalogenM state' action slots' output m) where
  catch body handler = HalogenM $ liftF $ Catch body (\e -> maybe (throwM e) handler (fromException e)) identity

instance (Functor m) => MonadState state' (HalogenM state' action slots' output m) where
  state = HalogenM . liftF . State

instance (Functor m) => MonadParallel (HalogenM state' action slots' output m) where
  type Parallel (HalogenM state' action slots' output m) = HalogenAp state' action slots' output m
  parallel = HalogenAp . liftAp
  sequential = HalogenM . liftF . Par

-- | Raises an output message for the component.
raise :: forall state action slots output m. (Functor m) => output -> HalogenM state action slots output m ()
raise o = HalogenM $ liftF $ Raise o ()

-- | Sends a query to a child of a component at the specified slot.
query
  :: forall label
  ->forall state action output m slots query output' slot a
   . (HasType label (Slot query output' slot) slots)
  => (KnownSymbol label)
  => (Ord slot)
  => (Functor m)
  => slot
  -> query a
  -> HalogenM state action slots output m (Maybe a)
query label p q =
  HalogenM
    $ liftF
    $ ChildQuery
    $ CQ.ChildQuery (\k -> maybe (pure Nothing) k . Slot.lookup label p) q identity

-- | Sends a query to all children of a component at a given slot label.
queryAll
  :: forall label
  ->forall state action output m slots query output' slot a
   . (HasType label (Slot query output' slot) slots)
  => (KnownSymbol label)
  => (Ord slot)
  => (Functor m)
  => query a
  -> HalogenM state action slots output m (Map slot a)
queryAll label q =
  HalogenM
    $ liftF
    $ ChildQuery
    $ CQ.ChildQuery (\k -> map catMapMaybes . traverse k . Slot.slots label) q identity
  where
    catMapMaybes :: forall k v. (Ord k) => Map k (Maybe v) -> Map k v
    catMapMaybes = M.foldlWithKey' (\acc k v -> maybe acc (flip (M.insert k) acc) v) M.empty

-- | Subscribes a component to an `Emitter`.
-- |
-- | When a component is disposed of any active subscriptions will automatically
-- | be stopped and no further subscriptions will be possible during
-- | finalization.
subscribe :: forall state action slots output m. (Functor m) => HS.Emitter IO action -> HalogenM state action slots output m SubscriptionId
subscribe es = HalogenM $ liftF $ Subscribe (const es) identity

-- | An alternative to `subscribe`, intended for subscriptions that unsubscribe
-- | themselves. Instead of returning the `SubscriptionId` from `subscribe'`, it
-- | is passed into an `Emitter` constructor. This allows emitted queries
-- | to include the `SubscriptionId`, rather than storing it in the state of the
-- | component.
-- |
-- | When a component is disposed of any active subscriptions will automatically
-- | be stopped and no further subscriptions will be possible during
-- | finalization.
subscribe' :: forall state action slots output m. (Functor m) => (SubscriptionId -> HS.Emitter IO action) -> HalogenM state action slots output m ()
subscribe' esc = HalogenM $ liftF $ Subscribe esc (const ())

-- | Unsubscribes a component from a subscription. If the subscription associated
-- | with the ID has already ended this will have no effect.
unsubscribe :: forall state action slots output m. (Functor m) => SubscriptionId -> HalogenM state action slots output m ()
unsubscribe sid = HalogenM $ liftF $ Unsubscribe sid ()

-- | Starts a `HalogenM` process running independent from the current `eval`
-- | "thread".
-- |
-- | A commonly use case for `fork` is in component initializers where some
-- | async action is started. Normally all interaction with the component will
-- | be blocked until the initializer completes, but if the async action is
-- | `fork`ed instead, the initializer can complete synchronously while the
-- | async action continues.
-- |
-- | Some care needs to be taken when using a `fork` that can modify the
-- | component state, as it's easy for the forked process to "clobber" the state
-- | (overwrite some or all of it with an old value) by mistake.
-- |
-- | When a component is disposed of any active forks will automatically
-- | be killed. New forks can be started during finalization but there will be
-- | no means of killing them.
fork :: forall state action slots output m. (Functor m) => HalogenM state action slots output m () -> HalogenM state action slots output m ForkId
fork hmu = HalogenM $ liftF $ Fork hmu identity

-- | Joins a forked process. Attempting to join a forked process that has
-- | already ended will result in eval continuing immediately. Attempting
-- | to join a forked process that has been killed will also terminate the
-- | current eval.
join :: forall state action slots output m. (Functor m) => ForkId -> HalogenM state action slots output m ()
join fid = HalogenM $ liftF $ Join fid ()

-- | Kills a forked process if it is still running. Attempting to kill a forked
-- | process that has already ended will have no effect.
kill :: forall state action slots output m. (Functor m) => ForkId -> HalogenM state action slots output m ()
kill fid = HalogenM $ liftF $ Kill fid ()

-- | Retrieves an `Element` value that is associated with a `Ref` in the
-- | rendered output of a component. If there is no currently rendered value for
-- | the requested ref this will return `Nothing`.
getRef :: forall state action slots output m. (Functor m) => RefLabel -> HalogenM state action slots output m (Maybe Element)
getRef p = HalogenM $ liftF $ GetRef p identity

imapState
  :: forall state state' action slots output m a
   . (state -> state')
  -> (state' -> state)
  -> HalogenM state action slots output m a
  -> HalogenM state' action slots output m a
imapState f f' = mapState (\s' -> (f' s', f))

mapState
  :: forall state state' action slots output m a
   . (state' -> (state, state -> state'))
  -> HalogenM state action slots output m a
  -> HalogenM state' action slots output m a
mapState lens = mapHalogen lens identity identity (NT identity)

mapAction
  :: forall state action action' slots output m a
   . (Functor m)
  => (action -> action')
  -> HalogenM state action slots output m a
  -> HalogenM state action' slots output m a
mapAction f = mapHalogen identityLens f identity (NT identity)

mapOutput
  :: forall state action slots output output' m a
   . (output -> output')
  -> HalogenM state action slots output m a
  -> HalogenM state action slots output' m a
mapOutput fo = mapHalogen identityLens identity fo (NT identity)

hoist
  :: forall state action slots output m m' a
   . (Functor m')
  => (m ~> m')
  -> HalogenM state action slots output m a
  -> HalogenM state action slots output m' a
hoist = mapHalogen identityLens identity identity

mapHalogen
  :: forall state state' action action' slots output output' m m' a
   . (state' -> (state, state -> state'))
  -> (action -> action')
  -> (output -> output')
  -> (m ~> m')
  -> HalogenM state action slots output m a
  -> HalogenM state' action' slots output' m' a
mapHalogen lens fa fo nat (HalogenM alg) = HalogenM (hoistF go alg)
  where
    go :: forall x. HalogenF state action slots output m x -> HalogenF state' action' slots output' m' x
    go = \case
      State f -> State (\s' -> let (s, g) = lens s' in map g (f s))
      Subscribe fes k -> Subscribe (map fa . fes) k
      Unsubscribe sid a -> Unsubscribe sid a
      Lift q -> Lift (runNT nat q)
      LiftEffect q -> LiftEffect (runNT nat q)
      Unlift q -> Unlift $ q . mapUnliftIO (mapHalogen lens fa fo nat)
      ChildQuery cq -> ChildQuery cq
      Raise o a -> Raise (fo o) a
      Par (HalogenAp p) -> Par (HalogenAp $ hoistAp (mapHalogen lens fa fo nat) p)
      Fork hmu k -> Fork (mapHalogen lens fa fo nat hmu) k
      Join fid a -> Join fid a
      Kill fid a -> Kill fid a
      GetRef p k -> GetRef p k
      Throw e -> Throw e
      Catch body handler k -> Catch (mapHalogen lens fa fo nat body) (mapHalogen lens fa fo nat . handler) k

identityLens :: forall s. s -> (s, s -> s)
identityLens s = (s, identity)
