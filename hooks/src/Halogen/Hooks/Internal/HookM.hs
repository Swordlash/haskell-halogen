-- | The monad a hook component's event handlers run in.
--
-- 'HookM' is to a hooks component what 'Halogen.Query.HalogenM.HalogenM' is to
-- an ordinary one, minus the component state: a hooks component's state is
-- whatever its 'Halogen.Hooks.useState' calls asked for, so instead of a
-- @state@ type parameter the state operations take a 'StateId' saying which
-- cell they mean.
--
-- That is also why this is a free monad over its own algebra rather than a
-- newtype over 'Halogen.Query.HalogenM.HalogenM'. The state of a hooks
-- component is an interpreter detail whose type mentions the whole hook list;
-- threading it through the type of every handler would defeat the point.
-- "Halogen.Hooks.Internal.Eval" interprets these instructions into the
-- @HalogenM@ the driver expects.
module Halogen.Hooks.Internal.HookM
  ( HookM (..)
  , HookF (..)
  , HookAction
  , HookHTML

    -- * State
  , get
  , put
  , modify
  , modify_

    -- * Talking to the component's surroundings
  , raise
  , query
  , queryAll

    -- * Subscriptions
  , subscribe
  , subscribe'
  , unsubscribe

    -- * Forks
  , fork
  , kill

    -- * Refs
  , getRef
  )
where

import Control.Monad.Free.Church (F, liftF)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Map.Strict qualified as M
import Data.Row (HasType, Row)
import Halogen.Data.Slot (Slot)
import Halogen.Data.Slot qualified as Slot
import Halogen.HTML (ComponentHTML)
import Halogen.Hooks.Internal.Types (StateId (..))
import Halogen.Query.ChildQuery qualified as CQ
import Halogen.Query.HalogenM (ForkId, SubscriptionId)
import Halogen.Query.Input (RefLabel)
import Halogen.Subscription qualified as HS
import Protolude hiding (get, gets, modify, put, state)
import Web.DOM.Element (Element)

type HookF :: Type -> Row Type -> Type -> (Type -> Type) -> Type -> Type

-- | The instruction set. Every constructor ends in a continuation rather than
-- carrying a @Functor m@ constraint, so 'HookM' is a monad for any @m@ at all
-- and the constraints appear only where the program is run.
data HookF scope slots output m a where
  Lift :: m x -> (x -> a) -> HookF scope slots output m a
  State :: StateId scope s -> (s -> (x, s)) -> (x -> a) -> HookF scope slots output m a
  Raise :: output -> a -> HookF scope slots output m a
  ChildQuery :: CQ.ChildQuery slots a -> HookF scope slots output m a
  Subscribe :: (SubscriptionId -> HS.Emitter IO (HookM scope slots output m ())) -> (SubscriptionId -> a) -> HookF scope slots output m a
  Unsubscribe :: SubscriptionId -> a -> HookF scope slots output m a
  Fork :: HookM scope slots output m () -> (ForkId -> a) -> HookF scope slots output m a
  Kill :: ForkId -> a -> HookF scope slots output m a
  GetRef :: RefLabel -> (Maybe Element -> a) -> HookF scope slots output m a

instance Functor (HookF scope slots output m) where
  fmap f = \case
    Lift mx k -> Lift mx (f . k)
    State sid g k -> State sid g (f . k)
    Raise o a -> Raise o (f a)
    ChildQuery cq -> ChildQuery (map f cq)
    Subscribe e k -> Subscribe e (f . k)
    Unsubscribe sid a -> Unsubscribe sid (f a)
    Fork hm k -> Fork hm (f . k)
    Kill fid a -> Kill fid (f a)
    GetRef l k -> GetRef l (f . k)

type HookM :: Type -> Row Type -> Type -> (Type -> Type) -> Type -> Type
newtype HookM scope slots output m a = HookM (F (HookF scope slots output m) a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans (HookM scope slots output) where
  lift mx = HookM $ liftF $ Lift mx identity

instance (MonadIO m) => MonadIO (HookM scope slots output m) where
  liftIO = lift . liftIO

-- | What a hooks component's DOM emits: an action is simply the 'HookM'
-- program to run when the event fires.
type HookAction scope slots output m = HookM scope slots output m ()

-- | The HTML a hook program renders to.
type HookHTML scope slots output m = ComponentHTML (HookAction scope slots output m) slots m

-- | Read the current value of a state cell. Always the current value, not the
-- one the handler was rendered with.
get :: forall s scope slots output m. StateId scope s -> HookM scope slots output m s
get sid = HookM $ liftF $ State sid (\s -> (s, s)) identity

-- | Replace the value of a state cell, scheduling a re-render.
put :: forall s scope slots output m. StateId scope s -> s -> HookM scope slots output m ()
put sid s = HookM $ liftF $ State sid (const ((), s)) identity

-- | Modify a state cell, returning the new value.
modify :: forall s scope slots output m. StateId scope s -> (s -> s) -> HookM scope slots output m s
modify sid f = HookM $ liftF $ State sid (\s -> let s' = f s in (s', s')) identity

-- | Modify a state cell.
modify_ :: forall s scope slots output m. StateId scope s -> (s -> s) -> HookM scope slots output m ()
modify_ sid f = HookM $ liftF $ State sid (\s -> ((), f s)) identity

-- | Raise an output message for the parent component.
raise :: forall scope slots output m. output -> HookM scope slots output m ()
raise o = HookM $ liftF $ Raise o ()

-- | Send a query to a child component at the given slot.
query
  :: forall label
  ->forall scope slots output m query output' slot a
   . (HasType label (Slot query output' slot) slots)
  => (KnownSymbol label)
  => (Ord slot)
  => slot
  -> query a
  -> HookM scope slots output m (Maybe a)
query label p q =
  HookM
    $ liftF
    $ ChildQuery
    $ CQ.ChildQuery (\k -> maybe (pure Nothing) k . Slot.lookup label p) q identity

-- | Send a query to every child component at the given slot label.
queryAll
  :: forall label
  ->forall scope slots output m query output' slot a
   . (HasType label (Slot query output' slot) slots)
  => (KnownSymbol label)
  => (Ord slot)
  => query a
  -> HookM scope slots output m (Map slot a)
queryAll label q =
  HookM
    $ liftF
    $ ChildQuery
    $ CQ.ChildQuery (\k -> map catMapMaybes . traverse k . Slot.slots label) q identity
  where
    catMapMaybes :: forall k v. (Ord k) => Map k (Maybe v) -> Map k v
    catMapMaybes = M.foldlWithKey' (\acc k v -> maybe acc (flip (M.insert k) acc) v) M.empty

-- | Subscribe to an emitter of hook programs. The subscription ends with the
-- component, or at 'unsubscribe'.
subscribe :: forall scope slots output m. HS.Emitter IO (HookAction scope slots output m) -> HookM scope slots output m SubscriptionId
subscribe es = HookM $ liftF $ Subscribe (const es) identity

-- | 'subscribe' for subscriptions that end themselves: the id is handed to the
-- emitter rather than returned, so what it emits can refer to it.
subscribe' :: forall scope slots output m. (SubscriptionId -> HS.Emitter IO (HookAction scope slots output m)) -> HookM scope slots output m ()
subscribe' esc = HookM $ liftF $ Subscribe esc (const ())

-- | End a subscription early.
unsubscribe :: forall scope slots output m. SubscriptionId -> HookM scope slots output m ()
unsubscribe sid = HookM $ liftF $ Unsubscribe sid ()

-- | Run a hook program independently of the one that started it.
fork :: forall scope slots output m. HookAction scope slots output m -> HookM scope slots output m ForkId
fork hm = HookM $ liftF $ Fork hm identity

-- | Kill a forked program.
kill :: forall scope slots output m. ForkId -> HookM scope slots output m ()
kill fid = HookM $ liftF $ Kill fid ()

-- | The element currently rendered at a 'RefLabel', if any.
getRef :: forall scope slots output m. RefLabel -> HookM scope slots output m (Maybe Element)
getRef label = HookM $ liftF $ GetRef label identity
