-- | The store itself: one value for the whole application, changed only by
-- actions through a pure reducer, and the monad that carries it.
module Halogen.Store.Monad
  ( MonadStore (..)
  , HalogenStore
  , newHalogenStore
  , storeEmitter
  , StoreT (..)
  , runStoreT
  , RunningStore (..)
  , runAndEmitStoreT
  )
where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (ContT)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.RWS.Lazy qualified as RWSL
import Control.Monad.RWS.Strict qualified as RWSS
import Control.Monad.State.Lazy qualified as StateL
import Control.Monad.State.Strict qualified as StateS
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.UUID (MonadUUID)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Writer.Lazy qualified as WriterL
import Control.Monad.Writer.Strict qualified as WriterS
import Data.NT (type (~>) (..))
import Data.Type.Equality (type (~))
import Halogen qualified as H
import Halogen.Store.Select (Selector, selectEmitter)
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (MonadAttributes, MonadBrowserDOM, MonadDOM)
import Protolude hiding (Selector)
import UnliftIO (MonadUnliftIO)
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Web.Storage.IndexedDB (MonadIndexedDB)

-- | A monad that can read the store, update it and say when a part of it
-- changes: purescript-halogen-store's @MonadStore@, with 'MonadIO' as its
-- @MonadEffect@.
--
-- The store's type and its actions' are determined by the monad, so a
-- component written against @MonadStore Action Store m@ is used in any
-- monad that carries that one store, 'StoreT' or a transformer over it.
class (MonadIO m) => MonadStore action store m | m -> store action where
  -- | What the store holds now.
  getStore :: m store
  default getStore :: (MonadTrans t, MonadStore action store n, m ~ t n) => m store
  getStore = lift getStore

  -- | Run the reducer on the store and the action, keep its result, and
  -- tell every subscriber.
  updateStore :: action -> m ()
  default updateStore :: (MonadTrans t, MonadStore action store n, m ~ t n) => action -> m ()
  updateStore = lift . updateStore

  -- | The values a selector picks out of the store's updates, each only
  -- when it changed (see 'Halogen.Store.Select.selectEmitter'). It is an
  -- emitter in 'IO', which is what 'H.subscribe' takes.
  emitSelected :: Selector store a -> m (HS.Emitter IO a)
  default emitSelected :: (MonadTrans t, MonadStore action store n, m ~ t n) => Selector store a -> m (HS.Emitter IO a)
  emitSelected = lift . emitSelected

-- | A store: its value, the reducer, and whom to tell of an update.
--
-- Unlike purescript-halogen-store's record, the value is kept with the
-- number of the update that made it, and every update is sent to the
-- subscribers with its number. JavaScript runs one update at a time, but
-- here two threads can update the store at once, and an update can happen
-- inside another one's notification (a component that answers a change by
-- changing the store again), so the second update's subscribers may be
-- told before the first one's notification has reached them all. Each
-- subscription hands the updates on one at a time, in the order of their
-- numbers, and drops one that an update it has already handed on overtook
-- ('storeEmitter'), so that what a subscriber was told last is what the
-- store holds.
data HalogenStore action store = HalogenStore
  { value :: IORef (Update store)
  , listener :: HS.Listener IO (Update store)
  , updates :: HS.Emitter IO (Update store)
  , reducer :: store -> action -> store
  }

-- | The store's value after an update, with the update's number. Its fields
-- are strict (the package compiles with @StrictData@): an update evaluates
-- the reducer's result, so a store without subscribers does not pile up a
-- chain of unevaluated reductions for the next read to go through.
data Update store = Update {number :: Word64, value :: store}

-- | A store holding the value given, which the reducer changes.
newHalogenStore :: forall action store n. (MonadIO n) => store -> (store -> action -> store) -> n (HalogenStore action store)
newHalogenStore initial reducer = liftIO $ do
  value <- newIORef Update {number = 0, value = initial}
  subscribe <- HS.create
  pure HalogenStore {value, listener = subscribe.listener, updates = subscribe.emitter, reducer}

-- | Where a subscription is with the store's updates: the number of the
-- last one it handed on, the newest one it has yet to hand on, and whether
-- a thread is handing them on now.
data Delivery store = Delivery
  { seen :: Word64
  , pending :: Maybe (Update store)
  , delivering :: Bool
  }

-- | Every value the store takes from now on, in the order the updates were
-- made: the updates numbered after the one the store holds as the
-- subscription is made. An update made before it whose notification is
-- still on its way is not delivered, nor is one that a later update
-- overtook before it reached this subscriber; what the store holds at the
-- subscription is read with 'getStore'.
--
-- The subscriber is run by one thread at a time. An update that arrives
-- while another is being handed on, from another thread or from the
-- subscriber itself (nested in the notification it is answering), is left
-- for the thread handing on to take next, in place of any older update
-- still waiting; so the subscriber sees the updates in order, and the last
-- it sees is the newest.
storeEmitter :: forall action store. HalogenStore action store -> HS.Emitter IO store
storeEmitter store = HS.makeEmitter $ \push -> do
  current <- (.number) <$> readIORef store.value
  delivery <- newIORef Delivery {seen = current, pending = Nothing, delivering = False}
  let -- The thread handing on gives the slot up if the subscriber throws
      -- (or is cancelled in it): an update left in the slot is then taken by
      -- the next update's thread.
      stop = atomicModifyIORef' delivery (\d -> (d {delivering = False}, ()))
      deliver restore = do
        next <- atomicModifyIORef' delivery $ \d -> case d.pending of
          Nothing -> (d {delivering = False}, Nothing)
          Just update -> (d {seen = update.number, pending = Nothing}, Just update)
        for_ next $ \update -> do
          restore (push update.value) `onException` stop
          deliver restore
  -- Asynchronous exceptions are masked from taking the slot to giving it
  -- up, and let through only for the subscriber itself, with the caller's
  -- masking state: a thread cancelled between the two would otherwise
  -- leave the slot taken, and the subscription would hear nothing more.
  subscription <- HS.subscribe store.updates $ \update -> mask $ \restore -> do
    mine <- atomicModifyIORef' delivery $ \d ->
      let newer = update.number > d.seen && all ((update.number >) . (.number)) d.pending
          queued = if newer then d {pending = Just update} else d
       in if queued.delivering || isNothing queued.pending
            then (queued, False)
            else (queued {delivering = True}, True)
    when mine (deliver restore)
  pure (HS.unsubscribe subscription)

-- | The application's monad with a store in it: a component in
-- @StoreT action store m@ can use the store wherever it has @m@'s
-- operations. It is a 'ReaderT' underneath, as purescript-halogen-store's
-- is, so the DOM classes come with it, and 'runStoreT' hoists a tree in it
-- to the monad underneath before 'Halogen.VDom.Driver.runUI'.
newtype StoreT action store m a = StoreT (ReaderT (HalogenStore action store) m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadError e
    , MonadState s
    , MonadWriter w
    , PrimMonad
    , MonadDOM
    , MonadAttributes
    , MonadBrowserDOM
    , MonadIndexedDB
    )

instance MonadTrans (StoreT action store) where
  lift = StoreT . lift

-- | Generated by the monad underneath ('H.newRefLabel' needs one).
instance (MonadUUID m) => MonadUUID (StoreT action store m)

instance (MonadIO m) => MonadStore action store (StoreT action store m) where
  getStore = StoreT $ do
    store <- ask
    (.value) <$> readIORef store.value
  updateStore action = StoreT $ do
    store <- ask
    liftIO $ do
      new <- atomicModifyIORef' store.value $ \old ->
        let new = Update {number = old.number + 1, value = store.reducer old.value action} in (new, new)
      HS.notify store.listener new
  emitSelected selector = StoreT $ do
    store <- ask
    pure (selectEmitter selector (storeEmitter store))

-- | A component's programs use the store through 'H.liftEffect', not
-- 'lift'. In this port 'lift' and 'liftIO' are where a program may wait:
-- they run on a worker thread while the rest of the tree goes on, and the
-- program resumes after what came before it. The store's operations never
-- wait, so they run at once, as purescript-halogen-store's @lift@ does on
-- 'Effect': an update made in an event's action is in the store, and its
-- subscribers have heard of it, before the action goes on.
instance (MonadStore action store m) => MonadStore action store (H.HalogenM st act slots output m) where
  getStore = H.liftEffect getStore
  updateStore = H.liftEffect . updateStore
  emitSelected = H.liftEffect . emitSelected

instance (MonadStore action store m) => MonadStore action store (ReaderT r m)

instance (MonadStore action store m) => MonadStore action store (ExceptT e m)

instance (MonadStore action store m) => MonadStore action store (MaybeT m)

instance (MonadStore action store m) => MonadStore action store (IdentityT m)

instance (MonadStore action store m) => MonadStore action store (StateS.StateT s m)

instance (MonadStore action store m) => MonadStore action store (StateL.StateT s m)

instance (MonadStore action store m, Monoid w) => MonadStore action store (WriterS.WriterT w m)

instance (MonadStore action store m, Monoid w) => MonadStore action store (WriterL.WriterT w m)

instance (MonadStore action store m, Monoid w) => MonadStore action store (RWSS.RWST r w s m)

instance (MonadStore action store m, Monoid w) => MonadStore action store (RWSL.RWST r w s m)

instance (MonadStore action store m) => MonadStore action store (ContT r m)

-- | A tree whose components use a store, made into one that runs in the
-- monad underneath: the store is made here, holding the value given, and
-- every component of the tree shares it. Call it once, at the root, before
-- 'Halogen.VDom.Driver.runUI':
--
-- > root <- runStoreT initialStore reduce App.component
-- > runUI root () body
runStoreT
  :: forall action store query input output m n
   . (MonadIO n, Functor m)
  => store
  -> (store -> action -> store)
  -> H.Component query input output (StoreT action store m)
  -> n (H.Component query input output m)
runStoreT initial reducer component = (.component) <$> runAndEmitStoreT initial reducer component

-- | What 'runAndEmitStoreT' gives: the tree, and an emitter of every value
-- the store takes.
data RunningStore query input output m store = RunningStore
  { emitter :: HS.Emitter IO store
  , component :: H.Component query input output m
  }

-- | 'runStoreT', also handing back an emitter of the store's values, for
-- the program around the tree: to save the store as it changes, say, or to
-- log it.
runAndEmitStoreT
  :: forall action store query input output m n
   . (MonadIO n, Functor m)
  => store
  -> (store -> action -> store)
  -> H.Component query input output (StoreT action store m)
  -> n (RunningStore query input output m store)
runAndEmitStoreT initial reducer component = do
  store <- newHalogenStore initial reducer
  pure
    RunningStore
      { emitter = storeEmitter store
      , component = H.hoist (NT (\(StoreT program) -> runReaderT program store)) component
      }
