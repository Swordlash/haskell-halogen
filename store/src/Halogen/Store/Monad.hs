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
-- subscription keeps the number of the last update it was given and drops
-- an older one ('storeEmitter'), so that what a subscriber was told last
-- is what the store holds.
data HalogenStore action store = HalogenStore
  { value :: IORef (Word64, store)
  , listener :: HS.Listener IO (Word64, store)
  , updates :: HS.Emitter IO (Word64, store)
  , reducer :: store -> action -> store
  }

-- | A store holding the value given, which the reducer changes.
newHalogenStore :: forall action store n. (MonadIO n) => store -> (store -> action -> store) -> n (HalogenStore action store)
newHalogenStore initial reducer = liftIO $ do
  value <- newIORef (0, initial)
  subscribe <- HS.create
  pure HalogenStore {value, listener = subscribe.listener, updates = subscribe.emitter, reducer}

-- | Every value the store takes from now on, in the order the updates were
-- made: an update that was overtaken by a later one before it reached this
-- subscriber is not delivered to it.
storeEmitter :: forall action store. HalogenStore action store -> HS.Emitter IO store
storeEmitter store = HS.makeEmitter $ \push -> do
  latest <- newIORef 0
  subscription <- HS.subscribe store.updates $ \(n, s) -> do
    newer <- atomicModifyIORef' latest (\seen -> if n > seen then (n, True) else (seen, False))
    when newer (push s)
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
    snd <$> readIORef store.value
  updateStore action = StoreT $ do
    store <- ask
    liftIO $ do
      new <- atomicModifyIORef' store.value $ \(n, s) ->
        let new = (n + 1, store.reducer s action) in (new, new)
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
