-- | What the driver keeps for a mounted tree and each of its components.
module Halogen.IO.Driver.State
  ( Life (..)
  , Tree (..)
  , treeRunM
  , treeRender
  , Batch (..)
  , DriverState (..)
  , DriverStateRef (..)
  , DriverStateX (..)
  , unDriverStateX
  , readDriverStateRef
  , RenderStateX (..)
  , renderStateX
  , renderStateX_
  , initDriverState
  )
where

import Data.Row
import HPrelude hiding (state)
import Halogen.Component
import Halogen.Data.Slot as SlotStorage
import Halogen.IO.Driver.Runtime (Fiber, Loop, Turn)
import Halogen.Query.HalogenM
import Halogen.Subscription qualified as HS
import Web.DOM.Element (Element)

-- | Where a component is in its life. Its programs run while it is 'Alive';
-- once it is being finalized ('Closing') only its finalizer (and what that
-- forks) does, and it renders no more; once that is over it is 'Dead', and
-- nothing of it runs again.
data Life = Alive | Closing | Dead
  deriving stock (Eq, Show)

-- | A mounted tree: its loop, the way into the components' monad, the
-- lifecycle work of the render transaction in progress, and how a
-- component renders.
data Tree m r = Tree
  { loop :: Loop
  , runM :: forall a. m a -> IO a
  , batch :: IORef (Maybe Batch)
  -- ^ 'Just' while a render transaction runs on the loop. Renders nested in
  -- it (a child's input changing its state) add to it, and it is carried
  -- out when the outermost one ends.
  , render :: forall s f act ps i o. IORef (DriverState m r s f act ps i o) -> IO [Fiber]
  -- ^ Renders a component; hands back the initializers that the render
  -- transaction it opened started (none if it was nested in another), for
  -- the program that changed the state to wait for.
  }

-- | Run the components' monad (the fields are polymorphic, so not reached
-- with a dot).
treeRunM :: Tree m r -> m a -> IO a
treeRunM Tree {runM} = runM

treeRender :: Tree m r -> IORef (DriverState m r s f act ps i o) -> IO [Fiber]
treeRender Tree {render} = render

-- | Lifecycle work collected during a render transaction: run once it has
-- committed, finalizers first. Initializers are programs, since a parent's
-- initializer waits for its children's.
data Batch = Batch
  { initializers :: IORef [Turn ()]
  -- ^ Newest first.
  , finalizers :: IORef [IO ()]
  }

data DriverState m r s f act ps i o = DriverState
  { component :: ComponentSpec s f act ps i o m
  , state :: s
  , refs :: Map Text Element
  , children :: SlotStorage ps (DriverStateRef m r)
  , selfRef :: IORef (DriverState m r s f act ps i o)
  , handlerRef :: IORef (o -> IO ())
  -- ^ Where the component's outputs go: its parent, or the tree's messages.
  , pendingQueries :: IORef (Maybe [IO ()])
  -- ^ Outputs of the children that come before this component has been
  -- initialized; 'Nothing' after.
  , pendingOuts :: IORef (Maybe [IO ()])
  -- ^ This component's outputs that come before it has been initialized.
  , pendingHandlers :: IORef (Maybe [IO ()])
  -- ^ 'Just' while a render transaction of this component runs: the
  -- actions raised meanwhile start once it is over.
  , inPass :: IORef Bool
  , renderAgain :: IORef Bool
  -- ^ The state changed during a render pass: the pass is done again.
  , rendering :: Maybe (r s act ps o)
  , fresh :: IORef Int
  , life :: IORef Life
  , fibers :: IORef (IntMap Fiber)
  -- ^ Every program of the component that has not ended.
  , subscriptions :: IORef (Map SubscriptionId (HS.Subscription IO))
  , forks :: IORef (Map ForkId Fiber)
  , tree :: Tree m r
  }

data DriverStateX m r f o = forall s act ps i. DriverStateX (DriverState m r s f act ps i o)

data DriverStateRef m r f o = forall s act ps i. DriverStateRef (IORef (DriverState m r s f act ps i o))

{-# INLINE readDriverStateRef #-}
readDriverStateRef :: (MonadIO m) => DriverStateRef m r f o -> m (DriverStateX m r f o)
readDriverStateRef (DriverStateRef ref) = DriverStateX <$> readIORef ref

data RenderStateX (r :: Type -> Type -> Row Type -> Type -> Type) = forall s act ps o. RenderStateX (r s act ps o)

{-# INLINE renderStateX #-}
renderStateX
  :: (Functor m)
  => (forall s act ps. Maybe (r s act ps o) -> m (r s act ps o))
  -> DriverStateX m r f o
  -> m (RenderStateX r)
renderStateX f = unDriverStateX $ \st ->
  RenderStateX <$> f st.rendering

{-# INLINE renderStateX_ #-}
renderStateX_
  :: (Applicative m)
  => (forall s act ps. r s act ps o -> m ())
  -> DriverStateX m r f o
  -> m ()
renderStateX_ f = unDriverStateX $ \st ->
  traverse_ f st.rendering

{-# INLINE unDriverStateX #-}
unDriverStateX :: (forall s act ps i. DriverState m r s f act ps i o -> a) -> DriverStateX m r f o -> a
unDriverStateX f (DriverStateX st) = f st

initDriverState
  :: ComponentSpec s f act ps i o m
  -> s
  -> (o -> IO ())
  -> Tree m r
  -> IO (DriverState m r s f act ps i o)
initDriverState component state handler tree = do
  selfRef <- newIORef (fix identity)
  handlerRef <- newIORef handler
  pendingQueries <- newIORef (Just [])
  pendingOuts <- newIORef (Just [])
  pendingHandlers <- newIORef Nothing
  inPass <- newIORef False
  renderAgain <- newIORef False
  fresh <- newIORef 1
  life <- newIORef Alive
  fibers <- newIORef mempty
  subscriptions <- newIORef mempty
  forks <- newIORef mempty
  let ds =
        DriverState
          { component
          , state
          , refs = mempty
          , children = SlotStorage.empty
          , selfRef
          , handlerRef
          , pendingQueries
          , pendingOuts
          , pendingHandlers
          , inPass
          , renderAgain
          , rendering = Nothing
          , fresh
          , life
          , fibers
          , subscriptions
          , forks
          , tree
          }
  writeIORef selfRef ds
  pure ds
