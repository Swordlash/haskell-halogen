module Halogen.IO.Driver.State
  ( LifecycleHandlers (..)
  , DriverState (..)
  , DriverStateRef (..)
  , DriverStateX (..)
  , unDriverStateX
  -- , mkDriverStateXRef
  , readDriverStateRef
  , RenderStateX (..)
  , renderStateX
  , renderStateX_
  -- , unRenderStateX
  , initDriverState
  , RenderGate (..)
  , Leave (..)
  , enterRender
  , beginPass
  , runOrQueue
  , leaveRender
  , abandonRender
  )
where

import Control.Monad.Fork
import Data.Row
import HPrelude hiding (state)
import Halogen.Component
import Halogen.Data.Slot as SlotStorage
import Halogen.Query.HalogenM
import Halogen.Subscription qualified as HS
import Web.DOM.Element (Element)

data LifecycleHandlers m = LifecycleHandlers
  { initializers :: [m ()]
  , finalizers :: [m ()]
  , nesting :: Int
  }

data DriverState m r s f act ps i o = DriverState
  { component :: ComponentSpec s f act ps i o m
  , state :: s
  , refs :: Map Text Element
  , children :: SlotStorage ps (DriverStateRef m r)
  , selfRef :: IORef (DriverState m r s f act ps i o)
  , handlerRef :: IORef (o -> m ())
  , pendingQueries :: IORef (Maybe [m ()])
  , pendingOuts :: IORef (Maybe [m ()])
  , renderGate :: IORef (RenderGate m)
  -- ^ Whether a render is in progress, and what waits for it to end.
  , rendering :: Maybe (r s act ps o)
  , fresh :: IORef Int
  , subscriptions :: IORef (Maybe (Map SubscriptionId (HS.Subscription m)))
  , forks :: IORef (Maybe (Map ForkId (Fork m ())))
  -- ^ 'Nothing' once the component is finalized: a fork registered after
  -- that is killed at once rather than left running.
  , lifecycleHandlers :: IORef (LifecycleHandlers m)
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

{-# SPECIALIZE initDriverState :: ComponentSpec s f act ps i o IO -> i -> (o -> IO ()) -> IORef (LifecycleHandlers IO) -> IO (DriverState IO r s f act ps i o) #-}
initDriverState
  :: (MonadIO m)
  => ComponentSpec s f act ps i o m
  -> i
  -> (o -> m ())
  -> IORef (LifecycleHandlers m)
  -> m (DriverState m r s f act ps i o)
initDriverState component input handler lchs = do
  selfRef <- newIORef (fix identity)
  handlerRef <- newIORef handler
  pendingQueries <- newIORef (Just [])
  pendingOuts <- newIORef (Just [])
  renderGate <- newIORef Idle
  fresh <- newIORef 1
  subscriptions <- newIORef (Just mempty)
  forks <- newIORef (Just mempty)
  state <- component.initialState input
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
          , renderGate
          , rendering = Nothing
          , fresh
          , subscriptions
          , forks
          , lifecycleHandlers = lchs
          }
  atomicWriteIORef selfRef ds
  pure ds

-- | The render lock of one component, and everything that has to wait for it.
--
-- Only one render pass may walk a component's slots at a time: a second one
-- would pop children the first is still matching and re-create live ones
-- ("Duplicate slot address"). Nothing ever waits for the lock, though. A
-- render is re-entrant (a child's output makes its parent render, which
-- renders the child), so a thread that blocked on it could be waiting for
-- itself. A render asked for while one is in progress is marked instead, and
-- the render in progress does one more pass; an action raised during a render
-- is queued, and forked once the pass is over.
--
-- The lock, the queue and the mark are one value, and every change to it is
-- one 'atomicModifyIORef'' below. Kept apart, a handler queued just as the
-- lock is let go, or a render asked for just then, could be lost.
data RenderGate m
  = Idle
  | Rendering
      { queued :: [m ()]
      -- ^ Newest first.
      , again :: Bool
      -- ^ A render was asked for during this pass.
      }

-- | Take the lock, or, if a render holds it, ask that render for another
-- pass. 'True' when the caller now holds the lock and has to render.
enterRender :: (MonadIO m) => IORef (RenderGate m) -> m Bool
enterRender gate = atomicModifyIORef' gate $ \case
  Idle -> (Rendering [] False, True)
  Rendering q _ -> (Rendering q True, False)

-- | Start a pass: it renders the latest state, so earlier requests for
-- another pass are answered by it.
beginPass :: (MonadIO m) => IORef (RenderGate m) -> m ()
beginPass gate = atomicModifyIORef'_ gate $ \case
  Idle -> Idle
  Rendering q _ -> Rendering q False

-- | Run an action now, or queue it if a render holds the lock.
runOrQueue :: (MonadIO m) => IORef (RenderGate m) -> m () -> m ()
runOrQueue gate act = do
  runNow <- atomicModifyIORef' gate $ \case
    Idle -> (Idle, True)
    Rendering q again -> (Rendering (act : q) again, False)
  when runNow act

-- | What the holder of the lock does next.
data Leave m
  = -- | Run these (oldest first) and try to leave again.
    Drain [m ()]
  | -- | Render another pass, still holding the lock.
    Again
  | -- | The lock is let go.
    Done

-- | Let the lock go, unless something still waits for it: queued actions
-- come out first, then another pass if one was asked for.
leaveRender :: (MonadIO m) => IORef (RenderGate m) -> m (Leave m)
leaveRender gate = atomicModifyIORef' gate $ \case
  Idle -> (Idle, Done)
  Rendering [] False -> (Idle, Done)
  Rendering [] True -> (Rendering [] True, Again)
  Rendering q again -> (Rendering [] again, Drain (reverse q))

-- | A render failed: let the lock go, and hand back what was queued for it
-- (oldest first), so that neither the lock nor those actions are lost with
-- it. A request for another pass is dropped: it would render the same state.
abandonRender :: (MonadIO m) => IORef (RenderGate m) -> m [m ()]
abandonRender gate = atomicModifyIORef' gate $ \case
  Idle -> (Idle, [])
  Rendering q _ -> (Idle, reverse q)
