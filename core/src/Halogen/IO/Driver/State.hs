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
  , idleGate
  , Leave (..)
  , enterRender
  , beginPass
  , runOrQueue
  , leaveRender
  , abandonRender
  , nextToDrain
  , drainQueue
  , untilWaiting
  )
where

import Control.Monad.Fork
import GHC.Conc (ThreadStatus (..), threadStatus)
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
  renderGate <- newIORef idleGate
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

-- | The render lock of a component, and the actions waiting for it.
--
-- Only one render pass may walk a component's slots at a time: a second one
-- would pop children the first is still matching and re-create live ones
-- ("Duplicate slot address"). Nothing ever waits for the lock, though. A
-- render is re-entrant (a child's output makes its parent render, which
-- renders the child), so a thread that blocked on it could be waiting for
-- itself. A render asked for while one is in progress is marked instead, and
-- the render in progress does one more pass; an action raised during a render
-- is queued, and started once the pass is over.
--
-- Queued actions start in the order they came, one after another, each
-- running until it first waits (as purescript-halogen's Aff fibers do), on
-- a thread of its own so that one waiting long holds up no other. While any
-- of them has yet to start, a new action queues behind them too: run at
-- once, it would overtake them, and an input's handler, say, would see the
-- keys typed in another order.
--
-- The lock, the queue and the marks are one value, and every change to it
-- is one 'atomicModifyIORef'' below. Kept apart, a handler queued just as
-- the lock is let go, or a render asked for just then, could be lost.
data RenderGate m = RenderGate
  { held :: Bool
  -- ^ A render holds the lock.
  , again :: Bool
  -- ^ A render was asked for during this pass.
  , queued :: [m ()]
  -- ^ Newest first.
  , draining :: Bool
  -- ^ A thread is starting queued actions; it takes the ones queued
  -- meanwhile too.
  }

-- | Nothing rendering, nothing waiting.
idleGate :: RenderGate m
idleGate = RenderGate False False [] False

-- | Take the lock, or, if a render holds it, ask that render for another
-- pass. 'True' when the caller now holds the lock and has to render.
enterRender :: (MonadIO m) => IORef (RenderGate m) -> m Bool
enterRender gate = atomicModifyIORef' gate $ \g ->
  if g.held then (g {again = True}, False) else (g {held = True, again = False}, True)

-- | Start a pass: it renders the latest state, so earlier requests for
-- another pass are answered by it.
beginPass :: (MonadIO m) => IORef (RenderGate m) -> m ()
beginPass gate = atomicModifyIORef'_ gate $ \g -> if g.held then g {again = False} else g

-- | Run an action now, or queue it if a render holds the lock or queued
-- actions have yet to start.
runOrQueue :: (MonadIO m) => IORef (RenderGate m) -> m () -> m ()
runOrQueue gate act = do
  runNow <- atomicModifyIORef' gate $ \g ->
    if g.held || g.draining || not (null g.queued)
      then (g {queued = act : g.queued}, False)
      else (g, True)
  when runNow act

-- | What the holder of the lock does next.
data Leave m
  = -- | Start these (oldest first) with 'drainQueue', then try to leave
    -- again. The caller is now the one draining the queue.
    Drain [m ()]
  | -- | Render another pass, still holding the lock.
    Again
  | -- | The lock is let go.
    Done

-- | Let the lock go, unless something still waits for it: queued actions
-- come out first (unless a thread is already starting queued actions,
-- which will take these too), then another pass if one was asked for.
leaveRender :: (MonadIO m) => IORef (RenderGate m) -> m (Leave m)
leaveRender gate = atomicModifyIORef' gate leave
  where
    leave g
      | not g.held = (g, Done)
      | not g.draining && not (null g.queued) = (g {queued = [], draining = True}, Drain (reverse g.queued))
      | g.again = (g, Again)
      | otherwise = (g {held = False}, Done)

-- | The actions queued while a batch was being started (oldest first), to
-- start next; or, when there are none or a render holds the lock (it hands
-- them out as it leaves), 'Nothing', and the draining is over.
nextToDrain :: (MonadIO m) => IORef (RenderGate m) -> m (Maybe [m ()])
nextToDrain gate = atomicModifyIORef' gate $ \g ->
  if not g.held && not (null g.queued)
    then (g {queued = []}, Just (reverse g.queued))
    else (g {draining = False}, Nothing)

-- | Start queued actions in order, then the ones queued meanwhile. For
-- whoever was handed a 'Drain' (or a batch by 'abandonRender').
--
-- Each runs on a thread of its own, and the next starts only once it has
-- finished or waits: blocked on an 'MVar', STM, a delay, or a foreign call
-- (a JavaScript promise on wasm and JS). So an action's work up to its
-- first wait comes before the next action's, with any number of
-- capabilities; and one that waits long holds up no other. A 'yield'
-- would not do: it is a hint to the scheduler, and on another capability
-- (or even on this one) the next action could run first.
drainQueue :: (MonadIO m, MonadFork m) => IORef (RenderGate m) -> [m ()] -> m ()
drainQueue gate = fix $ \go batch -> do
  for_ batch $ \act -> do
    started <- liftIO newEmptyMVar
    _ <- Control.Monad.Fork.fork (liftIO (myThreadId >>= putMVar started) >> act)
    liftIO (takeMVar started >>= untilWaiting)
  nextToDrain gate >>= traverse_ go

-- | Until a thread has finished, died, or waits for something.
untilWaiting :: ThreadId -> IO ()
untilWaiting t =
  threadStatus t >>= \case
    ThreadRunning -> yield >> untilWaiting t
    _ -> pure ()

-- | A render failed: let the lock go, and hand back what was queued for it
-- (oldest first; to start with 'drainQueue', unless a thread already
-- drains the queue) and whether another render was asked for while it ran,
-- so that neither the lock, those actions, nor that request are lost with
-- it. The request came from a state newer than the one that failed, so it
-- is worth a try; with none, retrying would fail on the same state again.
abandonRender :: (MonadIO m) => IORef (RenderGate m) -> m (Maybe [m ()], Bool)
abandonRender gate = atomicModifyIORef' gate $ \g ->
  let g' = g {held = False, again = False}
   in if not g.draining && not (null g.queued)
        then (g' {queued = [], draining = True}, (Just (reverse g.queued), g.again))
        else (g', (Nothing, g.again))
