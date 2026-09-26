{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}

-- | The scheduler a mounted component tree runs on.
--
-- purescript-halogen runs a component's code in Aff: synchronous work runs
-- to its end, and only an asynchronous step lets anything else in. GHC's
-- threads are preemptive instead, so this module makes the same model
-- explicit:
--
-- * A tree has one 'Loop', the only place where the tree's state, children,
--   renders and lifecycle are touched. It is run by whichever thread brings
--   it work while it is idle (so a browser event is handled while the event
--   is being dispatched, and 'preventDefault' still counts), one piece of
--   work at a time, until nothing is queued. The one exception is the
--   browser's own: an event a piece of work dispatches (from a
--   'Halogen.Query.HalogenM.liftEffect') is handled there and then, nested
--   in it, as the browser runs handlers while it dispatches; so is work the
--   thread running the loop brings it itself.
--
-- * A component's program runs as a 'Fiber': a continuation, driven in
--   'Turn'. Its synchronous parts run on the loop; at a suspension ('await')
--   the rest is set aside, the awaited action runs on a worker thread, and
--   the continuation comes back through the loop's queue. Nothing else ever
--   waits: a fiber waiting for another (a join, a child's answer) leaves a
--   callback and returns.
--
-- * A fiber can be cancelled: it never runs again (not even the rest of the
--   synchronous work that cancelled it), and its worker, if it has one, is
--   killed, as are the fibers of its parallel branches. A worker never runs
--   the loop, so killing it cannot cut a piece of the tree's work in half.
module Halogen.IO.Driver.Runtime
  ( -- * The loop
    Loop
  , newLoop
  , enter
  , enterCallback
  , post
  , withinEffect

    -- * Fibers
  , Fiber
  , Outcome (..)
  , newFiber
  , startFiber
  , fiberFor
  , cancelFiber
  , fiberOver
  , onFiberOver
  , fiberClosingOk

    -- * Programs
  , Turn (..)
  , runTurn
  , sync
  , throwTurn
  , catchTurn
  , await
  , suspend
  , currentFiber
  , ParTurn (..)
  , sequentialTurn
  )
where

import Control.Concurrent (ThreadId, forkIO, forkIOWithUnmask, myThreadId, throwTo)
import Control.Exception (SomeAsyncException (..), SomeException, catch, fromException, mask, mask_, onException, throwIO, try)
import Control.Monad (ap, unless, void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_, traverse_)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Sequence (Seq, ViewL (..), viewl, (<|), (|>))
import Data.Sequence qualified as Seq
import Prelude
import UnliftIO (AsyncCancelled (..))

----------------------------------------------------------------------
-- The loop

-- | Whether a thread is running the loop (and which, once it has started),
-- and what waits for it.
data LoopState = LoopState
  { running :: !Bool
  , runner :: !(Maybe ThreadId)
  -- ^ 'Nothing' while the loop passes to a thread just forked to run it.
  , effects :: !Int
  -- ^ How many synchronous effects of components ('withinEffect') are
  -- under way on the loop.
  , queue :: !(Seq (IO ()))
  }

newtype Loop = Loop (IORef LoopState)

newLoop :: IO Loop
newLoop = Loop <$> newIORef (LoopState False Nothing 0 Seq.empty)

data Entry = Claimed | Nested | Queued

-- | Have the loop run a piece of work: on this thread and at once when
-- nobody runs the loop (and then whatever is queued meanwhile), otherwise
-- after what is queued already. Never waits.
--
-- The thread running the loop that enters it again, from inside a piece of
-- work (an emitter a 'Halogen.Query.HalogenM.liftEffect' notifies, say),
-- runs the new work there and then, nested in the old.
--
-- The work runs in the masking state of the thread that entered.
enter :: Loop -> IO () -> IO ()
enter = enterWith False

-- | 'enter', for a browser event's handler. A browser runs an event's
-- handlers while it dispatches it, so an event a component dispatches from
-- a synchronous effect is handled before the effect goes on: the handler
-- may still prevent the event's default action, or stop its propagation.
-- In the browser the handler runs on a thread of its own while the thread
-- running the loop waits for the dispatch to return, so there it is nested
-- whenever the loop is inside a synchronous effect ('withinEffect'). (There
-- is only ever one thread running, and a handler is called either from the
-- page's own event loop, with no effect under way, or by the effect.)
enterCallback :: Loop -> IO () -> IO ()
enterCallback = enterWith browser
  where
#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
    browser = True
#else
    browser = False
#endif

enterWith :: Bool -> Loop -> IO () -> IO ()
enterWith callback loop@(Loop ref) work = do
  me <- myThreadId
  -- Masked from taking the loop to running it: the loop must not be left
  -- taken with nobody to run it.
  mask $ \restore -> do
    entry <- atomicModifyIORef' ref $ \s ->
      if
        | not s.running -> (s {running = True, runner = Just me}, Claimed)
        | s.runner == Just me || (callback && s.effects > 0) -> (s, Nested)
        | otherwise -> (s {queue = s.queue |> work}, Queued)
    case entry of
      Claimed -> runLoop loop restore work
      Nested -> restore work
      Queued -> pure ()

-- | Run a component's synchronous effect, on the loop.
withinEffect :: Loop -> IO a -> IO a
withinEffect (Loop ref) io = mask $ \restore -> do
  count 1
  restore io `onException` count (-1) <* count (-1)
  where
    count n = atomicModifyIORef' ref (\s -> (s {effects = s.effects + n}, ()))

-- | The same, for a thread that must not run the loop itself (a worker,
-- which is killed with its fiber): when nobody runs the loop, a new thread
-- starts it.
post :: Loop -> IO () -> IO ()
post loop@(Loop ref) work = mask_ $ do
  mine <- atomicModifyIORef' ref $ \s ->
    if s.running
      then (s {queue = s.queue |> work}, False)
      else (s {running = True, runner = Nothing}, True)
  when mine $ handOver loop work

-- | Start a thread to run the loop, which this thread has taken, beginning
-- with this work. Called masked. If no thread can be started the loop is
-- given up, with the work back at the front of its queue for whoever
-- enters next.
handOver :: Loop -> IO () -> IO ()
handOver loop@(Loop ref) work =
  void (forkIOWithUnmask $ \unmask -> adopt >> runLoop loop unmask work)
    `onException` atomicModifyIORef' ref (\s -> (s {running = False, runner = Nothing, queue = work <| s.queue}, ()))
  where
    adopt = do
      me <- myThreadId
      atomicModifyIORef' ref (\s -> (s {runner = Just me}, ()))

-- | The next piece of work, or, with none, give the loop up (in the same
-- step, so that work queued just then is not stranded).
next :: Loop -> IO (Maybe (IO ()))
next (Loop ref) = atomicModifyIORef' ref $ \s -> case viewl s.queue of
  EmptyL -> (s {running = False, runner = Nothing}, Nothing)
  w :< rest -> (s {queue = rest}, Just w)

-- | Run the loop, masked, each piece of work under @restore@.
runLoop :: Loop -> (forall a. IO a -> IO a) -> IO () -> IO ()
runLoop loop restore = go
  where
    go work = do
      -- Each piece of work reports its own failures to its fiber; what
      -- arrives here was thrown at this thread from outside (it was, say, a
      -- caller's thread that someone killed). The loop moves to a new
      -- thread and the exception goes on to its target.
      result <- try (restore work)
      case result of
        Left (e :: SomeException) -> do
          -- This thread stops being the runner before the loop is handed
          -- over: caught, the exception may bring it back to 'enter'.
          next loop >>= traverse_ (\w -> disown loop >> handOver loop w)
          throwIO e
        Right () -> next loop >>= traverse_ go

disown :: Loop -> IO ()
disown (Loop ref) = atomicModifyIORef' ref (\s -> (s {runner = Nothing}, ()))

----------------------------------------------------------------------
-- Fibers

-- | How a fiber ended.
data Outcome a
  = Done a
  | Failed SomeException
  | -- | Killed, or its component went away first.
    Cancelled

data Fiber = Fiber
  { loop :: Loop
  , over :: IORef Bool
  , worker :: IORef (Maybe ThreadId)
  -- ^ The thread running what the fiber awaits, if it awaits something.
  , branches :: IORef (IntMap Fiber)
  -- ^ The fibers running its parallel branches ('ParTurn'), which end with it.
  , nextBranch :: IORef Int
  , permitted :: IO Bool
  -- ^ Whether its owner still lets it run; checked before each step.
  , closingOk :: Bool
  -- ^ It may run while its component is being finalized: a finalizer, or
  -- a fork a finalizer started.
  , ended :: Outcome () -> IO ()
  , watchers :: IORef [Outcome () -> IO ()]
  }

-- | A fiber, not yet started. The callback hears how it ended, once, on the
-- loop.
newFiber :: Loop -> IO Bool -> Bool -> (Outcome () -> IO ()) -> IO Fiber
newFiber loop permitted closingOk ended = do
  over <- newIORef False
  worker <- newIORef Nothing
  branches <- newIORef IntMap.empty
  nextBranch <- newIORef 0
  watchers <- newIORef []
  pure Fiber {loop, over, worker, branches, nextBranch, permitted, closingOk, ended, watchers}

fiberClosingOk :: Fiber -> Bool
fiberClosingOk fb = fb.closingOk

fiberOver :: Fiber -> IO Bool
fiberOver fb = readIORef fb.over

-- | Hear how a fiber ends (at once, if it has).
onFiberOver :: Fiber -> (Outcome () -> IO ()) -> IO Bool
onFiberOver fb k = do
  isOver <- readIORef fb.over
  unless isOver $ modifyIORef' fb.watchers (k :)
  pure (not isOver)

-- | End a fiber, once: the worker it waits on, if any, is killed, and its
-- branches are cancelled.
end :: Fiber -> Outcome () -> IO ()
end fb outcome = do
  wasOver <- atomicModifyIORef' fb.over (True,)
  unless wasOver $ do
    traverse_ kill =<< atomicModifyIORef' fb.worker (Nothing,)
    traverse_ cancelFiber =<< atomicModifyIORef' fb.branches (IntMap.empty,)
    ws <- atomicModifyIORef' fb.watchers ([],)
    fb.ended outcome
    traverse_ ($ outcome) (reverse ws)
  where
    -- From another thread: 'throwTo' waits while its target masks, and the
    -- loop has better things to do.
    kill t = void $ forkIO $ throwTo t AsyncCancelled

-- | Go on with a fiber: unless it is over, or its owner no longer lets it
-- run (then it is cancelled). Checked before every piece of it runs, the
-- ones that follow synchronous work included: that work may have ended it.
proceed :: Fiber -> IO () -> IO ()
proceed fb io = do
  isOver <- readIORef fb.over
  unless isOver $ do
    ok <- fb.permitted
    if ok then io else end fb Cancelled

-- | Run a piece of a fiber now, on the loop, as 'proceed' does. A failure
-- ends it.
step :: Fiber -> IO () -> IO ()
step fb io =
  proceed fb $
    io `catch` \(e :: SomeException) -> do
      end fb (Failed e)
      -- Thrown at this thread from outside: let the loop hear it too.
      case fromException e of
        Just (SomeAsyncException _) -> throwIO e
        Nothing -> pure ()

-- | Run a program as this fiber: its first part now (it ends or suspends
-- before this returns), the rest when it resumes.
startFiber :: Fiber -> Turn a -> (a -> IO ()) -> IO ()
startFiber fb t done = step fb $ runTurn t fb (end fb . Failed) $ \a -> do
  done a
  end fb (Done ())

-- | A fiber that reports its result as well, and how to start it (so that
-- it can be registered before it runs).
fiberFor :: Loop -> IO Bool -> Bool -> (Outcome a -> IO ()) -> IO (Fiber, Turn a -> IO ())
fiberFor loop permitted closingOk outcome = do
  result <- newIORef Nothing
  fb <- newFiber loop permitted closingOk $ \case
    Done () -> readIORef result >>= outcome . maybe Cancelled Done
    Failed e -> outcome (Failed e)
    Cancelled -> outcome Cancelled
  pure (fb, \t -> startFiber fb t (writeIORef result . Just))

-- | Stop a fiber: it never runs again, and a worker it waits on is killed,
-- as are its branches'.
cancelFiber :: Fiber -> IO ()
cancelFiber fb = end fb Cancelled

----------------------------------------------------------------------
-- Programs

-- | A fiber's program, in continuation-passing style: it is given the fiber
-- it runs as and what to do with its result.
--
-- Besides its result it is given where a failure goes: the handler of the
-- innermost 'catchTurn', or, outside all of them, the end of the fiber. A
-- failure is an exception thrown by synchronous work, or by what a
-- suspension awaited.
newtype Turn a = Turn (Fiber -> (SomeException -> IO ()) -> (a -> IO ()) -> IO ())

runTurn :: Turn a -> Fiber -> (SomeException -> IO ()) -> (a -> IO ()) -> IO ()
runTurn (Turn g) = g

instance Functor Turn where
  fmap f (Turn g) = Turn $ \fb ek k -> g fb ek (k . f)

instance Applicative Turn where
  pure a = Turn $ \_ _ k -> k a
  (<*>) = ap

instance Monad Turn where
  Turn g >>= f = Turn $ \fb ek k -> g fb ek $ \a -> runTurn (f a) fb ek k

-- | Synchronous work, on the loop.
instance MonadIO Turn where
  liftIO = sync

-- | Its failure goes to the program's handler; what follows runs outside it,
-- and only if the work did not end the fiber (it may have killed it, or its
-- component).
sync :: IO a -> Turn a
sync io = Turn $ \fb ek k -> trySync io >>= proceed fb . either ek k

-- | An exception thrown at this thread from outside is not the program's to
-- handle: it goes on up (to 'step', then the loop).
trySync :: IO a -> IO (Either SomeException a)
trySync io =
  try io >>= \case
    Left e | Just (SomeAsyncException _) <- fromException e -> throwIO e
    result -> pure result

throwTurn :: SomeException -> Turn a
throwTurn e = Turn $ \_ ek _ -> ek e

-- | Handle a failure of a program, synchronous or after a suspension.
catchTurn :: Turn a -> (SomeException -> Turn a) -> Turn a
catchTurn body handler = Turn $ \fb ek k -> runTurn body fb (\e -> runTurn (handler e) fb ek k) k

currentFiber :: Turn Fiber
currentFiber = Turn $ \fb _ k -> k fb

-- | A suspension: the action runs on a worker thread, and the fiber goes on
-- through the loop's queue once it is done.
await :: IO a -> Turn a
await io = Turn $ \fb ek k -> mask_ $ do
  -- Masked until the worker is the fiber's, to be killed with it; the
  -- worker hands its result over masked too, and unmasks only the action.
  t <- forkIOWithUnmask $ \unmask -> do
    result <- try (unmask io)
    post fb.loop $ step fb $ do
      writeIORef fb.worker Nothing
      either ek k result
  -- The worker cannot come back before this: its continuation is queued
  -- behind the work running now.
  writeIORef fb.worker (Just t)

-- | A suspension until something calls back (on the loop), which may be at
-- once.
suspend :: ((Either SomeException a -> IO ()) -> IO ()) -> Turn a
suspend register = Turn $ \fb ek k -> do
  resumed <- newIORef False
  register $ \result -> do
    already <- atomicModifyIORef' resumed (True,)
    unless already $ step fb $ either ek k result

-- | Programs run side by side: each until it suspends, and the whole goes
-- on once all have finished.
--
-- Each branch is a fiber of its own (with its own worker), which ends with
-- the fiber it branches from. The first branch to fail cancels the others
-- (one not started yet never starts) and the whole fails with it; a branch
-- that is cancelled cancels the whole.
newtype ParTurn a = ParTurn (Turn a)
  deriving (Functor)

sequentialTurn :: ParTurn a -> Turn a
sequentialTurn (ParTurn t) = t

instance Applicative ParTurn where
  pure = ParTurn . pure
  ParTurn tf <*> ParTurn tx = ParTurn $ Turn $ \fb ek k -> do
    rf <- newIORef Nothing
    rx <- newIORef Nothing
    -- Set once the whole has failed or been cancelled.
    settled <- newIORef False
    let settle = atomicModifyIORef' settled (True,)
        cancelled = do
          already <- settle
          unless already $ cancelFiber fb
    bf <- branch fb cancelled
    bx <- branch fb cancelled
    let failure e = do
          already <- settle
          unless already $ do
            cancelFiber bf
            cancelFiber bx
            step fb (ek e)
        fire = do
          f <- readIORef rf
          x <- readIORef rx
          for_ (f <*> x) $ \r -> step fb (k r)
        run :: Fiber -> Turn c -> IORef (Maybe c) -> IO ()
        run b t slot =
          step b $
            runTurn t b (\e -> end b (Failed e) >> failure e) $ \a -> do
              end b (Done ())
              writeIORef slot (Just a)
              fire
    run bf tf rf
    run bx tx rx

-- | A fiber for a branch of this one: run on its loop, while its owner lets
-- it, and ended with it.
branch :: Fiber -> IO () -> IO Fiber
branch parent cancelled = do
  n <- atomicModifyIORef' parent.nextBranch (\i -> (i + 1, i))
  b <- newFiber parent.loop parent.permitted parent.closingOk $ \outcome -> do
    modifyIORef' parent.branches (IntMap.delete n)
    case outcome of
      Cancelled -> cancelled
      _ -> pure ()
  modifyIORef' parent.branches (IntMap.insert n b)
  pure b
