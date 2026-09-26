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
--   work at a time, until nothing is queued.
--
-- * A component's program runs as a 'Fiber': a continuation, driven in
--   'Turn'. Its synchronous parts run on the loop; at a suspension ('await')
--   the rest is set aside, the awaited action runs on a worker thread, and
--   the continuation comes back through the loop's queue. Nothing else ever
--   waits: a fiber waiting for another (a join, a child's answer) leaves a
--   callback and returns.
--
-- * A fiber can be cancelled: it never runs again, and its worker, if it has
--   one, is killed. A worker never runs the loop, so killing it cannot cut a
--   piece of the tree's work in half.
module Halogen.IO.Driver.Runtime
  ( -- * The loop
    Loop
  , newLoop
  , enter
  , post

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

import Control.Concurrent (ThreadId, forkIO, forkIOWithUnmask, throwTo)
import Control.Exception (SomeAsyncException (..), SomeException, catch, fromException, mask, throwIO, try)
import Control.Monad (ap, unless, void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_, traverse_)
import Data.IORef
import Data.Sequence (Seq, ViewL (..), viewl, (|>))
import Data.Sequence qualified as Seq
import Prelude
import UnliftIO (AsyncCancelled (..))

----------------------------------------------------------------------
-- The loop

-- | Whether a thread is running the loop, and what waits for it.
data LoopState = LoopState
  { running :: !Bool
  , queue :: !(Seq (IO ()))
  }

newtype Loop = Loop (IORef LoopState)

newLoop :: IO Loop
newLoop = Loop <$> newIORef (LoopState False Seq.empty)

-- | Have the loop run a piece of work: on this thread and at once when
-- nobody runs the loop (and then whatever is queued meanwhile), otherwise
-- after what is queued already. Never waits.
enter :: Loop -> IO () -> IO ()
enter loop work = do
  mine <- claim loop work
  when mine $ runLoop loop work

-- | The same, for a thread that must not run the loop itself (a worker,
-- which is killed with its fiber): when nobody runs the loop, a new thread
-- starts it.
post :: Loop -> IO () -> IO ()
post loop work = do
  mine <- claim loop work
  when mine $ void $ forkIO $ runLoop loop work

-- | Take the loop to run this work, or queue it.
claim :: Loop -> IO () -> IO Bool
claim (Loop ref) work = atomicModifyIORef' ref $ \s ->
  if s.running
    then (s {queue = s.queue |> work}, False)
    else (s {running = True}, True)

-- | The next piece of work, or, with none, give the loop up (in the same
-- step, so that work queued just then is not stranded).
next :: Loop -> IO (Maybe (IO ()))
next (Loop ref) = atomicModifyIORef' ref $ \s -> case viewl s.queue of
  EmptyL -> (s {running = False}, Nothing)
  w :< rest -> (s {queue = rest}, Just w)

runLoop :: Loop -> IO () -> IO ()
runLoop loop first = mask $ \restore ->
  let go work = do
        -- Each piece of work reports its own failures to its fiber; what
        -- arrives here was thrown at this thread from outside (it was, say,
        -- a caller's thread that someone killed). The loop moves to a new
        -- thread and the exception goes on to its target.
        result <- try (restore work)
        case result of
          Left (e :: SomeException) -> do
            next loop >>= traverse_ (void . forkIO . runLoop loop)
            throwIO e
          Right () -> next loop >>= traverse_ go
   in go first

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
  watchers <- newIORef []
  pure Fiber {loop, over, worker, permitted, closingOk, ended, watchers}

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

-- | End a fiber, once.
end :: Fiber -> Outcome () -> IO ()
end fb outcome = do
  wasOver <- atomicModifyIORef' fb.over (True,)
  unless wasOver $ do
    writeIORef fb.worker Nothing
    ws <- atomicModifyIORef' fb.watchers ([],)
    fb.ended outcome
    traverse_ ($ outcome) (reverse ws)

-- | Run a piece of a fiber now, on the loop: unless it is over, or its
-- owner no longer lets it run (then it is cancelled). A failure ends it.
step :: Fiber -> IO () -> IO ()
step fb io = do
  isOver <- readIORef fb.over
  unless isOver $ do
    ok <- fb.permitted
    if not ok
      then end fb Cancelled
      else
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

-- | Stop a fiber: it never runs again, and a worker it waits on is killed.
cancelFiber :: Fiber -> IO ()
cancelFiber fb = do
  w <- readIORef fb.worker
  end fb Cancelled
  -- From another thread: 'throwTo' waits while its target masks, and the
  -- loop has better things to do.
  for_ w $ \t -> void $ forkIO $ throwTo t AsyncCancelled

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

-- | Its failure goes to the program's handler; what follows runs outside it.
sync :: IO a -> Turn a
sync io = Turn $ \_ ek k -> trySync io >>= either ek k

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
await io = Turn $ \fb ek k -> do
  t <- forkIOWithUnmask $ \unmask -> do
    result <- try (unmask io)
    post fb.loop $ step fb $ either ek k result
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

-- | Programs run side by side within one fiber: each until it suspends, and
-- the whole goes on once all have finished.
newtype ParTurn a = ParTurn (Turn a)
  deriving (Functor)

sequentialTurn :: ParTurn a -> Turn a
sequentialTurn (ParTurn t) = t

instance Applicative ParTurn where
  pure = ParTurn . pure
  ParTurn tf <*> ParTurn tx = ParTurn $ Turn $ \fb ek k -> do
    rf <- newIORef Nothing
    rx <- newIORef Nothing
    -- The first failure is the whole's; then neither side goes on with it.
    failed <- newIORef False
    let failure e = do
          already <- atomicModifyIORef' failed (True,)
          unless already $ ek e
        fire = do
          bad <- readIORef failed
          f <- readIORef rf
          x <- readIORef rx
          unless bad $ for_ (f <*> x) k
    runTurn tf fb failure $ \f -> writeIORef rf (Just f) >> fire
    runTurn tx fb failure $ \x -> writeIORef rx (Just x) >> fire
