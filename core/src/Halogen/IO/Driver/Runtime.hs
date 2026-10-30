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
--   thread running the loop brings it itself. And on WebAssembly a browser
--   event that finds the loop held by a thread that was merely preempted
--   lets that thread finish, inside the dispatch ('enterCallback').
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

import Control.Concurrent (ThreadId, forkIO, forkIOWithUnmask, myThreadId, throwTo, yield)
import Control.Exception (SomeAsyncException (..), SomeException, catch, finally, fromException, mask, mask_, onException, throwIO, try)
import Control.Monad (ap, unless, void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_, traverse_)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (isNothing)
import Data.Sequence (Seq, ViewL (..), viewl, (<|), (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Conc (ThreadStatus (..), threadStatus)
import Halogen.JSBits (inSyncCallback)
import Prelude
import UnliftIO (AsyncCancelled (..))

----------------------------------------------------------------------
-- The loop

-- | Whether a thread is running the loop (and which), and what waits for it.
data LoopState = LoopState
  { running :: !Bool
  , runner :: !(Maybe ThreadId)
  -- ^ While the loop runs, 'Nothing' only as it passes to a thread about
  -- to be forked to run it: the thread is recorded as soon as it exists,
  -- before it has started (see 'handOver'). Meaningless while it does not.
  , handOvers :: !Int
  -- ^ How many times the loop has been taken to be handed to a new thread
  -- ('handing'), so that 'handOver' records only the thread of the latest.
  , effects :: !Int
  -- ^ How many synchronous effects of components ('withinEffect') are
  -- under way on the loop.
  , queue :: !(Seq (IO ()))
  , workers :: !(Set ThreadId)
  -- ^ The threads running what fibers await: killed with their fibers, so
  -- they must never run the loop ('post' for them, whoever calls 'enter').
  }

newtype Loop = Loop (IORef LoopState)

newLoop :: IO Loop
newLoop = Loop <$> newIORef (LoopState False Nothing 0 0 Seq.empty Set.empty)

-- | How a thread entered the loop: it took it ('Claimed'), ran the work
-- inside the work running ('Nested'), queued it behind the thread running
-- the loop ('Behind'), queued it as a worker, which never waits ('Queued'),
-- or took the loop for a thread it starts ('Handed').
data Entry = Claimed | Nested | Behind | Queued | Handed

-- | Have the loop run a piece of work: on this thread and at once when
-- nobody runs the loop (and then whatever is queued meanwhile), otherwise
-- after what is queued already. Never waits (unlike 'enterCallback' on
-- WebAssembly).
--
-- The thread running the loop that enters it again, from inside a piece of
-- work (an emitter a 'Halogen.Query.HalogenM.liftEffect' notifies, say),
-- runs the new work there and then, nested in the old.
--
-- The work runs in the masking state of the thread that entered.
enter :: Loop -> IO () -> IO ()
enter = enterWith False

-- | 'enter', for a browser event's handler: an element's listener, or an
-- emitter a component subscribed to (one 'Halogen.Query.Event.eventListener'
-- made, say), when the page calls it. A browser runs an event's handlers
-- while it dispatches it, so an event a component dispatches from a
-- synchronous effect is handled before the effect goes on: the handler may
-- still prevent the event's default action, or stop its propagation. In
-- the browser the handler runs on a thread of its own while the thread
-- running the loop waits for the dispatch to return, so there it is nested
-- whenever the loop is inside a synchronous effect ('withinEffect'). (There
-- is only ever one thread running, and a handler is called either from the
-- page's own event loop, with no effect under way, or by the effect.)
--
-- On WebAssembly the loop can also be held by a thread that is runnable
-- but not running: the scheduler hands the page back after a slice of
-- time with threads still runnable, and runs them again in a later task
-- (a callback the page makes, likewise, returns with the threads it woke
-- still runnable), so a long render can be cut in two with an event
-- dispatched in between.
-- Nothing of the loop is on the JavaScript stack under such an event, so
-- there the handler lets that thread go on ('yield'), inside the dispatch,
-- until the work it queued has run, and returns only then (see
-- 'letRunnerFinish').
--
-- All of this is so only for a thread the page called: one running a
-- callback made with 'Halogen.JSBits.mkSyncCallback', as every DOM
-- listener is ('Halogen.JSBits.inSyncCallback'). Any other thread that
-- comes here is given 'enter', whatever the loop is doing:
--
-- * a worker (a 'liftIO' body that notifies an emitter) never runs the
--   loop itself, nor waits, as 'enter' has it;
--
-- * the thread running the loop runs the work there and then, nested, as
--   'enter' has it too;
--
-- * any other thread (a timer's, such as
--   'Halogen.Query.Event.Window.interval''s, or one a program forked
--   itself) runs the work at once only when the loop is idle, and
--   otherwise queues it and goes on. Such a thread is not under the effect
--   on the stack, whatever it finds: on WebAssembly it may run while the
--   effect's thread is blocked in the call that dispatched an event (another
--   thread is then running that event's handler), and on the JavaScript
--   backend while the effect's thread is merely preempted. Nested, its work
--   would run beside the effect, or beside the handler, rather than inside
--   it.
--
-- Natively this is always 'enter': there the DOM ("Halogen.VDom.DOM.Monad.Native")
-- runs a listener on the thread that dispatches the event, so an event an
-- effect dispatches is nested because the loop's own thread brings it.
enterCallback :: Loop -> IO () -> IO ()
enterCallback loop work = do
  called <- if browser then inSyncCallback else pure False
  enterWith called loop work
  where
#if !defined(HALOGEN_WASI) && (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
    browser = True
#else
    browser = False
#endif

-- | Whether a browser event's handler that finds the loop held by a
-- runnable thread lets it finish, inside the dispatch. Only on WebAssembly,
-- where a 'yield' inside a synchronous callback runs the other runnable
-- threads before the callback returns. On the JavaScript backend a
-- callback that yields is carried on asynchronously
-- (@syncCallback1 ContinueAsync@): it would return to the browser at the
-- first 'yield', its event dispatched, so waiting would gain nothing.
waitsForRunner :: Bool
#if defined(wasm32_HOST_ARCH) && !defined(HALOGEN_WASI)
waitsForRunner = True
#else
waitsForRunner = False
#endif

enterWith :: Bool -> Loop -> IO () -> IO ()
enterWith callback loop@(Loop ref) work0 = do
  me <- myThreadId
  -- Set once queued work has run (or failed), for a handler that waits for
  -- it below.
  ran <- newIORef False
  let waits = callback && waitsForRunner
      work = if waits then work0 `finally` writeIORef ran True else work0
  -- A browser event's handler runs nested in an effect under way only when
  -- the effect's thread is under it on the stack. Where handlers wait for
  -- a runnable runner, that thread may instead have been preempted inside
  -- the effect, between two of the page's tasks; nested, the handler's
  -- work would run beside the effect, not inside it. There only a runner
  -- that is not runnable (blocked in the call that dispatched the event)
  -- is under this thread. Read before the state is changed below: the
  -- runner cannot change its status without running, and it does not run
  -- between here and there.
  under <- if waits then runnerStuck else pure True
  -- Masked from taking the loop to running it: the loop must not be left
  -- taken with nobody to run it.
  behind <- mask $ \restore -> do
    entry <- atomicModifyIORef' ref $ \s ->
      if
        -- A worker (a 'liftIO' body that notifies an emitter, say) is
        -- killed with its fiber: were it to run the loop, the work that
        -- cancels that fiber would cut itself short. It queues, as 'post'.
        | Set.member me s.workers ->
            if s.running
              then (s {queue = s.queue |> work}, Queued)
              else (handing s, Handed)
        | not s.running -> (s {running = True, runner = Just me}, Claimed)
        | maybe False (sameThread me) s.runner || (callback && s.effects > 0 && under) -> (s, Nested)
        | otherwise -> (s {queue = s.queue |> work}, Behind)
    case entry of
      Claimed -> False <$ runLoop loop restore work
      Nested -> False <$ restore work
      Behind -> pure True
      Queued -> pure False
      Handed -> False <$ handOver loop work
  -- Outside the mask: the work is queued already, so nothing is left
  -- half-done if this thread is interrupted while it waits, and an
  -- exception thrown at it (the page's, or a 'killThread') lands at once
  -- rather than when the wait is over. Masked, a 'yield' would hand over
  -- just the same, so masking would only delay such an exception.
  when (behind && waits) $ letRunnerFinish loop ran
  where
    -- Whether the thread running the loop is not runnable: blocked in a
    -- foreign call (the one that dispatched the event being handled, under
    -- this thread on the stack), or otherwise, or finished. A loop nobody
    -- runs, or one being handed to a thread not yet forked, has no such
    -- thread.
    runnerStuck = do
      s <- readIORef ref
      case s.runner of
        Just r | s.running -> (/= ThreadRunning) <$> threadStatus r
        _ -> pure False

-- | Yield to the thread running the loop until the work this thread queued
-- has run, for as long as yielding lets that thread go on.
--
-- What the runner is doing decides: if it is runnable ('ThreadRunning') it
-- was preempted between pieces of the page's work, nothing of it is on the
-- JavaScript stack, and a 'yield' lets it run on. Blocked
-- ('BlockedOnForeignCall'), it is the one that made the call that
-- dispatched this event (a render that focuses an element, say), under
-- this thread on the stack: it cannot go on until this handler returns.
-- Blocked otherwise, or finished, it is waiting for something only the
-- page can bring. In those cases the work stays queued, to run once the
-- dispatch is over, as before.
--
-- The state is read again at every turn: the loop may change hands while
-- this thread waits. A loop taken with no runner recorded is being handed
-- to a new thread (between 'disown' and 'handOver'), which is about to be
-- recorded, so that counts as running.
letRunnerFinish :: Loop -> IORef Bool -> IO ()
letRunnerFinish (Loop ref) ran = go
  where
    go = do
      done <- readIORef ran
      unless done $ do
        s <- readIORef ref
        if
          | not s.running -> pure ()
          | Just r <- s.runner ->
              threadStatus r >>= \case
                ThreadRunning -> yield >> go
                _ -> pure ()
          | otherwise -> yield >> go

-- | 'compare', not '==': GHC's JavaScript runtime has the @cmp_thread@ that
-- 'Ord' 'ThreadId' calls, but not the @eq_thread@ that 'Eq' calls.
sameThread :: ThreadId -> ThreadId -> Bool
sameThread a b = compare a b == EQ

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
      else (handing s, True)
  when mine $ handOver loop work

-- | Start a thread to run the loop, which this thread has taken, beginning
-- with this work. Called masked. If no thread can be started the loop is
-- given up, with the work back at the front of its queue for whoever
-- enters next.
--
-- The loop is taken for it beforehand, with 'handing'.
--
-- The new thread is recorded as the runner twice: here, as soon as it
-- exists, so that a loop handed over but not yet started shows who holds it
-- (a browser event's handler on WebAssembly waits for a runnable runner),
-- and by the thread itself as it starts. The thread may run first, and even
-- run the loop to its end, before this records it; so this records it only
-- if no runner is recorded and the loop has not been taken for another
-- hand-over since, or it could name a finished thread as the runner of
-- someone else's turn. A record left on a loop nobody runs is harmless:
-- whoever takes the loop next records itself.
handOver :: Loop -> IO () -> IO ()
handOver loop@(Loop ref) work = do
  -- Nobody else can take the loop for a hand-over until this one's thread
  -- has run it, so this is the count of this hand-over.
  n <- (.handOvers) <$> readIORef ref
  t <-
    forkIOWithUnmask (\unmask -> adopt >> runLoop loop unmask work)
      `onException` atomicModifyIORef' ref (\s -> (s {running = False, runner = Nothing, queue = work <| s.queue}, ()))
  atomicModifyIORef' ref $ \s ->
    if s.handOvers == n && isNothing s.runner then (s {runner = Just t}, ()) else (s, ())
  where
    adopt = do
      me <- myThreadId
      atomicModifyIORef' ref (\s -> (s {runner = Just me}, ()))

-- | Take the loop for a thread that is about to be forked to run it (with
-- 'handOver'): it is running, by no thread yet.
handing :: LoopState -> LoopState
handing s = s {running = True, runner = Nothing, handOvers = s.handOvers + 1}

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
disown (Loop ref) = atomicModifyIORef' ref (\s -> (handing s, ()))

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
    -- Noted by the worker itself, still masked, so that it is a worker
    -- before anything it runs can enter the loop.
    me <- myThreadId
    noteWorker fb.loop Set.insert me
    result <- try (unmask io) `finally` noteWorker fb.loop Set.delete me
    post fb.loop $ step fb $ do
      writeIORef fb.worker Nothing
      either ek k result
  -- The worker cannot come back before this: its continuation is queued
  -- behind the work running now.
  writeIORef fb.worker (Just t)

-- | Note or forget a worker thread of the loop.
noteWorker :: Loop -> (ThreadId -> Set ThreadId -> Set ThreadId) -> ThreadId -> IO ()
noteWorker (Loop ref) f t = atomicModifyIORef' ref (\s -> (s {workers = f t s.workers}, ()))

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
    siblings <- newIORef []
    let settle = atomicModifyIORef' settled (True,)
        -- How a branch ended decides for the whole, however it came to end:
        -- through its error continuation, or a failure 'step' caught (one
        -- thrown while the next part of the branch was being made, say).
        ended = \case
          Done () -> pure ()
          Failed e -> do
            already <- settle
            unless already $ do
              traverse_ cancelFiber =<< readIORef siblings
              step fb (ek e)
          Cancelled -> do
            already <- settle
            unless already $ cancelFiber fb
        fire = do
          f <- readIORef rf
          x <- readIORef rx
          for_ (f <*> x) $ \r -> step fb (k r)
        run :: Fiber -> Turn c -> IORef (Maybe c) -> IO ()
        run b t slot =
          step b $
            runTurn t b (end b . Failed) $ \a -> do
              end b (Done ())
              writeIORef slot (Just a)
              fire
    bf <- branch fb ended
    bx <- branch fb ended
    writeIORef siblings [bf, bx]
    run bf tf rf
    run bx tx rx

-- | A fiber for a branch of this one: run on its loop, while its owner lets
-- it, and ended with it.
branch :: Fiber -> (Outcome () -> IO ()) -> IO Fiber
branch parent ended = do
  n <- atomicModifyIORef' parent.nextBranch (\i -> (i + 1, i))
  b <- newFiber parent.loop parent.permitted parent.closingOk $ \outcome -> do
    modifyIORef' parent.branches (IntMap.delete n)
    ended outcome
  modifyIORef' parent.branches (IntMap.insert n b)
  pure b
