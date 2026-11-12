-- | The scheduler under the driver ("Halogen.IO.Driver.Runtime"), tested on
-- its own: a fiber stops as soon as it is cancelled, even by its own work;
-- the loop and a fiber's worker change hands in masked steps, so a thread
-- killed at the wrong moment neither strands the loop nor leaves a worker
-- nobody can kill; and the thread running the loop that enters it again
-- runs the new work there and then.
module Test.Runtime (spec) where

import Control.Concurrent (forkIO, killThread, rtsSupportsBoundThreads, setNumCapabilities, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (ErrorCall (..), bracket_, throwIO, toException)
import Control.Monad (forM_, forever, replicateM_, when)
import Data.Foldable (traverse_)
import Data.IORef
import Data.Text (Text)
import Halogen.IO.Driver.Runtime
import Prelude
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual)

within :: String -> IO a -> IO a
within what io = timeout 5_000_000 io >>= maybe (throwIO (ErrorCall ("timed out: " <> what))) pure

onCapabilities :: IO ()
onCapabilities = when rtsSupportsBoundThreads (setNumCapabilities 4)

-- | Run a program as a fiber on a fresh loop, and say how it ended.
runFiber :: Turn () -> IO Text
runFiber program = do
  loop <- newLoop
  ended <- newIORef "running"
  (_, start) <- fiberFor loop (pure True) False $ \case
    Done () -> writeIORef ended "done"
    Failed _ -> writeIORef ended "failed"
    Cancelled -> writeIORef ended "cancelled"
  enter loop (start program)
  readIORef ended

-- | The same, waiting until it ends (it may resume on a worker's behalf).
awaitFiber :: Turn () -> IO Text
awaitFiber program = do
  loop <- newLoop
  ended <- newEmptyMVar
  (_, start) <- fiberFor loop (pure True) False $ \case
    Done () -> putMVar ended "done"
    Failed _ -> putMVar ended "failed"
    Cancelled -> putMVar ended "cancelled"
  enter loop (start program)
  within "the fiber to end" (takeMVar ended)

-- | A branch that fails outside its error continuation (while its next
-- part is being made) fails the whole, and stops its sibling.
branchThrows :: IO ()
branchThrows = do
  touched <- newIORef []
  let throwsNext :: Turn () -> Turn ()
      throwsNext before = before >>= \_ -> error "branch continuation"
      sibling = sync (threadDelay 0) >> sync (modifyIORef' touched (<> ["sibling" :: Text]))
      both first second = sequentialTurn $ (\_ _ -> ()) <$> ParTurn first <*> ParTurn second
  runFiber (both (throwsNext (sync (pure ()))) (pure ())) >>= assertEqual "failing at once" "failed"
  runFiber (both (throwsNext (sync (pure ()))) sibling) >>= assertEqual "failing at once, sibling after" "failed"
  awaitFiber (both (throwsNext (await (pure ()))) (pure ())) >>= assertEqual "failing after a wait" "failed"
  gate <- newEmptyMVar
  awaitFiber (both (throwsNext (await (pure ()))) (await (takeMVar gate) >> sibling)) >>= assertEqual "failing while the sibling waits" "failed"
  _ <- tryPutMVar gate ()
  threadDelay 10_000
  readIORef touched >>= assertEqual "no sibling went on" []

selfCancel :: IO ()
selfCancel = do
  touched <- newIORef []
  let touch t = sync (modifyIORef' touched (<> [t :: Text]))
  ended <- runFiber $ do
    self <- currentFiber
    sync (cancelFiber self)
    touch "after"
  assertEqual "ended" "cancelled" ended
  -- Failing work that cancelled the fiber: the handler does not run either.
  failing <- runFiber $ do
    self <- currentFiber
    catchTurn (sync (cancelFiber self >> throwIO (ErrorCall "boom"))) $ \_ -> touch "handler"
    touch "after"
  assertEqual "ended" "cancelled" failing
  -- Nor does it start what it would wait for.
  awaiting <- runFiber $ do
    self <- currentFiber
    sync (cancelFiber self)
    await (modifyIORef' touched (<> ["awaited"]))
  assertEqual "ended" "cancelled" awaiting
  threadDelay 10_000
  readIORef touched >>= assertEqual "nothing ran after the cancelling work" []

nested :: IO ()
nested = do
  loop <- newLoop
  logRef <- newIORef []
  let note t = modifyIORef' logRef (<> [t :: Text])
  enter loop $ do
    enter loop (note "inner")
    note "outer"
  readIORef logRef >>= assertEqual "the new work ran inside the old" ["inner", "outer"]
  -- A branch that fails while it runs cancels the others, which never start.
  outcome <- runFiber $ sequentialTurn $ (\_ _ -> ()) <$> ParTurn (throwTurn (toException (ErrorCall "first"))) <*> ParTurn (sync (note "second"))
  assertEqual "the whole failed" "failed" outcome
  readIORef logRef >>= assertEqual "the second branch did not start" ["inner", "outer"]

-- | A thread that brings work while another runs the loop queues it and
-- goes on: neither 'enter' nor 'enterCallback' waits for the work. (A
-- browser event's handler on WebAssembly does wait while the runner is
-- runnable, see 'enterCallback'; natively it is 'enter'.)
queuesBehind :: IO ()
queuesBehind = do
  loop <- newLoop
  holding <- newEmptyMVar
  release <- newEmptyMVar
  logRef <- newIORef []
  let note t = atomicModifyIORef' logRef (\l -> (l <> [t :: Text], ()))
  _ <- forkIO $ enter loop $ do
    putMVar holding ()
    takeMVar release
    note "holder"
  takeMVar holding
  within "enter on a held loop" (enter loop (note "entered"))
  within "enterCallback on a held loop" (enterCallback loop (note "callback"))
  readIORef logRef >>= assertEqual "nothing queued ran yet" []
  done <- newEmptyMVar
  enter loop (putMVar done ())
  putMVar release ()
  within "the queued work" (takeMVar done)
  readIORef logRef >>= assertEqual "the queued work ran after the holder's, in order" ["holder", "entered", "callback"]

-- | Threads that take the loop (or hand it to a runner) and are killed while
-- they do: the loop is never left taken with nobody to run it.
loopHandOff :: IO ()
loopHandOff = do
  onCapabilities
  loop <- newLoop
  forM_ [1 :: Int .. 1000] $ \i -> do
    started <- newEmptyMVar
    t <- forkIO $ do
      putMVar started ()
      forever $ if even i then enter loop (pure ()) else post loop (pure ())
    takeMVar started
    threadDelay (i `mod` 7)
    killThread t
  done <- newEmptyMVar
  enter loop (putMVar done ())
  within "work entered after the kills" (takeMVar done)

-- | Threads killed while they start fibers that wait: every worker started
-- belongs to its fiber, and goes when the fiber is cancelled.
workerHandOff :: IO ()
workerHandOff = do
  onCapabilities
  loop <- newLoop
  live <- newIORef (0 :: Int)
  fibers <- newIORef []
  let waiting = bracket_ (atomicModifyIORef' live (\n -> (n + 1, ()))) (atomicModifyIORef' live (\n -> (n - 1, ()))) (forever (threadDelay 1_000_000))
  forM_ [1 :: Int .. 200] $ \i -> do
    started <- newEmptyMVar
    t <- forkIO $ do
      putMVar started ()
      replicateM_ 20 $ enter loop $ do
        (fb, start) <- fiberFor loop (pure True) False (const (pure ()))
        modifyIORef' fibers (fb :)
        start (await waiting)
    takeMVar started
    threadDelay (i `mod` 11)
    killThread t
  done <- newEmptyMVar
  enter loop (readIORef fibers >>= traverse_ cancelFiber >> putMVar done ())
  within "the cancellation" (takeMVar done)
  let settled = do
        n <- readIORef live
        when (n > 0) (threadDelay 1_000 >> settled)
  within "every worker to go" settled

-- | A suspension that is not counted ('awaitUncounted', a pause): the loop
-- reads as idle with nobody waiting while it sleeps, it resumes through the
-- loop as 'await' does, and its worker goes with its fiber.
uncounted :: IO ()
uncounted = do
  loop <- newLoop
  -- What happens after a fiber's last step (the loop giving up, a worker
  -- leaving) may still be under way as the test hears of that step.
  let comesTo what expected = within what $ do
        let go = loopActivity loop >>= \a -> when (a /= expected) (threadDelay 1_000 >> go)
        go
  -- Resumes: the continuation runs, after the action, on the loop.
  resumed <- newEmptyMVar
  sleeping <- newEmptyMVar
  wake <- newEmptyMVar
  (_, start) <- fiberFor loop (pure True) False (const (pure ()))
  enter loop $ start $ do
    awaitUncounted (putMVar sleeping () >> takeMVar wake)
    sync (putMVar resumed ())
  within "the pause to start" (takeMVar sleeping)
  loopActivity loop >>= assertEqual "neither running nor waiting while it sleeps" (False, 0)
  putMVar wake ()
  within "the fiber to resume" (takeMVar resumed)
  comesTo "nothing left once it resumed" (False, 0)
  -- A counted suspension beside it is still counted.
  counted <- newEmptyMVar
  release <- newEmptyMVar
  (_, startCounted) <- fiberFor loop (pure True) False (const (putMVar counted ()))
  enter loop (startCounted (await (takeMVar release)))
  loopActivity loop >>= assertEqual "a counted fiber is waiting" (False, 1)
  putMVar release ()
  within "the counted fiber to end" (takeMVar counted)
  comesTo "nothing counted once it is back" (False, 0)
  -- Cancelled: its worker is killed, and what follows never runs.
  live <- newIORef (0 :: Int)
  after <- newIORef False
  (fb, startKilled) <- fiberFor loop (pure True) False (const (pure ()))
  enter loop $ startKilled $ do
    awaitUncounted (bracket_ (atomicModifyIORef' live (\n -> (n + 1, ()))) (atomicModifyIORef' live (\n -> (n - 1, ()))) (threadDelay 60_000_000))
    sync (writeIORef after True)
  let started = readIORef live >>= \n -> when (n == 0) (threadDelay 1_000 >> started)
  within "the pause's worker to start" started
  enter loop (cancelFiber fb)
  let gone = readIORef live >>= \n -> when (n > 0) (threadDelay 1_000 >> gone)
  within "the pause's worker to go" gone
  readIORef after >>= assertEqual "nothing after a cancelled pause ran" False
  comesTo "nothing counted after the cancel" (False, 0)

spec :: Spec
spec =
  describe "runtime" $ do
    it "runs nothing more of a fiber once its own work cancelled it" selfCancel
    it "runs work the loop's own thread enters inside the work running, and stops parallel siblings of a failure" nested
    it "fails the whole when a parallel branch fails outside its error continuation" branchThrows
    it "queues work brought while another thread runs the loop, without waiting for it" queuesBehind
    it "never strands the loop when the thread taking it is killed" loopHandOff
    it "kills every worker of a cancelled fiber, whenever its starter was killed" workerHandOff
    it "does not count a fiber in an uncounted suspension, resumes it, and kills its worker with it" uncounted
