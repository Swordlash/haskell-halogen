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

spec :: Spec
spec =
  describe "runtime" $ do
    it "runs nothing more of a fiber once its own work cancelled it" selfCancel
    it "runs work the loop's own thread enters inside the work running, and stops parallel siblings of a failure" nested
    it "never strands the loop when the thread taking it is killed" loopHandOff
    it "kills every worker of a cancelled fiber, whenever its starter was killed" workerHandOff
