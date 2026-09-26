-- | Regression test: a component's state update is atomic.
--
-- The driver used to evaluate @H.modify@ by reading the whole DriverState,
-- applying the function, and writing the whole record back. A thread that
-- ran in between (GHC's RTS preempts threads, wasm's included) had its own
-- write undone: another state update was lost, or a render pass's children
-- and rendering were rolled back, after which the next render re-minted live
-- children ("Duplicate slot address was detected during rendering") and left
-- the old ones running.
--
-- The test parks one update in the middle of its function, lets a second
-- update run meanwhile, then releases the first. Both increments must be
-- there. With the old driver the second is overwritten and the count is 1.
module Test.DriverStateAtomic (spec) where

import Control.Concurrent (forkIO, rtsSupportsBoundThreads, setNumCapabilities, threadDelay, yield)
import Control.Exception (evaluate)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, replicateM_, void, when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Control.Monad.State.Class (get, modify, state)
import Data.Kind (Type)
import Data.Row (Empty, Row)
import Data.Void (Void)
import Halogen as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Core qualified as HC
import Halogen.IO.Driver qualified as AD
import Halogen.IO.Driver.State (Leave (..), RenderGate (..), RenderStateX (..), beginPass, drainQueue, enterRender, idleGate, leaveRender, nextToDrain, runOrQueue)
import Halogen.Query.Input (Input)
import Prelude
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual)

-- | No DOM: nothing to walk, the component has no slots.
data NoRender (s :: Type) (act :: Type) (ps :: Row Type) (o :: Type) = NoRender

noRender :: AD.RenderSpec IO NoRender
noRender =
  AD.RenderSpec
    { AD.render = render
    , AD.renderChild = id
    , AD.removeChild = \_ -> pure ()
    , AD.dispose = \_ -> pure ()
    }
  where
    render
      :: (Input act -> IO ())
      -> (ComponentSlotBox ps IO act -> IO (RenderStateX NoRender))
      -> HC.HTML (ComponentSlot ps IO act) act
      -> Maybe (NoRender s act ps o)
      -> IO (NoRender s act ps o)
    render _ _ _ _ = pure NoRender

data Query a
  = Parked a
  | Plain a
  | Current (Int -> a)

data Gates = Gates
  { entered :: MVar ()
  -- ^ The parked update has read the state and is inside its function.
  , release :: MVar ()
  }

-- | Stop inside the state function until released; the result depends on
-- the argument, so the call is not floated out.
park :: Gates -> Int -> Int
park gates n = unsafePerformIO $ do
  putMVar gates.entered ()
  takeMVar gates.release
  pure (n + 1)
{-# NOINLINE park #-}

component :: Gates -> H.Component Query () Void IO
component gates =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure (0 :: Int)
      , render = \_ -> HH.text "" :: H.ComponentHTML Void Empty IO
      , eval = H.mkEval H.defaultEval {handleQuery = answer}
      }
  where
    answer :: Query a -> H.HalogenM Int Void Empty Void IO (Maybe a)
    answer = \case
      -- Like any update that works out its result before handing it back
      -- (a condition on the state, say): the pair is only ready once the
      -- work is done, and here the work waits for the test.
      Parked a -> state (\n -> let n' = park gates n in n' `seq` ((), n')) >> pure (Just a)
      Plain a -> modify (+ 1) >> pure (Just a)
      Current k -> Just . k <$> get

test :: IO ()
test = do
  gates <- Gates <$> newEmptyMVar <*> newEmptyMVar
  AD.HalogenSocket {AD.query = ask, AD.dispose = dispose} <- AD.runUI noRender (component gates) ()

  parkedDone <- newEmptyMVar
  plainDone <- newEmptyMVar
  _ <- forkIO $ ask (Parked ()) >> putMVar parkedDone ()
  takeMVar gates.entered
  -- The second update runs while the first is inside its function. With
  -- an atomic update it waits for the first; without, it finishes first
  -- and the first then writes back the state it had read.
  _ <- forkIO $ ask (Plain ()) >> putMVar plainDone ()
  threadDelay 50000
  putMVar gates.release ()
  takeMVar parkedDone
  takeMVar plainDone

  current <- ask (Current id)
  assertEqual "both updates kept" (Just 2) current
  dispose

-- | The render lock, step by step.
gateSteps :: IO ()
gateSteps = do
  gate <- newIORef idleGate
  ran <- newIORef []
  let act n = atomicModifyIORef' ran (\ns -> (ns <> [n :: Int], ()))
      drain acts = sequence_ acts >> nextToDrain gate >>= mapM_ drain
      leave = leaveRender gate >>= \case
        Drain acts -> drain acts >> pure "drain"
        Again -> pure "again"
        Done -> pure "done"
  runOrQueue gate (act 0)
  readIORef ran >>= assertEqual "with no render, an action runs at once" [0]
  enterRender gate >>= assertEqual "the first render takes the lock" True
  enterRender gate >>= assertEqual "a second render waits for the first" False
  runOrQueue gate (act 1)
  runOrQueue gate (act 2)
  readIORef ran >>= assertEqual "during a render, actions wait" [0]
  leave >>= assertEqual "the queue comes out first" ("drain" :: String)
  readIORef ran >>= assertEqual "in the order they came" [0, 1, 2]
  leave >>= assertEqual "then the pass asked for" "again"
  beginPass gate
  leave >>= assertEqual "and then the lock goes" "done"
  enterRender gate >>= assertEqual "for the next render to take" True

-- | An action raised while queued ones have yet to start waits behind them,
-- and actions come out in the order they were raised.
gateOrder :: IO ()
gateOrder = do
  gate <- newIORef idleGate
  ran <- newIORef []
  let act n = atomicModifyIORef' ran (\ns -> (ns <> [n :: Int], ()))
  _ <- enterRender gate
  runOrQueue gate (act 1)
  batch <-
    leaveRender gate >>= \case
      Drain acts -> pure acts
      _ -> pure []
  length batch `shouldBe'` 1
  leaveRender gate >>= \case
    Done -> pure ()
    _ -> assertEqual "the render lets the lock go" True False
  -- The render is over, but the first action has not started yet.
  runOrQueue gate (act 2)
  readIORef ran >>= assertEqual "the second waits for the first" []
  sequence_ batch
  nextToDrain gate >>= \case
    Just next -> sequence_ next
    Nothing -> assertEqual "the second is handed to the thread draining" True False
  nextToDrain gate >>= assertEqual "and then the draining is over" Nothing . fmap length
  readIORef ran >>= assertEqual "in the order they were raised" [1, 2]
  runOrQueue gate (act 3)
  readIORef ran >>= assertEqual "with nothing waiting, an action runs at once" [1, 2, 3]
  where
    shouldBe' a b = assertEqual "one action queued" b a

-- | The real drainer, on four capabilities where the runtime has them: an
-- action that computes before it records (and never waits) still records
-- before the one queued after it starts.
drainOrder :: IO ()
drainOrder = do
  when rtsSupportsBoundThreads (setNumCapabilities 4)
  work <- newIORef (300000 :: Int)
  forM_ [1 .. 20 :: Int] $ \_ -> do
    gate <- newIORef idleGate
    ran <- newIORef []
    let record n = atomicModifyIORef' ran (\ns -> (ns <> [n :: Int], ()))
        -- Work that no optimisation can share between calls, and no wait.
        busy = readIORef work >>= \n -> void (evaluate (foldl' (+) 0 [1 .. n]))
    drainQueue gate [busy >> record 1, record 2, busy >> record 3, record 4]
    let settle k = do
          ns <- readIORef ran
          when (length ns < 4 && k > (0 :: Int)) (threadDelay 1000 >> settle (k - 1))
    settle 5000
    readIORef ran >>= assertEqual "each action recorded before the next began" [1, 2, 3, 4]
    readIORef gate >>= \g -> assertEqual "and the draining is over" False g.draining

-- | Actions raised from many threads while renders come and go: each one
-- runs, and runs once.
gateStress :: IO ()
gateStress = do
  gate <- newIORef idleGate
  count <- newIORef (0 :: Int)
  let threads = 8
      each = 2000
      bump = atomicModifyIORef' count (\n -> (n + 1, ()))
      drain acts = sequence_ acts >> nextToDrain gate >>= mapM_ drain
      render = do
        took <- enterRender gate
        when took $ do
          let pass' = do
                beginPass gate
                yield
                leaveRender gate >>= \case
                  Drain acts -> drain acts >> pass'
                  Again -> pass'
                  Done -> pure ()
          pass'
  done <- newEmptyMVar
  forM_ [1 .. threads] $ \_ -> forkIO $ do
    replicateM_ each (runOrQueue gate bump >> yield)
    putMVar done ()
  renderer <- newEmptyMVar
  void $ forkIO $ replicateM_ (threads * each) render >> putMVar renderer ()
  replicateM_ threads (takeMVar done)
  takeMVar renderer
  -- Whatever the last render left behind.
  render
  readIORef count >>= assertEqual "every action ran exactly once" (threads * each)
  readIORef gate >>= \g -> assertEqual "the lock is let go, and nothing waits" (False, False, 0) (g.held, g.draining, length g.queued)

spec :: Spec
spec =
  describe "driver state" $ do
    it "keeps an update made while another is being computed" test
    it "queues actions during a render and hands them out in order" gateSteps
    it "queues an action behind queued ones that have yet to start" gateOrder
    it "starts each queued action only once the one before it finished or waits" drainOrder
    it "runs every action exactly once while renders come and go" gateStress
