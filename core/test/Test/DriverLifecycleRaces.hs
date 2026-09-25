-- | Regression tests for the driver's bookkeeping under concurrency and
-- failure, with a render spec that walks real slots.
--
-- * A state update that is being computed while a render adds a child must
--   not write back the children from before: the next render would create
--   that child a second time (the "Duplicate slot address" warning) and leave
--   the first one running.
-- * A fork that registers after its component was finalized must be killed.
-- * A render that throws must let the render lock go, or no later render
--   would ever run.
module Test.DriverLifecycleRaces (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (ErrorCall (..), throwIO, try)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get, modify, put, state)
import Data.Foldable (traverse_)
import Data.IORef
import Data.Kind (Type)
import Data.Row (Empty, Row, type (.==))
import Data.Text (Text)
import Data.Void (Void)
import Halogen as H
import Halogen.Component (ComponentSlot (..))
import Halogen.HTML qualified as HH
import Halogen.HTML.Core qualified as HC
import Halogen.IO.Driver qualified as AD
import Halogen.IO.Driver.State (RenderStateX (..))
import Halogen.Query.Input (Input)
import Halogen.VDom.Types (VDom (..), runGraft)
import Prelude
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual)

----------------------------------------------------------------------
-- A render spec with no DOM that renders every slot, records the text of
-- each render, and throws while 'failing' is up.
----------------------------------------------------------------------

data Rendered (s :: Type) (act :: Type) (ps :: Row Type) (o :: Type) = Rendered

data Probe = Probe
  { texts :: IORef [Text]
  , failing :: IORef Bool
  }

newProbe :: IO Probe
newProbe = Probe <$> newIORef [] <*> newIORef False

walking :: Probe -> AD.RenderSpec IO Rendered
walking probe =
  AD.RenderSpec
    { AD.render = render
    , AD.renderChild = id
    , AD.removeChild = \_ -> pure ()
    , AD.dispose = \_ -> pure ()
    }
  where
    render
      :: (Input act -> IO ())
      -> (ComponentSlotBox ps IO act -> IO (RenderStateX Rendered))
      -> HC.HTML (ComponentSlot ps IO act) act
      -> Maybe (Rendered s act ps o)
      -> IO (Rendered s act ps o)
    render _ renderChild html _ = do
      fails <- readIORef probe.failing
      when fails $ throwIO (ErrorCall "render failed")
      let (slots, text) = walk (HC.unHTML html)
      modifyIORef' probe.texts (<> [mconcat text])
      traverse_ (\case ComponentSlot box -> void (renderChild box); ThunkSlot _ -> pure ()) slots
      pure Rendered

    walk :: VDom p w -> ([w], [Text])
    walk = \case
      Text t -> ([], [t])
      Elem _ _ _ cs -> foldMap walk cs
      Keyed _ _ _ cs -> foldMap (walk . snd) cs
      Widget w -> ([w], [])
      Grafted g -> walk (runGraft g)

----------------------------------------------------------------------
-- Children counted in and out.
----------------------------------------------------------------------

data Counts = Counts
  { started :: IORef Int
  , stopped :: IORef Int
  }

data ChildAction = Start | Stop

child :: Counts -> H.Component H.VoidF () Void IO
child counts =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ()
      , render = \_ -> HH.text "" :: H.ComponentHTML ChildAction Empty IO
      , eval =
          H.mkEval
            H.defaultEval
              { initialize = Just Start
              , finalize = Just Stop
              , handleAction = \case
                  Start -> liftIO $ modifyIORef' counts.started (+ 1)
                  Stop -> liftIO $ modifyIORef' counts.stopped (+ 1)
              }
      }

type Slots = "child" .== H.Slot H.VoidF Void Int

data Query a
  = Parked a
  | Plain a
  | Current (Int -> a)

data Gates = Gates
  { entered :: MVar ()
  , release :: MVar ()
  }

park :: Gates -> Int -> Int
park gates n = unsafePerformIO $ do
  putMVar gates.entered ()
  takeMVar gates.release
  pure (n + 1)
{-# NOINLINE park #-}

-- | One child per number up to the state.
parent :: Counts -> Gates -> H.Component Query () Void IO
parent counts gates =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure (0 :: Int)
      , render = \n -> HH.div_ [HH.slot_ "child" k (child counts) () | k <- [0 .. n]] :: H.ComponentHTML Void Slots IO
      , eval = H.mkEval H.defaultEval {handleQuery = answer}
      }
  where
    answer :: Query a -> H.HalogenM Int Void Slots Void IO (Maybe a)
    answer = \case
      Parked a -> state (\n -> let n' = park gates n in n' `seq` ((), n')) >> pure (Just a)
      Plain a -> modify (+ 1) >> pure (Just a)
      Current k -> Just . k <$> get

childrenKept :: IO ()
childrenKept = do
  probe <- newProbe
  counts <- Counts <$> newIORef 0 <*> newIORef 0
  gates <- Gates <$> newEmptyMVar <*> newEmptyMVar
  AD.HalogenSocket {AD.query = ask, AD.dispose = dispose} <- AD.runUI (walking probe) (parent counts gates) ()

  parkedDone <- newEmptyMVar
  plainDone <- newEmptyMVar
  _ <- forkIO $ ask (Parked ()) >> putMVar parkedDone ()
  takeMVar gates.entered
  -- Meanwhile another update renders, and a child is added.
  _ <- forkIO $ ask (Plain ()) >> putMVar plainDone ()
  threadDelay 50000
  putMVar gates.release ()
  takeMVar parkedDone
  takeMVar plainDone

  Just n <- ask (Current id)
  running <- (-) <$> readIORef counts.started <*> readIORef counts.stopped
  started <- readIORef counts.started
  assertEqual "each child started once" (n + 1) started
  assertEqual "and none stopped" (n + 1) running
  dispose

----------------------------------------------------------------------
-- A fork that registers after finalization.
----------------------------------------------------------------------

data ForkQuery a = HoldThenFork (MVar ()) (MVar ()) (IORef Bool) a

forker :: H.Component ForkQuery () Void IO
forker =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ()
      , render = \_ -> HH.text "" :: H.ComponentHTML Void Empty IO
      , eval = H.mkEval H.defaultEval {handleQuery = answer}
      }
  where
    answer :: ForkQuery a -> H.HalogenM () Void Empty Void IO (Maybe a)
    answer (HoldThenFork holding go survived a) = do
      liftIO $ putMVar holding () >> takeMVar go
      -- The component is finalized by now; this fork must not outlive it.
      -- (It may be killed before it even starts, so it cannot report its
      -- own death; what it can report is having lived on.)
      void $ H.fork $ liftIO $ threadDelay 200000 >> writeIORef survived True
      pure (Just a)

forkAfterFinalize :: IO ()
forkAfterFinalize = do
  probe <- newProbe
  AD.HalogenSocket {AD.query = ask, AD.dispose = dispose} <- AD.runUI (walking probe) forker ()
  holding <- newEmptyMVar
  go <- newEmptyMVar
  survived <- newIORef False
  answered <- newEmptyMVar
  _ <- forkIO $ ask (HoldThenFork holding go survived ()) >> putMVar answered ()
  takeMVar holding
  dispose
  putMVar go ()
  takeMVar answered
  threadDelay 600000
  readIORef survived >>= assertEqual "the late fork was killed" False

----------------------------------------------------------------------
-- A render that throws.
----------------------------------------------------------------------

data SetQuery a = Set Int a

counter :: H.Component SetQuery () Void IO
counter =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure (0 :: Int)
      , render = \n -> HH.text (if n == 0 then "zero" else if n == 1 then "one" else "two") :: H.ComponentHTML Void Empty IO
      , eval = H.mkEval H.defaultEval {handleQuery = \(Set n a) -> put n >> pure (Just a)}
      }

renderAfterFailure :: IO ()
renderAfterFailure = do
  probe <- newProbe
  AD.HalogenSocket {AD.query = ask, AD.dispose = dispose} <- AD.runUI (walking probe) counter ()
  writeIORef probe.failing True
  failed <- try (ask (Set 1 ()))
  assertEqual "the render failed" True (either (\(ErrorCall _) -> True) (const False) failed)
  writeIORef probe.failing False
  void $ ask (Set 2 ())
  rendered <- readIORef probe.texts
  assertEqual "the next state is rendered" (Just "two") (lastMay rendered)
  dispose
  where
    lastMay xs = if null xs then Nothing else Just (last xs)

spec :: Spec
spec =
  describe "driver lifecycle" $ do
    it "does not start a child twice when an update overlaps a render" childrenKept
    it "kills a fork registered after its component was finalized" forkAfterFinalize
    it "renders again after a render threw" renderAfterFailure
