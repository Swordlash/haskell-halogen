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

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.State.Class (get, modify, state)
import Data.Kind (Type)
import Data.Row (Empty, Row)
import Data.Void (Void)
import Halogen as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Core qualified as HC
import Halogen.IO.Driver qualified as AD
import Halogen.IO.Driver.State (RenderStateX (..))
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

spec :: Spec
spec =
  describe "driver state" $ do
    it "keeps an update made while another is being computed" test
