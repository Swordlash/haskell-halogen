-- | Driver-level regression test for a render that meets a child's
-- @Receive@ still in progress.
--
-- The first child's first @Receive@ waits (in 'liftIO') until the test lets
-- it go, and meanwhile another state change renders the parent again. With
-- the old driver the second render re-entered the first and re-minted a live
-- child. Now the @Receive@ is a program of the child that suspends, the
-- render goes on without it, and the second render comes after the first.
--
-- Correct behaviour: existing children are reused, a newly added child is
-- initialized exactly once, and no live child is finalized.
module Test.DriverReentrancy (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (modify)
import Data.Foldable (traverse_)
import Data.IORef
import Data.Kind (Type)
import Data.Row (Empty, Row, type (.==))
import Data.Void (Void)
import Halogen as H
import Halogen.Component (ComponentSlot (..))
import Halogen.HTML qualified as HH
import Halogen.HTML.Core qualified as HC
import Halogen.IO.Driver qualified as AD
import Halogen.IO.Driver.State (RenderStateX (..))
import Halogen.Query.Input (Input)
import Halogen.Subscription qualified as HS
import Halogen.VDom.Types (VDom (..), runGraft)
import Prelude
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual)

----------------------------------------------------------------------
-- Headless RenderSpec: no DOM, just walks the VDom and renders slots.
----------------------------------------------------------------------

-- | Phantom render state; we observe behaviour through component lifecycle
-- counters rather than through the render state itself.
data TestRenderState (s :: Type) (act :: Type) (ps :: Row Type) (o :: Type)
  = TestRenderState

testRenderSpec :: AD.RenderSpec IO TestRenderState
testRenderSpec =
  AD.RenderSpec
    { AD.render = renderTest
    , AD.renderChild = id
    , AD.removeChild = \_ -> pure ()
    , AD.dispose = \_ -> pure ()
    }

renderTest
  :: forall s act ps o
   . (Input act -> IO ())
  -> (ComponentSlotBox ps IO act -> IO (RenderStateX TestRenderState))
  -> HC.HTML (ComponentSlot ps IO act) act
  -> Maybe (TestRenderState s act ps o)
  -> IO (TestRenderState s act ps o)
renderTest _handler renderChild html _prev = do
  traverse_ renderSlot (collect (HC.unHTML html))
  pure TestRenderState
  where
    renderSlot :: ComponentSlot ps IO act -> IO ()
    renderSlot = \case
      ComponentSlot box -> void (renderChild box)
      ThunkSlot _ -> error "Test.Native: thunk slots are unsupported"

    -- VDom widgets in document order, so slots render left-to-right.
    collect :: VDom p (ComponentSlot ps IO act) -> [ComponentSlot ps IO act]
    collect = \case
      Text _ -> []
      Elem _ _ _ cs -> concatMap collect cs
      Keyed _ _ _ cs -> concatMap (collect . snd) cs
      Widget w -> [w]
      Grafted g -> collect (runGraft g)

----------------------------------------------------------------------
-- Test components.
----------------------------------------------------------------------

data ChildEnv = ChildEnv
  { initCount :: IORef Int
  , finalizeCount :: IORef Int
  , receiveHook :: Int -> IO ()
  }

data ChildAction = CInit | CFinalize | CReceive Int

-- | A child whose only job is to record how often it is initialized/finalized
-- and to run a test hook on each @Receive@. The @cid@ distinguishes slot
-- instances so the hook can park only the first child's first @Receive@.
mkChild :: ChildEnv -> Int -> H.Component H.VoidF Int Void IO
mkChild env cid =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ()
      , render = renderChildHtml
      , eval =
          H.mkEval
            H.defaultEval
              { initialize = Just CInit
              , finalize = Just CFinalize
              , receive = Just . CReceive
              , handleAction = handleChild
              }
      }
  where
    renderChildHtml :: () -> H.ComponentHTML ChildAction Empty IO
    renderChildHtml _ = HH.text "child"

    handleChild = \case
      CInit -> H.liftEffect $ modifyIORef' env.initCount (+ 1)
      CFinalize -> H.liftEffect $ modifyIORef' env.finalizeCount (+ 1)
      CReceive _ -> liftIO $ env.receiveHook cid

data ParentAction = PInit | Bump | Reenter

type PSlots = "child" .== H.Slot H.VoidF Void Int

parentComponent
  :: ChildEnv
  -> HS.Emitter IO ParentAction
  -> H.Component H.VoidF () Void IO
parentComponent env emitter =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure (0 :: Int)
      , render = renderParent
      , eval =
          H.mkEval
            H.defaultEval
              { initialize = Just PInit
              , handleAction = handleParent
              }
      }
  where
    renderParent :: Int -> H.ComponentHTML ParentAction PSlots IO
    renderParent tick =
      HH.div_ $
        (if tick > 0 then [HH.slot_ "child" (2 :: Int) (mkChild env 2) tick] else [])
          <> [ HH.slot_ "child" (0 :: Int) (mkChild env 0) tick
             , HH.slot_ "child" (1 :: Int) (mkChild env 1) tick
             ]

    handleParent = \case
      PInit -> void $ H.subscribe emitter
      Bump -> modify (+ 1)
      Reenter -> modify (+ 100)

----------------------------------------------------------------------
-- Spec.
----------------------------------------------------------------------

test :: IO ()
test = do
  initCount <- newIORef (0 :: Int)
  finalizeCount <- newIORef (0 :: Int)
  armed <- newIORef True
  inReceive <- newEmptyMVar
  gate <- newEmptyMVar
  ctrl <- HS.create :: IO (HS.Subscribe IO ParentAction)

  -- The first child's first Receive parks the in-flight render here.
  let hook cid =
        when (cid == 0) $ do
          go <- atomicModifyIORef' armed (False,)
          when go $ do
            putMVar inReceive ()
            takeMVar gate
      env = ChildEnv initCount finalizeCount hook

  socket <- AD.runUI testRenderSpec (parentComponent env ctrl.emitter) ()

  -- Initial render mounts both children exactly once.
  assertEqual "initial child count" 2 =<< readIORef initCount

  -- While child 0's Receive waits, the parent renders again (as a
  -- subscription or a fork would make it), then the Receive is let go.
  reentered <- newEmptyMVar
  _ <- forkIO $ do
    takeMVar inReceive
    HS.notify ctrl.listener Reenter
    putMVar gate ()
    putMVar reentered ()

  -- The render walks the reuse path and mounts child 2; it does not wait
  -- for child 0's Receive.
  HS.notify ctrl.listener Bump
  assertEqual "child count after the render" 3 =<< readIORef initCount
  takeMVar reentered

  assertEqual "child count after the second render" 3 =<< readIORef initCount
  assertEqual "finalized live child count" 0 =<< readIORef finalizeCount

  socket.dispose

spec :: Spec
spec =
  describe "driver re-entrancy" $
    it
      "preserves child lifecycle during a re-entrant render"
      test
