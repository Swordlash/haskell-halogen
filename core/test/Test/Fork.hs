-- | What a component's forks are registered as, and for how long.
--
-- 'Halogen.Query.HalogenM.fork' hands back a 'ForkId', and 'kill' and 'join'
-- look that id up in the component's register of running forks. So does
-- finalization, which is how a component's forks are stopped when it goes
-- away. A fork that is not in the register while it runs makes all three
-- silently do nothing, which is what these tests are here to notice.
module Test.Fork (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get, put)
import Data.Foldable (traverse_)
import Data.IORef
import Data.Kind (Type)
import Data.Row (Empty, Row)
import Data.Void (Void)
import Halogen as H
import Halogen.HTML qualified as HH
import Halogen.IO.Driver qualified as AD
import Prelude
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual)

-- | No DOM: these tests never look at what was rendered.
data TestRenderState (s :: Type) (act :: Type) (ps :: Row Type) (o :: Type)
  = TestRenderState

renderSpec :: AD.RenderSpec IO TestRenderState
renderSpec =
  AD.RenderSpec
    { AD.render = \_handler _renderChild _html _prev -> pure TestRenderState
    , AD.renderChild = id
    , AD.removeChild = \_ -> pure ()
    , AD.dispose = \_ -> pure ()
    }

data Query a
  = Start a
  | KillIt a
  | JoinIt a

-- | Forks a program that waits and then writes; keeps the id so that a later
-- query can kill or join it.
component :: IORef [String] -> H.Component Query () Void IO
component ran =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure Nothing
      , render = \_ -> HH.text "fork"
      , eval = H.mkEval H.defaultEval {handleQuery}
      }
  where
    handleQuery :: forall a. Query a -> H.HalogenM (Maybe ForkId) () Empty Void IO (Maybe a)
    handleQuery = \case
      Start a -> do
        forkId <- H.fork $ liftIO $ do
          threadDelay 50_000
          modifyIORef' ran (<> ["ran"])
        put (Just forkId)
        pure (Just a)
      KillIt a -> do
        get >>= traverse_ H.kill
        pure (Just a)
      JoinIt a -> do
        get >>= traverse_ H.join
        pure (Just a)

spec :: Spec
spec = describe "component forks" $ do
  it "kills a fork that is still running" $ do
    ran <- newIORef []
    H.HalogenSocket {query, dispose} <- AD.runUI renderSpec (component ran) ()
    _ <- query (H.mkTell Start)
    _ <- query (H.mkTell KillIt)
    threadDelay 250_000
    assertEqual "killed fork did not run" [] =<< readIORef ran
    dispose

  it "joins a fork that is still running" $ do
    ran <- newIORef []
    H.HalogenSocket {query, dispose} <- AD.runUI renderSpec (component ran) ()
    _ <- query (H.mkTell Start)
    _ <- query (H.mkTell JoinIt)
    -- No waiting here: joining is the wait.
    assertEqual "joined fork had finished" ["ran"] =<< readIORef ran
    dispose

  it "kills the forks of a component that is finalized" $ do
    ran <- newIORef []
    H.HalogenSocket {query, dispose} <- AD.runUI renderSpec (component ran) ()
    _ <- query (H.mkTell Start)
    dispose
    threadDelay 250_000
    assertEqual "fork outlived its component" [] =<< readIORef ran
