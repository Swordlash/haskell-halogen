-- | A clock that ticks while it is mounted, from a timer it subscribes to
-- when it is initialised. The timer counts its ticks in the given counter,
-- so a test can see whether it is still running once the clock is gone.
module Example.Clock (component) where

import Data.IORef (IORef, atomicModifyIORef')
import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.Subscription qualified as HS
import Protolude

data Action = Initialize | Tick

component :: forall q o m. (Monad m) => H.Component q (IORef Int) o m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \counter -> pure (counter, 0 :: Int)
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.initialize = Just Initialize}
      }
  where
    render :: (IORef Int, Int) -> H.ComponentHTML Action Empty m
    render (_, ticks) = HH.p [HP.class_ (HH.ClassName "ticks")] [HH.text (show ticks)]

    handleAction :: Action -> H.HalogenM (IORef Int, Int) Action Empty o m ()
    handleAction = \case
      Initialize -> do
        counter <- gets fst
        void $ H.subscribe (timer counter)
      Tick -> modify (second (+ 1))

    -- Every 20ms until unsubscribed, which the component's finaliser does.
    timer counter = HS.makeEmitter $ \emit -> do
      thread <- forkIO $ forever $ do
        threadDelay 20000
        atomicModifyIORef' counter (\n -> (n + 1, ()))
        emit Tick
      pure (killThread thread)
