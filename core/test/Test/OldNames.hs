{-# OPTIONS_GHC -Wno-deprecations #-}

-- | 'lift', 'liftIO' and 'withRunInIO' in 'H.HalogenM' are deprecated, and
-- they still work. Each suspends the program, as 'H.async' and 'H.asyncWith'
-- do. This module uses them on purpose, so it turns the warning off.
module Test.OldNames (spec) where

import Control.Concurrent.MVar
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.State.Class (get, modify)
import Control.Monad.Trans (lift)
import Data.IORef
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.IO.Driver qualified as AD
import Halogen.Subscription qualified as HS
import Prelude
import Test.Hspec (Spec, describe, it)
import Test.Sync (walking, within)
import Test.Utils (assertEqual)

-- | How the program waits for its gate.
data Old = Lift | LiftIO | WithRunInIO

data Act
  = Bump
  | -- | Waits for the gate the old way, then notes the count and signals.
    Wait Old (MVar ()) (MVar ())

waiting :: IORef [Text] -> HS.Emitter IO Act -> H.Component H.VoidF () Void IO
waiting logRef acts =
  H.component
    (\_ -> pure (0 :: Int))
    (\n -> HH.text (T.pack (show n)) :: H.ComponentHTML Act Empty IO)
    [H.onAction handleAction, H.onInitialize (void (H.subscribe acts))]
  where
    note t = H.sync (modifyIORef' logRef (<> [t]))
    handleAction = \case
      Bump -> modify (+ 1) >> note "bump"
      Wait old gate done -> do
        case old of
          Lift -> lift (takeMVar gate)
          LiftIO -> liftIO (takeMVar gate)
          WithRunInIO -> withRunInIO (\run -> takeMVar gate >> run (pure ()))
        n <- get
        note ("saw " <> T.pack (show n))
        H.sync (putMVar done ())

-- | An event that arrives while the program waits is handled first, and the
-- program then sees the new state.
suspends :: Old -> IO ()
suspends old = do
  logRef <- newIORef []
  HS.Subscribe {HS.listener, HS.emitter} <- HS.create
  socket <- AD.runUI walking (waiting logRef emitter) ()
  gate <- newEmptyMVar
  done <- newEmptyMVar
  HS.notify listener (Wait old gate done)
  HS.notify listener Bump
  readIORef logRef >>= assertEqual "the bump ran meanwhile" ["bump"]
  putMVar gate ()
  within "the program" (takeMVar done)
  readIORef logRef >>= assertEqual "the program saw the new state" ["bump", "saw 1"]
  socket.dispose

spec :: Spec
spec = describe "the deprecated instances of HalogenM" $ do
  it "suspends at lift, as at an async" (suspends Lift)
  it "suspends at liftIO, as at an async" (suspends LiftIO)
  it "suspends at withRunInIO, as at an asyncWith" (suspends WithRunInIO)
