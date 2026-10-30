{-# LANGUAGE CPP #-}

-- | "Halogen.Query.Event.Window" without a browser: the timer ticks and
-- stops, the frame clock is silent, and, natively, the page's emitters put
-- their listeners on the in-memory window and take them off again.
--
-- What they hear in a browser is tested in one, in hspec-halogen's suite
-- (its @Window@ example).
module Test.WindowEvents (spec) where

import Control.Concurrent (threadDelay)
import Data.IORef
import Halogen.Query.Event.Window qualified as Window
import Halogen.Subscription qualified as HS
import Prelude
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)
#if defined(HALOGEN_WASI) || (!defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH))
import Data.Foldable (for_)
import Data.Foreign (toForeign)
import Data.Text (Text)
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM, window)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Web.Event.Event (Event (..))
import Web.HTML.HTMLDocument.VisibilityState (VisibilityState (..))
#endif

spec :: Spec
spec = describe "window emitters" $ do
  describe "interval" $ do
    it "ticks until it is unsubscribed" $ do
      ticks <- newIORef (0 :: Int)
      sub <- HS.subscribe (Window.interval 10) $ \() -> atomicModifyIORef' ticks (\n -> (n + 1, ()))
      threadDelay 200_000
      HS.unsubscribe sub
      ticked <- readIORef ticks
      -- 20 are due; a loaded machine delivers fewer, but not none.
      assertWith ("ticked " <> show ticked <> " times in 200ms of 10ms ticks") (ticked >= 3 && ticked <= 21)
      threadDelay 100_000
      assertEqual "ticks after unsubscribing" ticked =<< readIORef ticks

    it "gives each subscription a clock of its own" $ do
      first <- newIORef (0 :: Int)
      second <- newIORef (0 :: Int)
      let count ref () = atomicModifyIORef' ref (\n -> (n + 1, ()))
      sub1 <- HS.subscribe (Window.interval 10) (count first)
      sub2 <- HS.subscribe (Window.interval 10) (count second)
      threadDelay 100_000
      HS.unsubscribe sub1
      stopped <- readIORef first
      threadDelay 100_000
      HS.unsubscribe sub2
      assertEqual "the first after it was unsubscribed" stopped =<< readIORef first
      kept <- readIORef second
      assertWith ("the second went on, to " <> show kept <> " ticks") (kept > stopped)

    -- The driver may run a tick's action on the ticking thread, and the
    -- action may unsubscribe: the rest of it must still run.
    it "finishes a tick whose handler unsubscribes it" $ do
      subRef <- newIORef Nothing
      after <- newIORef (0 :: Int)
      sub <- HS.subscribe (Window.interval 10) $ \() -> do
        readIORef subRef >>= mapM_ HS.unsubscribe
        atomicModifyIORef' after (\n -> (n + 1, ()))
      writeIORef subRef (Just sub)
      threadDelay 150_000
      assertEqual "ticks finished after unsubscribing" 1 =<< readIORef after

  describe "animationFrame" $
    it "never fires without a browser, and unsubscribes" $ do
      frames <- newIORef (0 :: Int)
      sub <- HS.subscribe Window.animationFrame $ \_ -> atomicModifyIORef' frames (\n -> (n + 1, ()))
      threadDelay 50_000
      HS.unsubscribe sub
      assertEqual "frames" 0 =<< readIORef frames

#if defined(HALOGEN_WASI) || (!defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH))
  describe "on the in-memory DOM" $ do
    it "listens for keys on the window while subscribed" $ do
      before <- listenersFor "keydown"
      keys <- runMemDOM Window.keyDown
      heard <- newIORef (0 :: Int)
      sub <- HS.subscribe keys $ \_ -> atomicModifyIORef' heard (\n -> (n + 1, ()))
      assertEqual "listeners while subscribed" (before + 1) =<< listenersFor "keydown"
      dispatch "keydown"
      assertEqual "keys heard" 1 =<< readIORef heard
      HS.unsubscribe sub
      assertEqual "listeners once unsubscribed" before =<< listenersFor "keydown"
      dispatch "keydown"
      assertEqual "keys heard once unsubscribed" 1 =<< readIORef heard

    it "measures a window of no size" $ do
      sizes <- collect Window.resize "resize"
      assertEqual "sizes" [(0, 0)] sizes

    it "finds the page visible" $ do
      states <- collect Window.visibility "visibilitychange"
      assertEqual "states" [Visible] states

    it "hears the page go offline and online, on two listeners it takes off" $ do
      before <- (+) <$> listenersFor "online" <*> listenersFor "offline"
      changes <- runMemDOM Window.online
      heard <- newIORef []
      sub <- HS.subscribe changes $ \b -> modifyIORef' heard (<> [b])
      dispatch "offline"
      dispatch "online"
      HS.unsubscribe sub
      assertEqual "changes" [False, True] =<< readIORef heard
      assertEqual "listeners once unsubscribed" before =<< ((+) <$> listenersFor "online" <*> listenersFor "offline")

-- | What an emitter delivers for one event of the type, after which it is
-- unsubscribed; and its listener is gone then.
collect :: MemDOM (HS.Emitter IO a) -> Text -> IO [a]
collect make ty = do
  before <- listenersFor ty
  emitter <- runMemDOM make
  heard <- newIORef []
  sub <- HS.subscribe emitter $ \a -> modifyIORef' heard (<> [a])
  dispatch ty
  HS.unsubscribe sub
  assertEqual ("listeners for " <> show ty <> " once unsubscribed") before =<< listenersFor ty
  readIORef heard

-- | The in-memory window, which is also its document.
ambient :: IO N.NativeNode
ambient = N.toNative <$> runMemDOM window

listenersFor :: Text -> IO Int
listenersFor ty = length . filter (== ty) <$> (N.listenerTypes =<< ambient)

-- | What a browser does when the page's event happens: call the window's
-- listeners for it.
dispatch :: Text -> IO ()
dispatch ty = do
  listeners <- readIORef . (.listeners) =<< ambient
  for_ [l | l <- listeners, l.eventType == ty] $ \l -> runMemDOM (l.fire (Event (toForeign ())))
#endif
