-- | A socket and an event source natively, where there is no engine to
-- connect them: they are made, and never connect. The connections
-- themselves are the browser suite's (Browser.hs).
module Test.Inert (spec) where

import Control.Monad (void)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Halogen.JSBits (inBrowser)
import Halogen.Net.EventSource qualified as SSE
import Halogen.Net.WebSocket qualified as WS
import Halogen.Subscription qualified as HS
import Test.Hspec

spec :: Spec
spec =
  -- In a browser, or under Node on the browser backends, there is an engine
  -- that would try to connect.
  (if inBrowser then xdescribe else describe) "natively" $ do
    describe "a socket" $ do
      it "is made, closed, and never fires" $ do
        socket <- either (fail . show) pure =<< WS.open "wss://example.com/chat" ["v1"]
        heard <- newIORef (0 :: Int)
        counted heard (WS.opened socket)
        counted heard (WS.messages socket)
        counted heard (WS.errors socket)
        counted heard (WS.closed socket)
        WS.readyState socket `shouldReturn` WS.Closed
        WS.protocol socket `shouldReturn` ""
        readIORef heard `shouldReturn` 0

      it "sends and closes without a word" $ do
        socket <- either (fail . show) pure =<< WS.open "ws://example.com/" []
        WS.send socket (WS.TextMessage "nobody hears this") `shouldReturn` Right ()
        WS.send socket (WS.BinaryMessage "nor this") `shouldReturn` Right ()
        WS.close socket 4000 "done" `shouldReturn` Right ()
        WS.close' socket
        WS.readyState socket `shouldReturn` WS.Closed

    describe "an event source" $
      it "is made, closed, and never fires" $ do
        source <- either (fail . show) pure =<< SSE.open "/updates" True ["price"]
        heard <- newIORef (0 :: Int)
        counted heard (SSE.opened source)
        counted heard (SSE.events source)
        counted heard (SSE.errors source)
        SSE.readyState source `shouldReturn` SSE.Closed
        SSE.close source
        SSE.close source
        readIORef heard `shouldReturn` 0

-- | Count what the emitter gives.
counted :: IORef Int -> HS.Emitter IO a -> IO ()
counted heard emitter = void (HS.subscribe emitter (\_ -> atomicModifyIORef' heard (\n -> (n + 1, ()))))
