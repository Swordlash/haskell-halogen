-- | Sockets opened and listened to directly: against the socket double
-- (test/web/socket-double.js) for what a server does, and against the
-- browser's own WebSocket for what it refuses and how a connection nothing
-- answers fails.
module Test.WebSocket (spec) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen.Net.WebSocket qualified as WS
import Test.Hspec
import Test.Sockets

spec :: Spec
spec = do
  describe "open" $ do
    it "asks for the subprotocols given, and hears the one the server chose" $ do
      socket <- opened' "ws://double/chat" ["v2", "v1"]
      asked "chat" `shouldReturn` "v2,v1"
      WS.readyState socket `shouldReturn` WS.Connecting
      WS.protocol socket `shouldReturn` ""
      (heard, _) <- listen (WS.opened socket)
      accept "chat" "v1"
      waitUntil "the socket opens" ((== [()]) <$> heard)
      WS.readyState socket `shouldReturn` WS.Open
      WS.protocol socket `shouldReturn` "v1"

    it "is refused, with a SyntaxError, for an address that is no WebSocket address" $ do
      refusedWith "SyntaxError" =<< WS.open "ftp://example.com/" []
      refusedWith "SyntaxError" =<< WS.open "ws://example.com/#a-fragment" []
      refusedWith "SyntaxError" =<< WS.open "http://[::1" []

    it "is refused, with a SyntaxError, for a subprotocol asked for twice" $
      refusedWith "SyntaxError" =<< WS.open "ws://127.0.0.1:2/" ["chat", "chat"]

    it "is not refused for a port the browser does not connect to, which fails instead" $ do
      socket <- opened' "ws://127.0.0.1:25/" []
      (heard, _) <- listen ((Left <$> WS.errors socket) <|> (Right <$> WS.closed socket))
      waitUntil "the socket closes" ((== 2) . length <$> heard)
      heard `shouldReturn` [Left (), Right WS.Close {code = 1006, reason = "", clean = False}]

    it "takes a relative address as one on the page's host" $ do
      socket <- opened' "no-socket-here" []
      (ended, _) <- listen (WS.closed socket)
      -- The runner's file server is no WebSocket server, so the handshake
      -- fails: but it was tried, rather than refused.
      waitUntil "the socket closes" (not . null <$> ended)
      ended `shouldReturn` [WS.Close {code = 1006, reason = "", clean = False}]

    it "takes an http address as a ws one" $ do
      socket <- opened' "http://127.0.0.1:2/" []
      (ended, _) <- listen (WS.closed socket)
      waitUntil "the socket closes" (not . null <$> ended)
      ended `shouldReturn` [WS.Close {code = 1006, reason = "", clean = False}]

  describe "what arrives" $ do
    it "is the open, then every message in order, text and bytes as the server sent them" $ do
      socket <- opened' "ws://double/order" []
      (heard, _) <- listen ((Nothing <$ WS.opened socket) <|> (Just <$> WS.messages socket))
      accept "order" ""
      serverSends "order" (WS.TextMessage "one")
      serverSends "order" (WS.BinaryMessage someBytes)
      serverSends "order" (WS.TextMessage "zażółć gęślą jaźń")
      serverSends "order" (WS.BinaryMessage ByteString.empty)
      waitUntil "every message arrives" ((== 5) . length <$> heard)
      heard
        `shouldReturn` [ Nothing
                       , Just (WS.TextMessage "one")
                       , Just (WS.BinaryMessage someBytes)
                       , Just (WS.TextMessage "zażółć gęślą jaźń")
                       , Just (WS.BinaryMessage ByteString.empty)
                       ]

    it "before anyone subscribed is kept, and handed to the first subscriber" $ do
      socket <- opened' "ws://double/early" []
      accept "early" ""
      serverSends "early" (WS.TextMessage "first")
      serverSends "early" (WS.TextMessage "second")
      waitUntil "the socket opens" ((== WS.Open) <$> WS.readyState socket)
      -- The messages are dispatched in tasks after the open's.
      threadDelay 100_000
      (opens, _) <- listen (WS.opened socket)
      (heard, _) <- listen (WS.messages socket)
      opens `shouldReturn` [()]
      heard `shouldReturn` [WS.TextMessage "first", WS.TextMessage "second"]
      -- A second subscriber hears only what arrives after it.
      (later, _) <- listen (WS.messages socket)
      later `shouldReturn` []
      serverSends "early" (WS.TextMessage "third")
      waitUntil "the third message arrives" ((== [WS.TextMessage "third"]) <$> later)
      heard `shouldReturn` [WS.TextMessage "first", WS.TextMessage "second", WS.TextMessage "third"]

    it "no longer reaches a subscriber that unsubscribed" $ do
      socket <- opened' "ws://double/unsubscribed" []
      accept "unsubscribed" ""
      (gone, unsubscribe) <- listen (WS.messages socket)
      (staying, _) <- listen (WS.messages socket)
      serverSends "unsubscribed" (WS.TextMessage "before")
      waitUntil "the first message arrives" ((== 1) . length <$> gone)
      unsubscribe
      serverSends "unsubscribed" (WS.TextMessage "after")
      waitUntil "the second message arrives" ((== 2) . length <$> staying)
      gone `shouldReturn` [WS.TextMessage "before"]

  describe "send" $ do
    it "sends text and bytes as they are" $ do
      socket <- opened' "ws://double/echo/round-trip" []
      (heard, _) <- listen (WS.messages socket)
      waitUntil "the socket opens" ((== WS.Open) <$> WS.readyState socket)
      let said = [WS.TextMessage "zażółć gęślą jaźń", WS.BinaryMessage (ByteString.pack [0 .. 255]), WS.TextMessage ""]
      traverse (WS.send socket) said `shouldReturn` map (const (Right ())) said
      sent "echo/round-trip" `shouldReturn` said
      waitUntil "the echoes arrive" ((== 3) . length <$> heard)
      heard `shouldReturn` said

    it "is refused, with an InvalidStateError, while the socket is still connecting" $ do
      socket <- opened' "ws://127.0.0.1:2/" []
      refusedWith "InvalidStateError" =<< WS.send socket (WS.TextMessage "too soon")
      WS.close' socket

  describe "closing" $ do
    it "sends the server the page's code and reason, and ends with a clean close" $ do
      socket <- opened' "ws://double/echo/closed" []
      (ended, _) <- listen (WS.closed socket)
      waitUntil "the socket opens" ((== WS.Open) <$> WS.readyState socket)
      WS.close socket 4000 "done" `shouldReturn` Right ()
      WS.readyState socket `shouldReturn` WS.Closing
      closedBy "echo/closed" `shouldReturn` Just (Just 4000, "done")
      waitUntil "the socket closes" (not . null <$> ended)
      ended `shouldReturn` [WS.Close {code = 4000, reason = "done", clean = True}]
      WS.readyState socket `shouldReturn` WS.Closed

    it "with close' sends no code, which the close says is 1005" $ do
      socket <- opened' "ws://double/echo/plain-close" []
      (ended, _) <- listen (WS.closed socket)
      waitUntil "the socket opens" ((== WS.Open) <$> WS.readyState socket)
      WS.close' socket
      closedBy "echo/plain-close" `shouldReturn` Just (Nothing, "")
      waitUntil "the socket closes" (not . null <$> ended)
      ended `shouldReturn` [WS.Close {code = 1005, reason = "", clean = True}]

    it "is refused for a code a page may not use, and for too long a reason" $ do
      socket <- opened' "ws://127.0.0.1:2/" []
      refusedWith "InvalidAccessError" =<< WS.close socket 1001 ""
      refusedWith "InvalidAccessError" =<< WS.close socket 5000 ""
      refusedWith "SyntaxError" =<< WS.close socket 1000 (Text.replicate 124 "a")
      WS.readyState socket `shouldReturn` WS.Connecting
      WS.close' socket

    it "by the server arrives with the server's code and reason" $ do
      socket <- opened' "ws://double/server-closed" []
      (ended, _) <- listen (WS.closed socket)
      accept "server-closed" ""
      serverCloses "server-closed" 4001 "bye"
      waitUntil "the socket closes" (not . null <$> ended)
      ended `shouldReturn` [WS.Close {code = 4001, reason = "bye", clean = True}]
      closedBy "server-closed" `shouldReturn` Nothing

    it "takes the socket's listeners off it once it has closed" $ do
      socket <- opened' "ws://double/listeners" []
      socketListeners "listeners" `shouldReturn` 4
      (ended, _) <- listen (WS.closed socket)
      accept "listeners" ""
      serverCloses "listeners" 1000 ""
      waitUntil "the socket closes" (not . null <$> ended)
      socketListeners "listeners" `shouldReturn` 0

  describe "a connection that fails" $ do
    it "is an error, then a close with 1006 that is not clean" $ do
      socket <- opened' "ws://double/failed" []
      (heard, _) <- listen ((Left <$> WS.errors socket) <|> (Right <$> WS.closed socket))
      accept "failed" ""
      serverFails "failed"
      waitUntil "the socket closes" ((== 2) . length <$> heard)
      heard `shouldReturn` [Left (), Right WS.Close {code = 1006, reason = "", clean = False}]
      socketListeners "failed" `shouldReturn` 0

    it "fails the same way in the browser when nothing answers" $ do
      socket <- opened' "ws://127.0.0.1:2/" []
      (heard, _) <- listen ((Left <$> WS.errors socket) <|> (Right <$> WS.closed socket))
      waitUntil "the socket closes" ((== 2) . length <$> heard)
      heard `shouldReturn` [Left (), Right WS.Close {code = 1006, reason = "", clean = False}]
      WS.readyState socket `shouldReturn` WS.Closed

    it "is what closing a socket that is still connecting gives" $ do
      socket <- opened' "ws://double/abandoned" []
      (heard, _) <- listen ((Left <$> WS.errors socket) <|> (Right <$> WS.closed socket))
      WS.close' socket
      waitUntil "the socket closes" ((== 2) . length <$> heard)
      heard `shouldReturn` [Left (), Right WS.Close {code = 1006, reason = "", clean = False}]
  where
    someBytes = ByteString.pack [0, 255, 10, 13, 0xc3]

-- | The socket, or the test fails with what the browser refused.
opened' :: Text -> [Text] -> IO WS.Socket
opened' url protocols = WS.open url protocols >>= either (\e -> fail ("expected a socket, got " <> show e)) pure
