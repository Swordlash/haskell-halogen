-- | Event sources opened and listened to directly: against the socket
-- double (test/web/socket-double.js) for what a server sends, and against
-- the browser's own EventSource, and the runner's file server, for what it
-- refuses and when it gives up.
module Test.EventSource (spec) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Halogen.Net.EventSource qualified as SSE
import Test.Hspec
import Test.Sockets

spec :: Spec
spec = do
  describe "open" $ do
    it "makes the request with credentials, or without, as asked" $ do
      _ <- opened' "http://double/with" True []
      _ <- opened' "http://double/without" False []
      withCredentials "with" `shouldReturn` True
      withCredentials "without" `shouldReturn` False

    it "listens for message and the names asked for, once each, and not for open or error" $ do
      _ <- opened' "http://double/names" False ["price", "price", "message", "open", "error"]
      -- open, message, price and error.
      sourceListeners "names" `shouldReturn` 4

    it "is refused, with a SyntaxError, for an address the browser cannot parse" $
      refusedWith "SyntaxError" =<< SSE.open "http://[::1" False []

  describe "what arrives" $ do
    it "is the open, then the events of the names asked for, in order" $ do
      source <- opened' "http://double/feed" False ["price"]
      (heard, _) <- listen ((Nothing <$ SSE.opened source) <|> (Just <$> SSE.events source))
      SSE.readyState source `shouldReturn` SSE.Connecting
      openSource "feed"
      serverEvent "feed" "message" "zażółć\ngęślą" "1"
      serverEvent "feed" "price" "10" "2"
      serverEvent "feed" "other" "dropped" "3"
      serverEvent "feed" "message" "" ""
      waitUntil "every event arrives" ((== 4) . length <$> heard)
      heard
        `shouldReturn` [ Nothing
                       , Just SSE.ServerEvent {name = "message", data_ = "zażółć\ngęślą", lastEventId = "1"}
                       , Just SSE.ServerEvent {name = "price", data_ = "10", lastEventId = "2"}
                       , Just SSE.ServerEvent {name = "message", data_ = "", lastEventId = ""}
                       ]
      SSE.readyState source `shouldReturn` SSE.Open

    it "before anyone subscribed is kept, and handed to the first subscriber" $ do
      source <- opened' "http://double/early" False []
      openSource "early"
      serverEvent "early" "message" "first" ""
      waitUntil "the source opens" ((== SSE.Open) <$> SSE.readyState source)
      threadDelay 100_000
      (opens, _) <- listen (SSE.opened source)
      (heard, _) <- listen (SSE.events source)
      opens `shouldReturn` [()]
      (map (.data_) <$> heard) `shouldReturn` ["first"]

    it "no longer reaches a subscriber that unsubscribed" $ do
      source <- opened' "http://double/unsubscribed" False []
      openSource "unsubscribed"
      (gone, unsubscribe) <- listen (SSE.events source)
      (staying, _) <- listen (SSE.events source)
      serverEvent "unsubscribed" "message" "before" ""
      waitUntil "the first event arrives" ((== 1) . length <$> gone)
      unsubscribe
      serverEvent "unsubscribed" "message" "after" ""
      waitUntil "the second event arrives" ((== 2) . length <$> staying)
      (map (.data_) <$> gone) `shouldReturn` ["before"]

  describe "an error" $ do
    it "says whether the browser will reconnect, or has given up" $ do
      source <- opened' "http://double/dropped" False []
      (opens, _) <- listen (SSE.opened source)
      (failures, _) <- listen (SSE.errors source)
      openSource "dropped"
      failSource "dropped" False
      waitUntil "the connection drops" ((== [SSE.Connecting]) <$> failures)
      SSE.readyState source `shouldReturn` SSE.Connecting
      sourceListeners "dropped" `shouldReturn` 3
      openSource "dropped"
      waitUntil "the source opens again" ((== 2) . length <$> opens)
      failSource "dropped" True
      waitUntil "the browser gives up" ((== [SSE.Connecting, SSE.Closed]) <$> failures)
      SSE.readyState source `shouldReturn` SSE.Closed
      -- The source has no more events, so its listeners are taken off it.
      sourceListeners "dropped" `shouldReturn` 0

    it "in the browser, for a stream that is not there, is the browser giving up" $ do
      source <- opened' "no-such-file.txt" False []
      (failures, _) <- listen (SSE.errors source)
      waitUntil "the browser gives up" ((== [SSE.Closed]) <$> failures)
      SSE.readyState source `shouldReturn` SSE.Closed

    it "in the browser, for a response that is no event stream, is the browser giving up" $ do
      source <- opened' "fixture.txt" False []
      (failures, _) <- listen (SSE.errors source)
      waitUntil "the browser gives up" ((== [SSE.Closed]) <$> failures)

  describe "close" $
    it "stops the source for good, and takes its listeners off it" $ do
      source <- opened' "http://double/closed" False []
      (heard, _) <- listen (SSE.events source)
      openSource "closed"
      waitUntil "the source opens" ((== SSE.Open) <$> SSE.readyState source)
      SSE.close source
      closedByPage "closed" `shouldReturn` True
      SSE.readyState source `shouldReturn` SSE.Closed
      sourceListeners "closed" `shouldReturn` 0
      -- Closing again does nothing.
      SSE.close source
      heard `shouldReturn` []

-- | The source, or the test fails with what the browser refused.
opened' :: Text -> Bool -> [Text] -> IO SSE.Source
opened' url credentials names = SSE.open url credentials names >>= either (\e -> fail ("expected a source, got " <> show e)) pure
