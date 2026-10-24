{-# LANGUAGE TemplateHaskell #-}

-- | The suite's view of the socket double, test/web/socket-double.js: the
-- WebSockets and event sources it fakes, by the name in their address,
-- driven as their server would drive them, and what the page did with them.
module Test.Sockets
  ( -- * Sockets
    sockets
  , asked
  , sent
  , closedBy
  , socketListeners
  , accept
  , serverSends
  , serverCloses
  , serverFails

    -- * Event sources
  , sources
  , withCredentials
  , closedByPage
  , sourceListeners
  , openSource
  , serverEvent
  , failSource

    -- * Listening
  , listen
  , waitUntil
  , refusedWith

    -- * Every run
  , reset
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen.JSBits (JSText, JSVal, Safety (..), browserJS, fromJSBytes, fromJSText, toJSBytes, toJSText)
import Halogen.JSBits.Promise (JSError (..))
import Halogen.Net.WebSocket (Message (..))
import Halogen.Subscription qualified as HS
import Test.Hspec (Expectation, expectationFailure, shouldBe)

$( browserJS
     ["test/jsbits/sockets.js"]
     [ ("js_socket_count", "halogen_net_test_socket_count", Unsafe, [t|JSText -> IO Int|])
     , ("js_socket_asked", "halogen_net_test_socket_asked", Unsafe, [t|JSText -> IO JSText|])
     , ("js_socket_sent", "halogen_net_test_socket_sent", Unsafe, [t|JSText -> IO Int|])
     , ("js_socket_sent_is_text", "halogen_net_test_socket_sent_is_text", Unsafe, [t|JSText -> Int -> IO Bool|])
     , ("js_socket_sent_text", "halogen_net_test_socket_sent_text", Unsafe, [t|JSText -> Int -> IO JSText|])
     , ("js_socket_sent_bytes", "halogen_net_test_socket_sent_bytes", Unsafe, [t|JSText -> Int -> IO JSVal|])
     , ("js_socket_closed_code", "halogen_net_test_socket_closed_code", Unsafe, [t|JSText -> IO Int|])
     , ("js_socket_closed_reason", "halogen_net_test_socket_closed_reason", Unsafe, [t|JSText -> IO JSText|])
     , ("js_socket_listeners", "halogen_net_test_socket_listeners", Unsafe, [t|JSText -> IO Int|])
     , ("js_socket_accept", "halogen_net_test_socket_accept", Unsafe, [t|JSText -> JSText -> IO ()|])
     , ("js_socket_send_text", "halogen_net_test_socket_send_text", Unsafe, [t|JSText -> JSText -> IO ()|])
     , ("js_socket_send_bytes", "halogen_net_test_socket_send_bytes", Unsafe, [t|JSText -> JSVal -> IO ()|])
     , ("js_socket_close", "halogen_net_test_socket_close", Unsafe, [t|JSText -> Int -> JSText -> IO ()|])
     , ("js_socket_fail", "halogen_net_test_socket_fail", Unsafe, [t|JSText -> IO ()|])
     , ("js_source_count", "halogen_net_test_source_count", Unsafe, [t|JSText -> IO Int|])
     , ("js_source_with_credentials", "halogen_net_test_source_with_credentials", Unsafe, [t|JSText -> IO Bool|])
     , ("js_source_closed", "halogen_net_test_source_closed", Unsafe, [t|JSText -> IO Bool|])
     , ("js_source_listeners", "halogen_net_test_source_listeners", Unsafe, [t|JSText -> IO Int|])
     , ("js_source_open", "halogen_net_test_source_open", Unsafe, [t|JSText -> IO ()|])
     , ("js_source_event", "halogen_net_test_source_event", Unsafe, [t|JSText -> JSText -> JSText -> JSText -> IO ()|])
     , ("js_source_fail", "halogen_net_test_source_fail", Unsafe, [t|JSText -> Bool -> IO ()|])
     , ("js_reset", "halogen_net_test_sockets_reset", Unsafe, [t|IO ()|])
     ]
 )

-- | How many sockets the page has made under this name. The rest are about
-- the last of them.
sockets :: Text -> IO Int
sockets = js_socket_count . toJSText

-- | The subprotocols it asked for, joined with commas.
asked :: Text -> IO Text
asked name = fromJSText <$> js_socket_asked (toJSText name)

-- | What the page has sent on it, in order.
sent :: Text -> IO [Message]
sent name = do
  count <- js_socket_sent (toJSText name)
  let one i = do
        isText <- js_socket_sent_is_text (toJSText name) i
        if isText
          then TextMessage . fromJSText <$> js_socket_sent_text (toJSText name) i
          else BinaryMessage <$> (fromJSBytes =<< js_socket_sent_bytes (toJSText name) i)
  traverse one [0 .. count - 1]

-- | How the page closed it: 'Nothing' if it has not, and a code of 'Nothing'
-- if it closed with none.
closedBy :: Text -> IO (Maybe (Maybe Int, Text))
closedBy name = do
  code <- js_socket_closed_code (toJSText name)
  reason <- fromJSText <$> js_socket_closed_reason (toJSText name)
  pure $ case code of
    -1 -> Nothing
    0 -> Just (Nothing, reason)
    _ -> Just (Just code, reason)

-- | How many listeners are on it.
socketListeners :: Text -> IO Int
socketListeners = js_socket_listeners . toJSText

-- | The server accepts the connection, choosing this subprotocol (or none,
-- if empty).
accept :: Text -> Text -> IO ()
accept name chosen = js_socket_accept (toJSText name) (toJSText chosen)

-- | The server sends a message.
serverSends :: Text -> Message -> IO ()
serverSends name = \case
  TextMessage line -> js_socket_send_text (toJSText name) (toJSText line)
  BinaryMessage bytes -> js_socket_send_bytes (toJSText name) =<< toJSBytes bytes

-- | The server closes the connection, and the handshake finishes.
serverCloses :: Text -> Int -> Text -> IO ()
serverCloses name code reason = js_socket_close (toJSText name) code (toJSText reason)

-- | The connection fails: an error, and a close with no handshake.
serverFails :: Text -> IO ()
serverFails = js_socket_fail . toJSText

-- | How many event sources the page has made under this name. The rest are
-- about the last of them.
sources :: Text -> IO Int
sources = js_source_count . toJSText

-- | Whether it was made to send credentials.
withCredentials :: Text -> IO Bool
withCredentials = js_source_with_credentials . toJSText

-- | Whether the page has closed it.
closedByPage :: Text -> IO Bool
closedByPage = js_source_closed . toJSText

-- | How many listeners are on it.
sourceListeners :: Text -> IO Int
sourceListeners = js_source_listeners . toJSText

-- | The stream's response arrives (or arrives again, after a reconnection).
openSource :: Text -> IO ()
openSource = js_source_open . toJSText

-- | The server sends an event: its name, its data and its ID.
serverEvent :: Text -> Text -> Text -> Text -> IO ()
serverEvent name event data_ eventId = js_source_event (toJSText name) (toJSText event) (toJSText data_) (toJSText eventId)

-- | The connection drops: the browser will reconnect, or, if the flag says
-- so, has given up.
failSource :: Text -> Bool -> IO ()
failSource name givenUp = js_source_fail (toJSText name) givenUp

-- | Subscribe to an emitter: what it has given so far, in order, and how to
-- unsubscribe.
listen :: HS.Emitter IO a -> IO (IO [a], IO ())
listen emitter = do
  heard <- newIORef []
  subscription <- HS.subscribe emitter (\a -> atomicModifyIORef' heard (\as -> (as <> [a], ())))
  pure (readIORef heard, HS.unsubscribe subscription)

-- | Wait until the check holds, for up to five seconds, letting the page run
-- meanwhile: the double, like the browser, dispatches every event in a task
-- of its own.
waitUntil :: String -> IO Bool -> Expectation
waitUntil what check = go (500 :: Int)
  where
    go 0 = expectationFailure ("timed out waiting until " <> what)
    go n = check >>= \ok -> unless ok (threadDelay 10_000 >> go (n - 1))

-- | What the browser refused, by its name.
refusedWith :: Text -> Either JSError a -> Expectation
refusedWith name = \case
  Left e -> e.name `shouldBe` name
  Right _ -> expectationFailure ("expected " <> Text.unpack name <> ", got a value")

-- | Close every connection the double still has open, and forget all of
-- them: a run under browser GHCi finds the page as the run before left it.
-- Natively it does nothing.
reset :: IO ()
reset = js_reset
