{-# LANGUAGE TemplateHaskell #-}

-- | The browser's WebSocket: a connection to a server that either side
-- sends messages on, text or binary, until one of them closes it.
--
-- The names here are short and shared with "Halogen.Net.EventSource", so
-- import it qualified:
--
-- > import Halogen.Net.WebSocket qualified as WS
-- >
-- > data Action = Initialize | Received WS.Message | Ended WS.Close | Say Text | Finalize
-- >
-- > handleAction = \case
-- >   Initialize ->
-- >     H.liftEffect (WS.open "wss://example.com/chat" []) >>= \case
-- >       Left refused -> modify $ \st -> st {problem = Just refused}
-- >       Right socket -> do
-- >         modify $ \st -> st {socket = Just socket}
-- >         void $ H.subscribe (Received <$> WS.messages socket)
-- >         void $ H.subscribe (Ended <$> WS.closed socket)
-- >   Received message -> modify $ \st -> st {heard = st.heard <> [message]}
-- >   Ended _ -> modify $ \st -> st {socket = Nothing}
-- >   Say line -> gets (.socket) >>= traverse_ (\s -> H.liftEffect (WS.send s (WS.TextMessage line)))
-- >   Finalize -> gets (.socket) >>= traverse_ (H.liftEffect . WS.close')
--
-- = Nothing here waits
--
-- 'open' starts connecting and returns at once, with the 'Socket'; 'send'
-- hands the browser a message and returns; 'close' starts the closing
-- handshake and returns. All of it is synchronous, so it belongs in
-- @H.liftEffect@, and none of it in @lift@, which would only move it to a
-- worker for nothing. What a socket does afterwards arrives through its
-- emitters, 'opened', 'messages', 'errors' and 'closed', which are for
-- @H.subscribe@: the waiting is the emitter's, and a component waits for a
-- message the way it waits for a click.
--
-- The emitters keep what arrives before anyone has subscribed to them, and
-- hand it to their first subscriber (each emitter its own): a component that
-- opens a socket in one action and subscribes in another, or is handed a
-- socket a program opened, hears the socket's @open@ and every message from
-- the first. Once an emitter has had a subscriber it keeps nothing more, and
-- what arrives while nobody is subscribed to it goes to nobody, so a
-- component that unsubscribes and subscribes again misses what came in
-- between. A socket whose messages nobody ever subscribes to keeps every one
-- of them for as long as the socket is kept.
--
-- The browser calls the socket's listeners in order, and the emitters are
-- given its events in that order, while the browser dispatches them: the
-- @open@ before the first message, an @error@ before the @close@ that
-- follows it, and the @close@ last of all.
--
-- = A socket's life
--
-- Every socket that 'open' gives ends with one event on 'closed', whether it
-- closed as it should (either side asked, and the handshake finished) or
-- failed: a connection that could not be made, or was cut, is an event on
-- 'errors' and then a 'Close' with code 1006 that is not 'clean'. The
-- browser says nothing else about a failure, to the page or to this
-- package, so 'errors' carries no detail.
--
-- A component's subscriptions end when it goes away, but its socket does
-- not: a socket is the program's, as a 'Halogen.Net.Fetch.Fetch' from
-- 'Halogen.Net.Fetch.start' is, and only 'close' closes it. A component that
-- opens one closes it in its @Finalize@, as above, unless it means to hand
-- it on.
--
-- The callbacks a socket hands the browser are freed once its 'closed'
-- event has been delivered, the last event the browser has for it, so a
-- closed socket holds on to nothing. One that is dropped without being
-- closed stays open, as it would in JavaScript, and keeps them.
--
-- = Natively
--
-- There is no engine to connect natively, and this is no WebSocket client of
-- its own: 'open' gives a socket that never connects, whose emitters never
-- fire and whose 'readyState' is 'Closed', and 'send' and 'close' do
-- nothing. That is enough to render a component that opens one, or to test
-- one natively up to the point where a server would answer.
module Halogen.Net.WebSocket
  ( -- * Connecting
    Socket
  , open

    -- * What arrives
  , Message (..)
  , messages
  , opened
  , errors
  , closed
  , Close (..)

    -- * Sending and closing
  , send
  , close
  , close'

    -- * Where it is
  , ReadyState (..)
  , readyState
  , protocol

    -- * Failures
  , JSError (..)
  )
where

import Control.Exception (finally)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Text (Text)
import Halogen.JSBits (Callback, JSText, JSVal, Safety (..), browserJS, fromJSBytes, fromJSText, inBrowser, jsValText, toJSBytes, toJSText)
import Halogen.JSBits.Promise (JSError (..))
import Halogen.Net.Internal (Backlog, backlogEmitter, callback, freeAll, give, newBacklog, newCallbacks, refusal)
import Halogen.Subscription qualified as HS

$( browserJS
     ["jsbits/websocket.js"]
     [ ("js_protocols", "halogen_net_websocket_protocols", Unsafe, [t|IO JSVal|])
     , ("js_protocol_push", "halogen_net_websocket_protocol_push", Unsafe, [t|JSVal -> JSText -> IO ()|])
     , ("js_open", "halogen_net_websocket_open", Unsafe, [t|JSText -> JSVal -> Callback -> Callback -> Callback -> Callback -> Callback -> IO JSVal|])
     , ("js_close_code", "halogen_net_websocket_close_code", Unsafe, [t|JSVal -> IO Int|])
     , ("js_close_reason", "halogen_net_websocket_close_reason", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_close_clean", "halogen_net_websocket_close_clean", Unsafe, [t|JSVal -> IO Bool|])
     , ("js_send_text", "halogen_net_websocket_send", Unsafe, [t|JSVal -> JSText -> IO JSVal|])
     , ("js_send_bytes", "halogen_net_websocket_send", Unsafe, [t|JSVal -> JSVal -> IO JSVal|])
     , ("js_close", "halogen_net_websocket_close", Unsafe, [t|JSVal -> Int -> JSText -> IO JSVal|])
     , ("js_ready_state", "halogen_net_websocket_ready_state", Unsafe, [t|JSVal -> IO Int|])
     , ("js_protocol", "halogen_net_websocket_protocol", Unsafe, [t|JSVal -> IO JSText|])
     ]
 )

-- | A connection, from 'open': the browser's @WebSocket@, and the emitters
-- its events go to.
data Socket = Socket
  { raw :: JSVal
  , openLog :: Backlog ()
  , messageLog :: Backlog Message
  , errorLog :: Backlog ()
  , closeLog :: Backlog Close
  }

-- | A message, either way: text, sent and received as UTF-8, or bytes, sent
-- and received as they are.
data Message
  = TextMessage Text
  | BinaryMessage ByteString
  deriving stock (Eq, Show)

-- | How a socket closed: the code and the reason of the closing handshake
-- (1005 and no reason when the side that closed gave none, and 1006 when
-- there was no handshake, the connection failing or being cut), and whether
-- it was 'clean', that is whether the handshake finished.
data Close = Close
  { code :: Int
  , reason :: Text
  , clean :: Bool
  }
  deriving stock (Eq, Show)

-- | Where a socket is: still connecting, open for messages, closing (one
-- side has asked, and the handshake has not finished), or closed.
data ReadyState
  = Connecting
  | Open
  | Closing
  | Closed
  deriving stock (Eq, Show)

-- | Start connecting to the address (@ws:@ or @wss:@, or @http:@ or
-- @https:@, which the browser takes to mean the same; relative to the page
-- if it is no absolute address), asking for the subprotocols given, in the
-- order of preference, if any; and return at once, without waiting for the
-- connection. It can run in @H.liftEffect@, and should: there is nothing to
-- wait for. Subscribe to 'opened' to hear that the socket is open.
--
-- What the browser refuses before it tries to connect is a 'Left', with
-- the browser's name for it: an address that is no WebSocket address, has a
-- fragment, or is no address at all, or a subprotocol that is no HTTP token
-- or is asked for twice, is a @SyntaxError@. A server that does not
-- answer, or refuses, is a socket that fails afterwards, on 'errors' and
-- 'closed', and so, in Chromium, is a port the browser will not connect to
-- (the port of another protocol, such as 25).
open :: (MonadIO m) => Text -> [Text] -> m (Either JSError Socket)
open url protocols = liftIO $ do
  openLog <- newBacklog
  messageLog <- newBacklog
  errorLog <- newBacklog
  closeLog <- newBacklog
  callbacks <- newCallbacks
  onOpen <- callback callbacks (\_ -> give openLog ())
  onText <- callback callbacks (give messageLog . TextMessage . jsValText)
  onBytes <- callback callbacks (\array -> give messageLog . BinaryMessage =<< fromJSBytes array)
  onError <- callback callbacks (\_ -> give errorLog ())
  onClose <- callback callbacks $ \event -> do
    code <- js_close_code event
    reason <- fromJSText <$> js_close_reason event
    clean <- js_close_clean event
    -- The browser has taken the listeners off the socket by now, and this
    -- is its last event.
    give closeLog Close {code, reason, clean} `finally` freeAll callbacks
  asked <- js_protocols
  for_ protocols (js_protocol_push asked . toJSText)
  raw <- js_open (toJSText url) asked onOpen onText onBytes onError onClose
  refusal raw >>= \case
    Just refused -> Left refused <$ freeAll callbacks
    Nothing -> pure (Right Socket {raw, openLog, messageLog, errorLog, closeLog})

-- | The socket's @open@: once, when the connection is made and the socket
-- can send. A socket that fails to connect never opens; it errs and closes.
opened :: Socket -> HS.Emitter IO ()
opened socket = backlogEmitter socket.openLog

-- | Every message the server sends, in order, from the first: see
-- /Nothing here waits/ above for what arrives before anyone subscribes.
messages :: Socket -> HS.Emitter IO Message
messages socket = backlogEmitter socket.messageLog

-- | The socket's @error@: the connection could not be made, or failed, and
-- the socket is closing. A 'Close' with code 1006 follows on 'closed'. The
-- browser gives no detail of what went wrong, so neither does this.
errors :: Socket -> HS.Emitter IO ()
errors socket = backlogEmitter socket.errorLog

-- | The socket's @close@: once, last of all its events, however it closed.
closed :: Socket -> HS.Emitter IO Close
closed socket = backlogEmitter socket.closeLog

-- | Hand the browser a message to send, without waiting for it to go: the
-- browser queues it behind those it has not sent yet.
--
-- A socket still connecting cannot send, and the browser refuses the
-- message with an @InvalidStateError@: send once 'opened' has fired. On a
-- socket that is closing or closed the browser takes the message and drops
-- it, without a word, so a 'Right' says that the browser has the message,
-- not that it has gone.
send :: (MonadIO m) => Socket -> Message -> m (Either JSError ())
send socket message = liftIO $ do
  outcome <- case message of
    TextMessage line -> js_send_text socket.raw (toJSText line)
    BinaryMessage bytes -> js_send_bytes socket.raw =<< toJSBytes bytes
  maybe (Right ()) Left <$> refusal outcome

-- | Start closing the socket with a code and a reason, which the server is
-- sent; 'closed' fires once the handshake ends. A page may close with 1000
-- (closed as it should be) or a code of its own, from 3000 to 4999, and a
-- reason of at most 123 bytes in UTF-8; the browser refuses another code
-- with an @InvalidAccessError@ and a longer reason with a @SyntaxError@.
-- Closing a socket that is still connecting gives up on the connection,
-- which the browser reports as a failure: an 'errors' event, and a 'Close'
-- with code 1006. Closing a socket that is closing or closed does nothing.
close :: (MonadIO m) => Socket -> Int -> Text -> m (Either JSError ())
close socket code reason = liftIO $ maybe (Right ()) Left <$> (refusal =<< js_close socket.raw code (toJSText reason))

-- | Start closing the socket with no code and no reason, which the browser
-- never refuses; the server, and 'closed', then see code 1005.
close' :: (MonadIO m) => Socket -> m ()
close' socket = liftIO $ void $ js_close socket.raw (-1) (toJSText "")

-- | Where the socket is now. Natively, always 'Closed'.
readyState :: (MonadIO m) => Socket -> m ReadyState
readyState socket
  | not inBrowser = pure Closed
  | otherwise =
      liftIO $
        js_ready_state socket.raw >>= \case
          0 -> pure Connecting
          1 -> pure Open
          2 -> pure Closing
          _ -> pure Closed

-- | The subprotocol the server chose from those 'open' asked for: empty
-- until the socket is open, and if the server chose none.
protocol :: (MonadIO m) => Socket -> m Text
protocol socket = liftIO $ fromJSText <$> js_protocol socket.raw
