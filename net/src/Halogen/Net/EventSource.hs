{-# LANGUAGE TemplateHaskell #-}

-- | The browser's @EventSource@: server-sent events, a response the server
-- keeps open and writes events into, one after another, as text. The
-- browser reconnects by itself when the connection drops, telling the server
-- the last event it had, until the page closes the source or the server
-- answers with something that is no event stream.
--
-- The names here are short and shared with "Halogen.Net.WebSocket", so
-- import it qualified:
--
-- > import Halogen.Net.EventSource qualified as SSE
-- >
-- > data Action = Initialize | Heard SSE.ServerEvent | Finalize
-- >
-- > handleAction = \case
-- >   Initialize ->
-- >     H.liftEffect (SSE.open "/api/updates" False ["price"]) >>= \case
-- >       Left refused -> modify $ \st -> st {problem = Just refused}
-- >       Right source -> do
-- >         modify $ \st -> st {source = Just source}
-- >         void $ H.subscribe (Heard <$> SSE.events source)
-- >   Heard event -> modify $ \st -> st {prices = event.data_ : st.prices}
-- >   Finalize -> gets (.source) >>= traverse_ (H.liftEffect . SSE.close)
--
-- As with a socket, nothing here waits: 'open' starts the request and
-- returns, and belongs in @H.liftEffect@, and the events arrive through the
-- emitters, 'opened', 'events' and 'errors', for @H.subscribe@. Each emitter
-- keeps what arrives before its first subscriber and hands it to that
-- subscriber, so a component that subscribes in a later action, or is handed
-- a source a program opened, hears it all from the first event; once it has
-- had a subscriber, what arrives while nobody is subscribed goes to nobody.
-- The emitters are given the events in the order the browser dispatches
-- them, while it dispatches them.
--
-- A component's subscriptions end when it goes away, but its source does
-- not, and goes on reconnecting for as long as the page lives: a component
-- that opens one closes it in its @Finalize@, as above. A source's callbacks
-- are freed once it has stopped for good, when it is closed or the browser
-- gives up on it.
--
-- Natively there is no engine to make the request: 'open' gives a source
-- whose emitters never fire and whose 'readyState' is 'Closed', and 'close'
-- does nothing.
module Halogen.Net.EventSource
  ( -- * Listening
    Source
  , open

    -- * What arrives
  , ServerEvent (..)
  , events
  , opened
  , errors

    -- * Stopping
  , close

    -- * Where it is
  , ReadyState (..)
  , readyState

    -- * Failures
  , JSError (..)
  )
where

import Control.Exception (finally)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_)
import Data.Text (Text)
import Halogen.JSBits (Callback, JSText, JSVal, Safety (..), browserJS, fromJSText, inBrowser, toJSText)
import Halogen.JSBits.Promise (JSError (..))
import Halogen.Net.Internal (Backlog, Callbacks, backlogEmitter, callback, freeAll, give, newBacklog, newCallbacks, refusal)
import Halogen.Subscription qualified as HS

$( browserJS
     ["jsbits/eventsource.js"]
     [ ("js_names", "halogen_net_eventsource_names", Unsafe, [t|IO JSVal|])
     , ("js_name_add", "halogen_net_eventsource_name_add", Unsafe, [t|JSVal -> JSText -> IO ()|])
     , ("js_open", "halogen_net_eventsource_open", Unsafe, [t|JSText -> Bool -> JSVal -> Callback -> Callback -> Callback -> IO JSVal|])
     , ("js_close", "halogen_net_eventsource_close", Unsafe, [t|JSVal -> IO ()|])
     , ("js_event_name", "halogen_net_eventsource_event_name", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_event_data", "halogen_net_eventsource_event_data", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_event_id", "halogen_net_eventsource_event_id", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_ready_state", "halogen_net_eventsource_ready_state", Unsafe, [t|JSVal -> IO Int|])
     ]
 )

-- | A stream of server-sent events, from 'open': the browser's
-- @EventSource@, and the emitters its events go to.
data Source = Source
  { raw :: JSVal
  , callbacks :: Callbacks
  , openLog :: Backlog ()
  , eventLog :: Backlog ServerEvent
  , errorLog :: Backlog ReadyState
  }

-- | An event the server sent: its name (@message@ unless the server named
-- it), its data (the lines of its @data:@ fields, joined with line breaks),
-- and the last event ID the server has set, which is the one the browser
-- sends back when it reconnects (empty if the server has set none).
data ServerEvent = ServerEvent
  { name :: Text
  , data_ :: Text
  , lastEventId :: Text
  }
  deriving stock (Eq, Show)

-- | Where a source is: connecting (at first, or again after the connection
-- dropped), open for events, or closed for good.
data ReadyState
  = Connecting
  | Open
  | Closed
  deriving stock (Eq, Show)

-- | Start the request for the stream at the address (relative to the page,
-- or absolute), with the browser's credentials (cookies and the like) if the
-- flag says so and the address is another origin's (to the page's own they
-- always go), and return at once, without waiting for it.
--
-- 'events' carries every event the server did not name, and those it named
-- with one of the names given here; the browser drops events with other
-- names, since only a listener for a name hears it. The names @open@ and
-- @error@ are the source's own events, which 'opened' and 'errors' carry,
-- and are not listened for: an event the server names so is taken by the
-- browser for the source's own.
--
-- An address the browser cannot parse is a 'Left' @SyntaxError@. Anything
-- else that goes wrong, a server that does not answer or answers with an
-- error status or with something that is no event stream, is an event on
-- 'errors' afterwards.
open :: (MonadIO m) => Text -> Bool -> [Text] -> m (Either JSError Source)
open url withCredentials names = liftIO $ do
  openLog <- newBacklog
  eventLog <- newBacklog
  errorLog <- newBacklog
  callbacks <- newCallbacks
  onOpen <- callback callbacks (\_ -> give openLog ())
  onEvent <- callback callbacks $ \event -> do
    name <- fromJSText <$> js_event_name event
    data_ <- fromJSText <$> js_event_data event
    lastEventId <- fromJSText <$> js_event_id event
    give eventLog ServerEvent {name, data_, lastEventId}
  onError <- callback callbacks $ \source -> do
    state <- stateOf <$> js_ready_state source
    -- A source the browser has given up on has no more events, and its
    -- listeners have been taken off it.
    give errorLog state `finally` when (state == Closed) (freeAll callbacks)
  listened <- js_names
  for_ names $ \name -> when (name `notElem` ["open", "error"]) (js_name_add listened (toJSText name))
  raw <- js_open (toJSText url) withCredentials listened onOpen onEvent onError
  refusal raw >>= \case
    Just refused -> Left refused <$ freeAll callbacks
    Nothing -> pure (Right Source {raw, callbacks, openLog, eventLog, errorLog})

-- | The source's @open@: when the response has arrived and is an event
-- stream, and again each time the browser has reconnected.
opened :: Source -> HS.Emitter IO ()
opened source = backlogEmitter source.openLog

-- | Every event the server sends, of the names 'open' was given, in order.
events :: Source -> HS.Emitter IO ServerEvent
events source = backlogEmitter source.eventLog

-- | The source's @error@, with where it is afterwards: 'Connecting' when the
-- connection dropped (or could not be made) and the browser will try again,
-- and 'Closed' when it has given up for good, on a response with an error
-- status or one that is no event stream, and the source has no more events.
-- The browser gives no detail of what went wrong, so neither does this.
errors :: Source -> HS.Emitter IO ReadyState
errors source = backlogEmitter source.errorLog

-- | Close the source for good: the browser drops the connection, or stops
-- trying to make one, and no more events arrive, not even on 'errors'.
-- Closing a closed source does nothing.
close :: (MonadIO m) => Source -> m ()
close source = liftIO $ js_close source.raw >> freeAll source.callbacks

-- | Where the source is now. Natively, always 'Closed'.
readyState :: (MonadIO m) => Source -> m ReadyState
readyState source
  | not inBrowser = pure Closed
  | otherwise = liftIO $ stateOf <$> js_ready_state source.raw

stateOf :: Int -> ReadyState
stateOf = \case
  0 -> Connecting
  1 -> Open
  _ -> Closed
