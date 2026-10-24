-- | Tauri v2's events, through the global API (@window.__TAURI__.event@):
-- listening, as an emitter a component subscribes to, and emitting.
--
-- > data Action = Initialize | Progress Download
-- >
-- > handleAction = \case
-- >   Initialize -> void (H.subscribe (Progress <$> listen "download-progress"))
-- >   Progress d -> modify $ \st -> st {download = Just d}
--
-- = Listening
--
-- 'listen' is an emitter like any other of "Halogen.Subscription", in 'IO'
-- as 'Halogen.Query.HalogenM.subscribe' takes it: each subscription to it
-- is a listener of its own (@event.listen@), and its unsubscribe is the
-- listener's unlisten function, so a component's subscription is taken off
-- with the component. Making the emitter does nothing; subscribing does.
--
-- @event.listen@ gives its unlisten function in a promise, after Tauri has
-- registered the listener. An unsubscribe that comes before the promise
-- has resolved is not lost: the listener delivers nothing from then on, and
-- is unlistened as soon as the promise resolves. Which events reach a
-- listener in the moment between subscribing and Tauri's registration is
-- Tauri's business, as it is for its own API.
--
-- A listener hears the event wherever it was emitted to: by the Rust side's
-- @emit@ or @emit_to@ (to any target), or by any window's 'emit'. The
-- current window's own events, which Tauri emits to the window, are in
-- "Halogen.Tauri.Window", which listens on the window.
--
-- = Payloads
--
-- An event's payload is whatever serde made of the Rust side's (or JSON of
-- the emitting webview's), decoded here with aeson. A payload that does
-- not decode as the type asked for is not delivered by 'listen', which
-- reports it to the browser's console instead: such a payload is a
-- disagreement between the two sides of the app, which the program can do
-- nothing about at that point, and an emitter of the wanted type keeps the
-- handler simple. 'listenEither' delivers it, as aeson's message, for a
-- program that would rather handle it.
--
-- = Outside Tauri
--
-- In a plain browser, and natively, a subscription to 'listen' is nothing,
-- and nothing is ever emitted to it; 'emit' and 'emitTo' do nothing.
module Halogen.Tauri.Event
  ( listen
  , listenEither
  , emit
  , emitTo
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Halogen.Subscription qualified as HS
import Halogen.Tauri.Internal

-- | The payloads of the event of this name, decoded; one that does not
-- decode is reported to the console and not delivered (see /Payloads/).
listen :: (FromJSON a) => Text -> HS.Emitter IO a
listen name = HS.Emitter $ \handler -> HS.subscribe (listenEither name) (either (reportPayload name) handler)

-- | The payloads of the event of this name, each decoded, or aeson's
-- message for one that does not decode.
listenEither :: (FromJSON a) => Text -> HS.Emitter IO (Either Text a)
listenEither = listenTo Anywhere

-- | Emit the event, with the payload, to every listener: those of every
-- window, this one's included, and the Rust side's. It waits for nothing,
-- so it can run anywhere, @H.liftEffect@ included; an emit Tauri refuses
-- (an event name with characters it does not allow, say, or one the
-- window's capabilities do not allow) is reported to the console.
emit :: (MonadIO m, ToJSON a) => Text -> a -> m ()
emit name payload = liftIO (emitText Nothing name (encodeJSON payload))

-- | Emit the event to the listeners of one target only, named by its label
-- (a window's, a webview's), as 'emit' does otherwise. Listeners that hear
-- the event anywhere ('listen' among them) hear it too, as Tauri has it.
emitTo :: (MonadIO m, ToJSON a) => Text -> Text -> a -> m ()
emitTo target name payload = liftIO (emitText (Just target) name (encodeJSON payload))
