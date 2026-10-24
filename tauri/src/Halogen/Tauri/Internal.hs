{-# LANGUAGE TemplateHaskell #-}

-- | What the public modules share: the bindings to jsbits/tauri.js (one
-- splice, so that WebAssembly embeds the file once), the error type, and
-- the JSON on this side of the IPC.
module Halogen.Tauri.Internal
  ( TauriError (..)
  , JSError (..)
  , isTauri
  , encodeJSON
  , decodeJSON
  , invokeText
  , settled
  , Scope (..)
  , listenTo
  , reportPayload
  , emitText
  , destroyCurrent
  )
where

import Control.Exception (Exception (..))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.ByteString.Lazy qualified as Lazy
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Halogen.JSBits (Callback, JSText, JSVal, Safety (..), browserJS, freeCallback, jsValText, mkCallback, toJSText)
import Halogen.JSBits.Promise (JSError (..), Promise (..), await)
import Halogen.Subscription qualified as HS

-- | Why a command gave no result.
data TauriError
  = -- | There is no Tauri here: the program runs in a plain browser, or
    -- natively, or in a Tauri app without @app.withGlobalTauri@ (see
    -- 'isTauri'). Nothing was sent.
    NotInTauri
  | -- | The command was rejected: by the Rust command itself, which returned
    -- an @Err@, or by Tauri, which refuses a command that does not exist or
    -- that the window's capabilities do not allow. An error the Rust side
    -- serialised as a string is the 'message' of a 'JSError' named
    -- @Error@; one serialised as anything else (an error enum, a struct) has
    -- that value's JSON as its message, to decode with aeson.
    Rejected JSError
  | -- | The command succeeded, but its result does not decode as the type
    -- asked for: aeson's message, with the path to what it could not read.
    BadResult Text
  deriving stock (Eq, Show)

instance Exception TauriError where
  displayException = \case
    NotInTauri -> "not in a Tauri webview with the global API"
    Rejected e -> "rejected: " <> displayException e
    BadResult message -> "a result that does not decode: " <> Text.unpack message

-- | Where a listener listens: to the event wherever it was emitted, or only
-- to what is emitted to the current window, which is where Tauri sends the
-- window's own events.
data Scope = Anywhere | CurrentWindow
  deriving stock (Eq)

$( browserJS
     ["jsbits/tauri.js"]
     [ ("js_is_tauri", "halogen_tauri_is_tauri", Unsafe, [t|IO Bool|])
     , ("js_invoke", "halogen_tauri_invoke", Unsafe, [t|JSText -> JSText -> IO Promise|])
     , ("js_listen", "halogen_tauri_listen", Unsafe, [t|JSText -> Bool -> Callback -> IO JSVal|])
     , ("js_unlisten", "halogen_tauri_unlisten", Unsafe, [t|JSText -> JSVal -> IO ()|])
     , ("js_emit", "halogen_tauri_emit", Unsafe, [t|JSText -> JSText -> IO ()|])
     , ("js_emit_to", "halogen_tauri_emit_to", Unsafe, [t|JSText -> JSText -> JSText -> IO ()|])
     , ("js_report_payload", "halogen_tauri_report_payload", Unsafe, [t|JSText -> JSText -> IO ()|])
     , ("js_destroy", "halogen_tauri_destroy", Unsafe, [t|IO Promise|])
     ]
 )

-- | Whether the program runs in a Tauri webview whose global API
-- (@window.__TAURI__@) is there, which is what every binding here goes
-- through. It is there when the app sets @app.withGlobalTauri@ in its
-- @tauri.conf.json@; a Tauri app without it counts as no Tauri, since these
-- bindings cannot reach Tauri there. 'False' in a plain browser and
-- natively.
--
-- It is asked afresh each time, and costs a property lookup or two, so a
-- program can ask it wherever it needs to rather than keep the answer.
isTauri :: (MonadIO m) => m Bool
isTauri = liftIO js_is_tauri

-- | A value as JSON text. aeson writes UTF-8, so the decoding cannot fail.
encodeJSON :: (ToJSON a) => a -> Text
encodeJSON = decodeUtf8 . Lazy.toStrict . encode

decodeJSON :: (FromJSON a) => Text -> Either Text a
decodeJSON json = either (Left . Text.pack) Right (eitherDecodeStrict' (encodeUtf8 json))

-- | Send a command with its arguments as JSON text, and give the promise of
-- its result as JSON text.
invokeText :: Text -> Text -> IO Promise
invokeText command args = js_invoke (toJSText command) (toJSText args)

-- | Wait for a promise of JSON text, as jsbits/tauri.js's promises are, and
-- decode what it resolved with.
settled :: (FromJSON a) => Promise -> IO (Either TauriError a)
settled promise =
  await promise >>= \case
    Left e -> pure (Left (Rejected e))
    Right json -> pure (either (Left . BadResult) Right (decodeJSON (jsValText json)))

-- | An emitter of the event's payloads, each decoded, or aeson's message
-- for one that does not decode. Each subscription is a listener of its own,
-- taken off by its 'HS.unsubscribe'; outside Tauri a subscription is
-- nothing, and nothing is ever emitted to it.
listenTo :: (FromJSON a) => Scope -> Text -> HS.Emitter IO (Either Text a)
listenTo scope name = HS.Emitter $ \handler -> do
  tauri <- isTauri
  if not tauri
    then pure (HS.Subscription (pure ()))
    else do
      callback <- mkCallback (handler . decodeJSON . jsValText)
      subscription <- js_listen (toJSText name) (scope == CurrentWindow) callback
      -- The callback is freed once, by the first unsubscribe: after it the
      -- JavaScript calls it no more.
      stopped <- newIORef False
      pure $ HS.Subscription $ do
        already <- atomicModifyIORef' stopped (True,)
        unless already $ do
          js_unlisten (toJSText name) subscription
          freeCallback callback

-- | Emit an event with a payload of JSON text, to every listener or to one
-- target's; outside Tauri, nothing.
emitText :: Maybe Text -> Text -> Text -> IO ()
emitText target name payload = do
  tauri <- isTauri
  if not tauri
    then pure ()
    else case target of
      Nothing -> js_emit (toJSText name) (toJSText payload)
      Just label -> js_emit_to (toJSText label) (toJSText name) (toJSText payload)

-- | Report an event whose payload did not decode to the console, which is
-- all there is to tell.
reportPayload :: Text -> Text -> IO ()
reportPayload name message = js_report_payload (toJSText name) (toJSText message)

-- | Destroy the current window, waiting until it is done. Outside Tauri
-- there is no window to destroy.
destroyCurrent :: IO (Either TauriError ())
destroyCurrent = do
  tauri <- isTauri
  if not tauri
    then pure (Left NotInTauri)
    else either (Left . Rejected) (const (Right ())) <$> (await =<< js_destroy)
