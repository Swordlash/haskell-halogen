-- | Calling the app's Rust commands: Tauri v2's @invoke@, through the global
-- API (@window.__TAURI__.core.invoke@).
--
-- > data SaveNote = SaveNote {noteId :: Int, text :: Text}
-- >   deriving stock (Generic)
-- >   deriving anyclass (ToJSON)
-- >
-- > -- #[tauri::command] fn save_note(note_id: i64, text: String) -> Result<Note, String>
-- > save :: (MonadIO m) => Int -> Text -> m (Either TauriError Note)
-- > save noteId text = invoke "save_note" SaveNote {noteId, text}
--
-- = Arguments and results
--
-- A command's arguments are one JSON object, keyed by the names of the Rust
-- function's parameters, and its result is whatever serde made of what the
-- function returned. Both go through aeson here, so the arguments are
-- usually a plain record with a 'GHC.Generics.Generic' 'ToJSON' instance,
-- and the result any 'FromJSON' type.
--
-- Tauri renames the parameters, by default: a Rust parameter @note_id@ is
-- the key @noteId@ of the arguments, in camelCase, as JavaScript would
-- spell it. A Haskell record's fields are camelCase already, so the
-- default aeson options give the keys Tauri expects, with no options at
-- all. A command declared with @#[tauri::command(rename_all =
-- "snake_case")]@ wants the Rust names instead, which
-- @'Data.Aeson.defaultOptions' {'Data.Aeson.fieldLabelModifier' =
-- 'Data.Aeson.camelTo2' \'_\'}@ gives. The renaming is of the parameters
-- only: a struct passed as one of them is read by its own @Deserialize@,
-- with its fields as serde names them (as in Rust unless the struct says
-- @#[serde(rename_all = "camelCase")]@), and so is a struct the command
-- returns.
--
-- The arguments must encode as a JSON object, since Tauri reads anything
-- else as something else (an array of numbers as raw bytes); anything else
-- is refused, with a @TypeError@, before it is sent. A command without
-- parameters takes 'noArgs'. A command that returns nothing (Rust's @()@)
-- gives @null@, which aeson's @()@ does not read (it reads @[]@): call it
-- with 'invoke_', or take its result as a 'Data.Aeson.Value'. A command
-- that answers with raw bytes (Rust's @tauri::ipc::Response@) is not
-- supported.
--
-- = Waiting
--
-- 'invoke' waits for the command's result, so it belongs in a component's
-- @lift@ (or @liftIO@), which waits on a worker thread while the rest of the
-- tree goes on, and never in @H.liftEffect@, which must not wait. A command
-- that must be sent at once, inside the dispatch of the event that asked
-- for it, is sent with 'start' in @H.liftEffect@, which gives an
-- 'Invocation' to wait for with 'result' later.
--
-- Killing the thread that waits (as a component's worker is killed when
-- the component goes away) stops the wait and nothing else: Tauri has no
-- way to cancel a command, so the Rust function runs to its end, and what
-- it returns is dropped.
--
-- = Outside Tauri
--
-- In a plain browser, and natively, 'isTauri' is 'False', nothing is sent,
-- and every command gives 'NotInTauri' at once, so one program can run in
-- both and ask 'isTauri' where it should do something else.
module Halogen.Tauri.Invoke
  ( -- * Where the program runs
    isTauri

    -- * Commands
  , invoke
  , invoke_
  , noArgs
  , encodeArgs

    -- * Sending now, waiting later
  , Invocation
  , start
  , result

    -- * Failures
  , TauriError (..)
  , JSError (..)
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON, Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Halogen.JSBits.Promise (Promise)
import Halogen.Tauri.Internal

-- | A command under way, from 'start': the promise of its result, or
-- nothing outside Tauri, where nothing was sent.
newtype Invocation = Invocation (Maybe Promise)

-- | Call the command and wait for its result, decoded. Call it from
-- @lift@; see /Waiting/ above for what killing the wait does.
invoke :: (MonadIO m, ToJSON args, FromJSON a) => Text -> args -> m (Either TauriError a)
invoke command args = start command args >>= result

-- | Call a command whose result does not matter, such as one that returns
-- nothing, and wait until it has succeeded or failed. Whatever it returns
-- is dropped, of whatever type it is.
invoke_ :: forall m args. (MonadIO m, ToJSON args) => Text -> args -> m (Either TauriError ())
invoke_ command args = fmap (const ()) <$> (invoke command args :: m (Either TauriError Value))

-- | The arguments of a command without parameters: an empty object.
noArgs :: Value
noArgs = Object KeyMap.empty

-- | The arguments as the JSON text 'invoke' sends, to check what a record's
-- 'ToJSON' instance says against what the Rust command expects.
encodeArgs :: (ToJSON args) => args -> Text
encodeArgs = encodeJSON

-- | Send the command, and wait for nothing: by the time this returns, it is
-- on its way to Tauri. It is synchronous, so it can run in @H.liftEffect@; wait
-- for its result with 'result'.
start :: (MonadIO m, ToJSON args) => Text -> args -> m Invocation
start command args = liftIO $ do
  tauri <- isTauri
  if not tauri
    then pure (Invocation Nothing)
    else Invocation . Just <$> invokeText command (encodeArgs args)

-- | Wait for the result of a command 'start' sent, as 'invoke' does. Any
-- number of waits for the same 'Invocation' each get its result, each
-- decoding it as the type it asks for.
result :: (MonadIO m, FromJSON a) => Invocation -> m (Either TauriError a)
result (Invocation Nothing) = pure (Left NotInTauri)
result (Invocation (Just promise)) = liftIO (settled promise)
