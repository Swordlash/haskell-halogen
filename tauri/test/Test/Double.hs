{-# LANGUAGE TemplateHaskell #-}

-- | The suite's view of the double of Tauri's global API,
-- test/web/tauri-double.js: what the Rust side would do (emit an event,
-- answer a held command), what the double saw, and being outside Tauri.
module Test.Double
  ( fire
  , fireTo
  , listening
  , unlistens
  , Emitted (..)
  , emitted
  , destroyed
  , release
  , outsideTauri
  , reset
  )
where

import Control.Exception (bracket_)
import Data.Aeson (FromJSON, ToJSON, Value, eitherDecodeStrict', encode)
import Data.ByteString.Lazy qualified as Lazy
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Halogen.JSBits (JSText, Safety (..), browserJS, fromJSText, toJSText)

$( browserJS
     ["test/jsbits/double.js"]
     [ ("js_fire", "halogen_tauri_test_fire", Unsafe, [t|JSText -> JSText -> JSText -> IO ()|])
     , ("js_listening", "halogen_tauri_test_listening", Unsafe, [t|JSText -> IO Int|])
     , ("js_unlistens", "halogen_tauri_test_unlistens", Unsafe, [t|IO Int|])
     , ("js_emitted", "halogen_tauri_test_emitted", Unsafe, [t|IO JSText|])
     , ("js_destroyed", "halogen_tauri_test_destroyed", Unsafe, [t|IO Int|])
     , ("js_release", "halogen_tauri_test_release", Unsafe, [t|JSText -> IO ()|])
     , ("js_outside", "halogen_tauri_test_outside", Unsafe, [t|Bool -> IO ()|])
     , ("js_reset", "halogen_tauri_test_reset", Unsafe, [t|IO ()|])
     ]
 )

-- | The Rust side emits the event to every target.
fire :: (ToJSON a) => Text -> a -> IO ()
fire = fireAt ""

-- | The Rust side emits the event to the window with this label.
fireTo :: (ToJSON a) => Text -> Text -> a -> IO ()
fireTo = fireAt

fireAt :: (ToJSON a) => Text -> Text -> a -> IO ()
fireAt target event payload = js_fire (toJSText target) (toJSText event) (toJSText (decodeUtf8 (Lazy.toStrict (encode payload))))

-- | How many listeners the bus has for the event, anywhere or on a window;
-- for every event, given @""@.
listening :: Text -> IO Int
listening = js_listening . toJSText

-- | How many times a listener was taken off.
unlistens :: IO Int
unlistens = js_unlistens

-- | An emit the page made, as the double saw it.
data Emitted = Emitted {target :: Maybe Text, event :: Text, payload :: Value}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

emitted :: IO [Emitted]
emitted = do
  json <- fromJSText <$> js_emitted
  either (fail . ("the double's emits do not decode: " <>)) pure (eitherDecodeStrict' (encodeUtf8 json))

-- | How many times the current window was destroyed.
destroyed :: IO Int
destroyed = js_destroyed

-- | Let the held command of this name answer.
release :: Text -> IO ()
release = js_release . toJSText

-- | Run the action without the global API, as in a plain browser.
outsideTauri :: IO a -> IO a
outsideTauri = bracket_ (js_outside True) (js_outside False)

-- | Forget everything the double saw and drop its listeners: a run under
-- browser GHCi finds the page as the run before left it. Natively it does
-- nothing.
reset :: IO ()
reset = js_reset
