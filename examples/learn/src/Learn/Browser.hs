{-# LANGUAGE TemplateHaskell #-}

-- | What the tutorial asks of the page's JavaScript: the compiler (GHC in
-- WebAssembly, @js\/playground.js@) and the code editor (CodeMirror,
-- @js\/editor.js@), both bundled into the page's @learn.js@, which publishes
-- them for the bindings of @jsbits\/learn.js@.
--
-- The bindings are 'browserJS' ones, so natively, where the prerender runs,
-- they are inert: the app calls none of them there ('inBrowser').
module Learn.Browser
  ( -- * The page
    rootfsAddress
  , narrowScreen
  , confirm
  , scrollToTop

    -- * The compiler
  , Sizes (..)
  , describe
  , Progress (..)
  , startCompiler
  , ensureLayers
  , CompileRequest (..)
  , RunResult (..)
  , compile
  , typecheck
  , endRun

    -- * The editor
  , Editor
  , EditorEvent (..)
  , createEditor
  , editorText
  , setEditorText
  , setEditorReadOnly
  , showDiagnostics
  , showTypes
  , gotoPosition
  , destroyEditor
  )
where

import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Halogen.JSBits (Callback, JSText, JSVal, Safety (..), browserJS, freeCallback, fromJSText, jsValText, mkCallback, toJSText)
import Halogen.JSBits.Promise (JSError (..), Promise (..), await)
import Learn.Diagnostics (Diagnostic)
import Learn.Types (Binding, splitTypes)
import Protolude
import Web.DOM.Internal.Types (HTMLElement (..))

$( browserJS
     ["jsbits/learn.js"]
     [ ("js_rootfs", "halogen_learn_rootfs", Unsafe, [t|IO JSText|])
     , ("js_narrow", "halogen_learn_narrow", Unsafe, [t|IO Bool|])
     , ("js_confirm", "halogen_learn_confirm", Unsafe, [t|JSText -> IO Bool|])
     , ("js_scroll_to_top", "halogen_learn_scroll_to_top", Unsafe, [t|IO ()|])
     , ("js_describe", "halogen_learn_describe", Unsafe, [t|JSText -> IO Promise|])
     , ("js_start", "halogen_learn_start", Unsafe, [t|JSText -> JSText -> Callback -> IO Promise|])
     , ("js_ensure_layers", "halogen_learn_ensure_layers", Unsafe, [t|JSText -> IO Promise|])
     , ("js_compile", "halogen_learn_compile", Unsafe, [t|JSText -> IO Promise|])
     , ("js_check", "halogen_learn_check", Unsafe, [t|JSText -> IO Promise|])
     , ("js_end_run", "halogen_learn_end_run", Unsafe, [t|IO Promise|])
     , ("js_editor_create", "halogen_learn_editor_create", Unsafe, [t|HTMLElement -> JSText -> Bool -> Callback -> IO JSVal|])
     , ("js_editor_text", "halogen_learn_editor_text", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_editor_set_text", "halogen_learn_editor_set_text", Unsafe, [t|JSVal -> JSText -> IO ()|])
     , ("js_editor_set_read_only", "halogen_learn_editor_set_read_only", Unsafe, [t|JSVal -> Bool -> IO ()|])
     , ("js_editor_diagnostics", "halogen_learn_editor_diagnostics", Unsafe, [t|JSVal -> JSText -> IO ()|])
     , ("js_editor_types", "halogen_learn_editor_types", Unsafe, [t|JSVal -> JSText -> IO ()|])
     , ("js_editor_goto", "halogen_learn_editor_goto", Unsafe, [t|JSVal -> Int -> Int -> IO ()|])
     , ("js_editor_destroy", "halogen_learn_editor_destroy", Unsafe, [t|JSVal -> IO ()|])
     ]
 )

-- | Where the rootfs is: @rootfs\/@ under the page's base.
rootfsAddress :: IO Text
rootfsAddress = fromJSText <$> js_rootfs

-- | A screen under 900 pixels wide, where the compiler does not load by
-- itself.
narrowScreen :: IO Bool
narrowScreen = js_narrow

-- | Ask the reader, with the browser's own dialog.
confirm :: Text -> IO Bool
confirm = js_confirm . toJSText

-- | Scroll the page back to its top, as loading another page would: on a
-- narrow screen the page scrolls as a whole, and a lesson followed from the
-- bottom of the last one should start at its title.
scrollToTop :: IO ()
scrollToTop = js_scroll_to_top

-- | What the compiler downloads, for the page to say: GHC's version and each
-- archive's size in bytes, when the server says.
data Sizes = Sizes
  { ghc :: Text
  , base :: Maybe Int
  , layers :: [(Text, Maybe Int)]
  }
  deriving stock (Eq, Show)

instance FromJSON Sizes where
  parseJSON = withObject "Sizes" $ \o -> do
    ghc <- o .: "ghc"
    base <- o .:? "base"
    layers <- traverse (withObject "layer" (\l -> (,) <$> l .: "name" <*> l .:? "size")) =<< o .: "layers"
    pure Sizes {ghc, base, layers}

-- | Read the rootfs' manifest.
describe :: Text -> IO (Either Text Sizes)
describe root = decoded =<< await =<< js_describe (toJSText root)

-- | Where the compiler's loading is.
data Progress
  = ReadingManifest
  | -- | Bytes so far, of how many if that is known, and every megabyte
    -- loaded with it, before.
    Downloading Int (Maybe Int) Double
  | -- | Reading an archive from the browser's cache, with nothing coming
    -- from the network meanwhile.
    ReadingCache
  | Extracting
  | Starting
  | LoadingLayers
  | -- | Ready, with this many megabytes loaded in all.
    Ready Double
  | LoadFailed Text
  deriving stock (Eq, Show)

instance FromJSON Progress where
  parseJSON = withObject "Progress" $ \o ->
    o .: "phase" >>= \case
      "manifest" -> pure ReadingManifest
      "download" -> Downloading <$> o .: "loaded" <*> o .:? "total" <*> o .:? "mb" .!= 0
      "cache" -> pure ReadingCache
      "extract" -> pure Extracting
      "start" -> pure Starting
      "layers" -> pure LoadingLayers
      "ready" -> Ready <$> o .:? "mb" .!= 0
      "error" -> LoadFailed <$> o .:? "message" .!= "the compiler failed to load"
      other -> fail ("no such phase: " <> other)

-- | Download, extract and start the compiler, and these layers alongside,
-- telling @onProgress@ how it goes, then and on every later 'ensureLayers'.
-- Waits until the compiler has started, or failed to. A layer's failure is
-- not the compiler's: the first 'ensureLayers' that names the layer reports
-- it, so a compile that needs the layers waits for that.
--
-- Once the compiler has started, @onProgress@ is what the page tells of
-- every later load, for as long as the page lives, so its callback stays;
-- when it failed, the page has let go of it, and it is freed here.
startCompiler :: Text -> [Text] -> (Progress -> IO ()) -> IO (Either Text ())
startCompiler root layers onProgress = do
  callback <- mkCallback $ \value -> either (const pass) onProgress (decode (jsValText value))
  started <- void <$> (settled =<< await =<< js_start (toJSText root) (encodeText layers) callback)
  when (isLeft started) (freeCallback callback)
  pure started

-- | Extract the layers named that are not yet, once the compiler has
-- started.
ensureLayers :: [Text] -> IO (Either Text ())
ensureLayers layers = void <$> (settled =<< await =<< js_ensure_layers (encodeText layers))

-- | A compile: the two modules, and what to pass to GHC beside what every
-- compile passes (@-package haskell-halogen-core@, @-i\/tmp@).
data CompileRequest = CompileRequest
  { lesson :: Text
  , main :: Text
  , args :: [Text]
  , layers :: [Text]
  }

instance ToJSON CompileRequest where
  toJSON r = object ["lesson" .= r.lesson, "main" .= r.main, "args" .= r.args, "layers" .= r.layers]

-- | What a compile or a check did: @ok@ when the program's @main@ ran to its
-- end (for a check, when the program typechecked), @compiled@ when GHC
-- compiled it (so that a @main@ that failed is told from code that did not
-- compile), what the program and GHC wrote, the types of the lesson's
-- top-level bindings, if it typechecked, and how long it all took.
data RunResult = RunResult
  { ok :: Bool
  , compiled :: Bool
  , failure :: Maybe Text
  , stdout :: Text
  -- ^ What the program wrote, without the lines of 'types'.
  , stderr :: Text
  , types :: [Binding]
  , ms :: Int
  }
  deriving stock (Eq, Show)

instance FromJSON RunResult where
  parseJSON = withObject "RunResult" $ \o -> do
    (types, out) <- splitTypes <$> o .: "stdout"
    RunResult
      <$> o
      .: "ok"
      <*> o .: "compiled"
      <*> o .:? "error"
      <*> pure out
      <*> o .: "stderr"
      <*> pure types
      <*> o .: "ms"

-- | Compile the lesson and run its @Main.main@. One compile runs at a time;
-- a second waits for the first.
compile :: CompileRequest -> IO (Either Text RunResult)
compile request = decoded =<< await =<< js_compile (encodeText request)

-- | Typecheck the lesson and run nothing: GHC's diagnostics, and the types
-- of the lesson's bindings when it typechecks (then @ok@). It waits for a
-- compile under way, and a compile asked for meanwhile waits for it.
typecheck :: CompileRequest -> IO (Either Text RunResult)
typecheck request = decoded =<< await =<< js_check (encodeText request)

-- | End whatever the last compile mounted, before the page takes its
-- element away (see @js\/playground.js@).
endRun :: IO ()
endRun = void (await =<< js_end_run)

-- | A CodeMirror editor, and the callback it calls.
data Editor = Editor JSVal Callback

data EditorEvent = Changed Text | CompileRequested
  deriving stock (Eq, Show)

instance FromJSON EditorEvent where
  parseJSON = withObject "EditorEvent" $ \o ->
    o .: "type" >>= \case
      "change" -> Changed <$> o .: "text"
      "compile" -> pure CompileRequested
      other -> fail ("no such event: " <> other)

-- | An editor in the element, which must be empty and stay the editor's:
-- rendered with no children, and never patched inside.
createEditor :: HTMLElement -> Text -> Bool -> (EditorEvent -> IO ()) -> IO Editor
createEditor element text readOnly onEvent = do
  callback <- mkCallback $ \value -> either (const pass) onEvent (decode (jsValText value))
  view <- js_editor_create element (toJSText text) readOnly callback
  pure (Editor view callback)

editorText :: Editor -> IO Text
editorText (Editor view _) = fromJSText <$> js_editor_text view

-- | Replace the editor's text; the editor reports it as a 'Changed'.
setEditorText :: Editor -> Text -> IO ()
setEditorText (Editor view _) = js_editor_set_text view . toJSText

setEditorReadOnly :: Editor -> Bool -> IO ()
setEditorReadOnly (Editor view _) = js_editor_set_read_only view

-- | Mark these in the editor, in place of what was marked.
showDiagnostics :: Editor -> [Diagnostic] -> IO ()
showDiagnostics (Editor view _) = js_editor_diagnostics view . encodeText

-- | The types the tooltips show over these bindings' names, in place of
-- the ones before.
showTypes :: Editor -> [Binding] -> IO ()
showTypes (Editor view _) = js_editor_types view . encodeText

-- | Put the cursor at this line and column (from 1), and focus the editor.
gotoPosition :: Editor -> Int -> Int -> IO ()
gotoPosition (Editor view _) = js_editor_goto view

destroyEditor :: Editor -> IO ()
destroyEditor (Editor view callback) = js_editor_destroy view >> freeCallback callback

encodeText :: (ToJSON a) => a -> JSText
encodeText = toJSText . decodeUtf8 . toS . Aeson.encode

decode :: (FromJSON a) => Text -> Either Text a
decode t = first toS (Aeson.eitherDecodeStrict (encodeUtf8 t))

-- | A promise that resolved with a JSON string, decoded; or why not.
decoded :: (FromJSON a) => Either JSError JSVal -> IO (Either Text a)
decoded outcome = pure (settled' outcome >>= decode . jsValText)
  where
    settled' = first (\e -> e.message)

settled :: Either JSError JSVal -> IO (Either Text JSVal)
settled = pure . first (\e -> e.message)
