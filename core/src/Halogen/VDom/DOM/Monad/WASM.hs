{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'MonadDOM' for the GHC WebAssembly backend.
--
-- The instance is an orphan because the class lives in
-- "Halogen.VDom.DOM.Monad.Class". Exactly one backend module is compiled into
-- the package (the cabal file selects on @arch@), so the instances can never
-- overlap.
module Halogen.VDom.DOM.Monad.WASM () where

import Data.Foreign
import GHC.Wasm.Prim
import HPrelude
import Halogen.VDom.DOM.Monad.Browser
import Halogen.VDom.DOM.Monad.Class
import Halogen.VDom.Types
import Web.DOM.Internal.Types
import Web.DOM.Internal.Types qualified as DOMTypes
import Web.DOM.ParentNode
import Web.Event.Event
import Web.Event.Internal.Types qualified as EventTypes
import Web.HTML.Common
import Web.HTML.HTMLDocument.ReadyState as ReadyState

-- The wasm backend embeds JavaScript snippets in the generated JSFFI module;
-- unlike the JavaScript backend it does not link global functions from jsbits.

foreign import javascript unsafe "$2.createTextNode($1)" js_create_text_node :: JSVal -> Document -> IO Node

foreign import javascript unsafe "$2.textContent = $1" js_set_text_content :: JSVal -> Node -> IO ()

foreign import javascript unsafe "$1 == null ? $3.createElement($2) : $3.createElementNS($1, $2)" js_create_element :: JSVal -> JSVal -> Document -> IO Element

foreign import javascript unsafe "$1 !== $2.previousSibling && $3.insertBefore($1, $2)" js_insert_before :: Node -> Node -> ParentNode -> IO ()

foreign import javascript unsafe "globalThis.window" js_get_window :: IO Window

foreign import javascript unsafe "(($1 === 'local' ? globalThis.localStorage : globalThis.sessionStorage).getItem($2) ?? '')" js_storage_read :: JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe "($1 === 'local' ? globalThis.localStorage : globalThis.sessionStorage).setItem($2, $3)" js_storage_write :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.document" js_get_document :: Window -> IO HTMLDocument

foreign import javascript unsafe "$2.lastChild !== $1 && $2.appendChild($1)" js_append_child :: Node -> ParentNode -> IO ()

foreign import javascript unsafe "$1 !== $2 && $3.replaceChild($1, $2)" js_replace_child :: Node -> Node -> ParentNode -> IO ()

foreign import javascript unsafe "const n = $3.childNodes.item($1); if (n !== $2) $3.insertBefore($2, n)" js_insert_child_ix :: Int -> Node -> ParentNode -> IO ()

foreign import javascript unsafe "$2.removeChild($1)" js_remove_child :: Node -> ParentNode -> IO ()

foreign import javascript unsafe "$1.parentNode" js_parent_node :: Node -> IO (Nullable ParentNode)

foreign import javascript unsafe "$1.nextSibling" js_next_sibling :: Node -> IO (Nullable Node)

foreign import javascript unsafe "$1 == null ? $4.setAttribute($2, $3) : $4.setAttributeNS($1, $2, $3)" js_set_attribute :: JSVal -> JSVal -> JSVal -> Element -> IO ()

foreign import javascript unsafe "$3[$1] !== $2 && ($3[$1] = $2)" js_set_property :: JSVal -> JSVal -> Element -> IO ()

foreign import javascript unsafe "$3[$1] === $2" js_property_equals :: JSVal -> JSVal -> Element -> IO Bool

foreign import javascript unsafe "delete $2[$1]" js_remove_property :: JSVal -> Element -> IO ()

foreign import javascript unsafe "$1 == null ? $3.removeAttribute($2) : $3.removeAttributeNS($1, $2)" js_remove_attribute :: JSVal -> JSVal -> Element -> IO ()

foreign import javascript unsafe "$1 == null ? $3.hasAttribute($2) : $3.hasAttributeNS($1, $2)" js_has_attribute :: JSVal -> JSVal -> Element -> IO Bool

foreign import javascript unsafe "$3.addEventListener($1, $2, false)" js_add_event_listener :: JSVal -> EventListener -> EventTarget -> IO ()

foreign import javascript unsafe "$3.removeEventListener($1, $2, false)" js_remove_event_listener :: JSVal -> EventListener -> EventTarget -> IO ()

foreign import javascript unsafe "$2.querySelector($1)" js_query_selector :: JSVal -> ParentNode -> IO (Nullable Element)

foreign import javascript unsafe "$1.readyState" js_ready_state :: HTMLDocument -> IO JSVal

foreign import javascript unsafe "null" js_null :: JSVal

foreign import javascript unsafe "$1" js_toJSInt :: Int -> JSVal

foreign import javascript unsafe "$1" js_toJSBool :: Bool -> JSVal

foreign import javascript unsafe "$1" js_toJSNum :: Double -> JSVal

foreign import javascript "wrapper" js_mk_event_listener :: (JSVal -> IO ()) -> IO JSVal

jsStringVal :: Text -> JSVal
jsStringVal value = case toJSString (toS value) of
  JSString result -> result

instance MonadDOM BrowserDOM where
  type DomNode BrowserDOM = DOMTypes.Node
  type DomElement BrowserDOM = DOMTypes.Element
  type DomDocument BrowserDOM = DOMTypes.Document
  type DomEventListener BrowserDOM = DOMTypes.EventListener
  type DomEventTarget BrowserDOM = EventTypes.EventTarget

  elementToNode el = pure (coerce el)
  elementToEventTarget el = pure (coerce el)

  mkEventListener f = liftIO $ EventListener <$> js_mk_event_listener (runBrowserDOM . f . Event)

  createTextNode txt doc = liftIO $ js_create_text_node (jsStringVal txt) doc
  setTextContent txt node = liftIO $ js_set_text_content (jsStringVal txt) node
  createElement ns (ElemName name) doc = liftIO $ js_create_element (maybe js_null (jsStringVal . unNamespace) ns) (jsStringVal name) doc
  insertChildIx a b c = liftIO $ js_insert_child_ix a b (coerce c)
  removeChild a b = liftIO $ js_remove_child a (coerce b)
  parentNode node = liftIO $ fmap Node . nullableToMaybe <$> js_parent_node node
  addEventListener (EventType eventType) listener target = liftIO $ js_add_event_listener (jsStringVal eventType) listener target
  removeEventListener (EventType eventType) listener@(EventListener callback) target = liftIO $ do
    js_remove_event_listener (jsStringVal eventType) listener target
    freeJSVal callback

instance MonadBrowserDOM BrowserDOM where
  insertBefore a b c = liftIO $ js_insert_before a b (coerce c)
  appendChild a b = liftIO $ js_append_child a (coerce b)
  replaceChild a b c = liftIO $ js_replace_child a b (coerce c)
  nextSibling node = liftIO $ fmap Node . nullableToMaybe <$> js_next_sibling node
  windowToEventTarget w = pure (coerce w)
  documentToNode d = pure (coerce d)
  window = liftIO $ js_get_window
  document w = liftIO $ js_get_document w
  querySelector (QuerySelector selector) parent = liftIO $ fmap Element . nullableToMaybe <$> js_query_selector (jsStringVal selector) (coerce parent)
  readyState doc = liftIO $ (fromMaybe ReadyState.Loading . ReadyState.parse . foreignToString) <$> js_ready_state doc
  readStorage kind = liftIO $ foreignToString <$> js_storage_read (storageName kind) (jsStringVal storageKey)
  writeStorage kind text = liftIO $ js_storage_write (storageName kind) (jsStringVal storageKey) (jsStringVal text)

-- | Which store the browser is being asked for, as the string the expressions
-- above switch on.
storageName :: StorageKind -> JSVal
storageName = \case
  LocalStorage -> jsStringVal "local"
  SessionStorage -> jsStringVal "session"

propValueToJSVal :: PropValue a -> JSVal
propValueToJSVal (IntProp x) = js_toJSInt $ fromIntegral x
propValueToJSVal (NumProp x) = js_toJSNum x
propValueToJSVal (BoolProp x) = js_toJSBool x
propValueToJSVal (TxtProp x) = jsStringVal x
propValueToJSVal (ViaTxtProp f x) = jsStringVal $ f x

instance MonadAttributes BrowserDOM where
  setAttribute ns (AttrName name) val el = liftIO $ js_set_attribute (maybe js_null (jsStringVal . unNamespace) ns) (jsStringVal name) (jsStringVal val) el
  setProperty (PropName name) val el = liftIO $ js_set_property (jsStringVal name) (propValueToJSVal val) el
  propertyEquals (PropName name) val el = liftIO $ js_property_equals (jsStringVal name) (propValueToJSVal val) el
  removeProperty (PropName name) el = liftIO $ js_remove_property (jsStringVal name) el
  removeAttribute ns (AttrName name) el = liftIO $ js_remove_attribute (maybe js_null (jsStringVal . unNamespace) ns) (jsStringVal name) el
  hasAttribute ns (AttrName name) el = liftIO $ js_has_attribute (maybe js_null (jsStringVal . unNamespace) ns) (jsStringVal name) el
