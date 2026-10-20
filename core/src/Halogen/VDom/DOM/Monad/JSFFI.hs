{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'MonadDOM' for the browser backends, JavaScript and WebAssembly alike.
--
-- The bindings are declared once, through 'browserJS': the JavaScript
-- backend links @jsbits/monad_dom.js@ and @jsbits/web_browser.js@ and calls
-- their functions by name, the wasm backend embeds the same files into its
-- JSFFI module. What differs between the two (how a string, a number or a
-- callback crosses the FFI) is what "Halogen.JSBits.Value" names once for
-- both, so there is no CPP here.
--
-- The instances are orphans because the class lives in
-- "Halogen.VDom.DOM.Monad.Class". This module is compiled only for the
-- browser backends (the cabal file selects on @arch@), and the native
-- backend's "Halogen.VDom.DOM.Monad.Native" gives 'MemDOM' its instances
-- instead, so no two instances can meet.
module Halogen.VDom.DOM.Monad.JSFFI () where

import Data.Foreign
import HPrelude
import Halogen.JSBits (JSText, JSVal, Safety (..), browserJS, freeCallback, fromJSText, jsNull, mkSyncCallback, toJSBool, toJSDouble, toJSInt, toJSText)
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

$( browserJS
     ["jsbits/monad_dom.js", "jsbits/web_browser.js"]
     [ ("js_create_text_node", "js_create_text_node", Unsafe, [t|JSText -> Document -> IO Node|])
     , ("js_set_text_content", "js_set_text_content", Unsafe, [t|JSText -> Node -> IO ()|])
     , ("js_create_element", "js_create_element", Unsafe, [t|JSVal -> JSText -> Document -> IO Element|])
     , ("js_insert_before", "js_insert_before", Unsafe, [t|Node -> Node -> ParentNode -> IO ()|])
     , ("js_get_window", "js_get_window", Unsafe, [t|IO Window|])
     , ("js_storage_read", "js_storage_read", Unsafe, [t|JSText -> JSText -> IO (Nullable JSVal)|])
     , ("js_storage_write", "js_storage_write", Unsafe, [t|JSText -> JSText -> JSText -> IO ()|])
     , ("js_storage_remove", "js_storage_remove", Unsafe, [t|JSText -> JSText -> IO ()|])
     , ("js_storage_length", "js_storage_length", Unsafe, [t|JSText -> IO (Foreign Int)|])
     , ("js_storage_key", "js_storage_key", Unsafe, [t|JSText -> Int -> IO (Nullable JSVal)|])
     , ("js_get_document", "js_get_document", Unsafe, [t|Window -> IO HTMLDocument|])
     , ("js_append_child", "js_append_child", Unsafe, [t|Node -> ParentNode -> IO ()|])
     , ("js_replace_child", "js_replace_child", Unsafe, [t|Node -> Node -> ParentNode -> IO ()|])
     , ("js_insert_child_ix", "js_insert_child_ix", Unsafe, [t|Int -> Node -> ParentNode -> IO ()|])
     , ("js_remove_child", "js_remove_child", Unsafe, [t|Node -> ParentNode -> IO ()|])
     , ("js_parent_node", "js_parent_node", Unsafe, [t|Node -> IO (Nullable ParentNode)|])
     , ("js_next_sibling", "js_next_sibling", Unsafe, [t|Node -> IO (Nullable Node)|])
     , ("js_set_attribute", "js_set_attribute", Unsafe, [t|JSVal -> JSText -> JSText -> Element -> IO ()|])
     , ("js_set_property", "js_set_property", Unsafe, [t|JSText -> JSVal -> Element -> IO ()|])
     , ("js_property_equals", "js_property_equals", Unsafe, [t|JSText -> JSVal -> Element -> IO Bool|])
     , ("js_remove_property", "js_remove_property", Unsafe, [t|JSText -> Element -> IO ()|])
     , ("js_remove_attribute", "js_remove_attribute", Unsafe, [t|JSVal -> JSText -> Element -> IO ()|])
     , ("js_has_attribute", "js_has_attribute", Unsafe, [t|JSVal -> JSText -> Element -> IO Bool|])
     , ("js_add_event_listener", "js_add_event_listener", Unsafe, [t|JSText -> EventListener -> EventTarget -> IO ()|])
     , ("js_remove_event_listener", "js_remove_event_listener", Unsafe, [t|JSText -> EventListener -> EventTarget -> IO ()|])
     , ("js_query_selector", "js_query_selector", Unsafe, [t|JSText -> ParentNode -> IO (Nullable Element)|])
     , ("js_ready_state", "js_ready_state", Unsafe, [t|HTMLDocument -> IO JSText|])
     ]
 )

instance MonadDOM BrowserDOM where
  type DomNode BrowserDOM = DOMTypes.Node
  type DomElement BrowserDOM = DOMTypes.Element
  type DomDocument BrowserDOM = DOMTypes.Document
  type DomEventListener BrowserDOM = DOMTypes.EventListener
  type DomEventTarget BrowserDOM = EventTypes.EventTarget

  elementToNode el = pure (coerce el)
  elementToEventTarget el = pure (coerce el)

  -- A listener runs on the spot, as the browser runs listeners while it
  -- dispatches an event: what it does before it first waits happens during
  -- the dispatch ('preventDefault' counts), and the rest afterwards.
  mkEventListener f = liftIO $ EventListener <$> mkSyncCallback (runBrowserDOM . f . Event)

  createTextNode txt doc = liftIO $ js_create_text_node (toJSText txt) doc
  setTextContent txt node = liftIO $ js_set_text_content (toJSText txt) node
  createElement ns (ElemName name) doc = liftIO $ js_create_element (namespace ns) (toJSText name) doc
  insertChildIx ix child parent = liftIO $ js_insert_child_ix ix child (coerce parent)
  removeChild child parent = liftIO $ js_remove_child child (coerce parent)
  parentNode node = liftIO $ fmap Node . nullableToMaybe <$> js_parent_node node
  addEventListener (EventType eventType) listener target = liftIO $ js_add_event_listener (toJSText eventType) listener target
  removeEventListener (EventType eventType) listener@(EventListener callback) target = liftIO $ do
    js_remove_event_listener (toJSText eventType) listener target
    freeCallback callback

instance MonadBrowserDOM BrowserDOM where
  insertBefore newNode sibling parent = liftIO $ js_insert_before newNode sibling (coerce parent)
  appendChild child parent = liftIO $ js_append_child child (coerce parent)
  replaceChild newChild oldChild parent = liftIO $ js_replace_child newChild oldChild (coerce parent)
  nextSibling node = liftIO $ fmap Node . nullableToMaybe <$> js_next_sibling node
  windowToEventTarget w = pure (coerce w)
  documentToNode d = pure (coerce d)
  window = liftIO js_get_window
  document w = liftIO $ js_get_document w
  querySelector (QuerySelector selector) parent = liftIO $ fmap Element . nullableToMaybe <$> js_query_selector (toJSText selector) (coerce parent)
  readyState doc = liftIO $ fromMaybe ReadyState.Loading . ReadyState.parse . fromJSText <$> js_ready_state doc
  readStorageItem kind key = liftIO $ fmap foreignToString . nullableToMaybe <$> js_storage_read (storageName kind) (toJSText key)
  writeStorageItem kind key text = liftIO $ js_storage_write (storageName kind) (toJSText key) (toJSText text)
  removeStorageItem kind key = liftIO $ js_storage_remove (storageName kind) (toJSText key)

  -- `key(i)` rather than a list, because a list would have to be marshalled
  -- and this is what a store offers: the indices are stable for as long as
  -- nothing is added or removed, which is as much as a store promises anyway.
  storageItemKeys kind = liftIO $ do
    count <- foreignToInt <$> js_storage_length (storageName kind)
    catMaybes <$> for [0 .. count - 1] (\ix -> fmap foreignToString . nullableToMaybe <$> js_storage_key (storageName kind) ix)

instance MonadAttributes BrowserDOM where
  setAttribute ns (AttrName name) val el = liftIO $ js_set_attribute (namespace ns) (toJSText name) (toJSText val) el
  setProperty (PropName name) val el = liftIO $ js_set_property (toJSText name) (propValueToJSVal val) el
  propertyEquals (PropName name) val el = liftIO $ js_property_equals (toJSText name) (propValueToJSVal val) el
  removeProperty (PropName name) el = liftIO $ js_remove_property (toJSText name) el
  removeAttribute ns (AttrName name) el = liftIO $ js_remove_attribute (namespace ns) (toJSText name) el
  hasAttribute ns (AttrName name) el = liftIO $ js_has_attribute (namespace ns) (toJSText name) el

-- | A namespace, or @null@ for none, as the jsbits take it.
namespace :: Maybe Namespace -> JSVal
namespace = maybe jsNull (toJSText . unNamespace)

-- | Which store the browser is being asked for, as the string the jsbits
-- switch on.
storageName :: StorageKind -> JSText
storageName = \case
  LocalStorage -> toJSText "local"
  SessionStorage -> toJSText "session"

propValueToJSVal :: PropValue a -> JSVal
propValueToJSVal = \case
  IntProp x -> toJSInt (fromIntegral x)
  NumProp x -> toJSDouble x
  BoolProp x -> toJSBool x
  TxtProp x -> toJSText x
  ViaTxtProp f x -> toJSText (f x)
