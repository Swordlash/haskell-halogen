{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'MonadDOM' for the GHC JavaScript backend.
--
-- The FFI here names global functions linked from @jsbits/monad_dom.js@; see
-- "Halogen.VDom.DOM.Monad.WASM" for the contrast with the wasm backend.
--
-- The instance is an orphan because the class lives in
-- "Halogen.VDom.DOM.Monad.Class". Exactly one backend module is compiled into
-- the package (the cabal file selects on @arch@), so the instances can never
-- overlap.
module Halogen.VDom.DOM.Monad.JS () where

import Data.Foreign
import GHC.JS.Foreign.Callback
import GHC.JS.Prim
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

-- implementation of MonadDOM for IO

foreign import javascript unsafe "js_create_text_node" js_create_text_node :: JSVal -> Document -> IO Node

foreign import javascript unsafe "js_set_text_content" js_set_text_content :: JSVal -> Node -> IO ()

foreign import javascript unsafe "js_create_element" js_create_element :: JSVal -> JSVal -> Document -> IO Element

foreign import javascript unsafe "js_insert_before" js_insert_before :: Node -> Node -> ParentNode -> IO ()

foreign import javascript unsafe "js_get_window" js_get_window :: IO Window

foreign import javascript unsafe "js_storage_read" js_storage_read :: JSVal -> JSVal -> IO (Nullable JSVal)

foreign import javascript unsafe "js_storage_write" js_storage_write :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe "js_storage_remove" js_storage_remove :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "js_storage_length" js_storage_length :: JSVal -> IO (Foreign Int)

foreign import javascript unsafe "js_storage_key" js_storage_key :: JSVal -> Int -> IO (Nullable JSVal)

foreign import javascript unsafe "js_get_document" js_get_document :: Window -> IO HTMLDocument

foreign import javascript unsafe "js_append_child" js_append_child :: Node -> ParentNode -> IO ()

foreign import javascript unsafe "js_replace_child" js_replace_child :: Node -> Node -> ParentNode -> IO ()

foreign import javascript unsafe "js_insert_child_ix" js_insert_child_ix :: Int -> Node -> ParentNode -> IO ()

foreign import javascript unsafe "js_remove_child" js_remove_child :: Node -> ParentNode -> IO ()

foreign import javascript unsafe "js_parent_node" js_parent_node :: Node -> IO (Nullable ParentNode)

foreign import javascript unsafe "js_next_sibling" js_next_sibling :: Node -> IO (Nullable Node)

foreign import javascript unsafe "js_set_attribute" js_set_attribute :: JSVal -> JSVal -> JSVal -> Element -> IO ()

foreign import javascript unsafe "js_set_property" js_set_property :: JSVal -> JSVal -> Element -> IO ()

foreign import javascript unsafe "js_unsafe_get_property" js_unsafe_get_property :: JSVal -> Element -> IO JSVal

foreign import javascript unsafe "js_remove_property" js_remove_property :: JSVal -> Element -> IO ()

foreign import javascript unsafe "js_remove_attribute" js_remove_attribute :: JSVal -> JSVal -> Element -> IO ()

foreign import javascript unsafe "js_has_attribute" js_has_attribute :: JSVal -> JSVal -> Element -> IO Bool

foreign import javascript unsafe "js_add_event_listener" js_add_event_listener :: JSVal -> EventListener -> EventTarget -> IO ()

foreign import javascript unsafe "js_remove_event_listener" js_remove_event_listener :: JSVal -> EventListener -> EventTarget -> IO ()

foreign import javascript unsafe "js_query_selector" js_query_selector :: JSVal -> ParentNode -> IO (Nullable Element)

foreign import javascript unsafe "js_ready_state" js_ready_state :: HTMLDocument -> IO JSVal

foreign import javascript unsafe "(($1) => { return $1; })"
  js_toJSBool :: Bool -> JSVal

foreign import javascript unsafe "(($1) => { return $1; })"
  js_toJSNum :: Double -> JSVal

instance MonadDOM BrowserDOM where
  type DomNode BrowserDOM = DOMTypes.Node
  type DomElement BrowserDOM = DOMTypes.Element
  type DomDocument BrowserDOM = DOMTypes.Document
  type DomEventListener BrowserDOM = DOMTypes.EventListener
  type DomEventTarget BrowserDOM = EventTypes.EventTarget

  elementToNode el = pure (coerce el)
  elementToEventTarget el = pure (coerce el)

  mkEventListener f = liftIO $ EventListener <$> asyncCallback1 (runBrowserDOM . f . Event)

  createTextNode txt doc = liftIO $ js_create_text_node (toJSString $ toS txt) doc
  setTextContent txt node = liftIO $ js_set_text_content (toJSString $ toS txt) node
  createElement ns (ElemName name) doc = liftIO $ js_create_element (maybe jsNull (toJSString . toS . unNamespace) ns) (toJSString $ toS name) doc
  insertChildIx ix child parent = liftIO $ js_insert_child_ix ix child (coerce parent)
  removeChild child parent = liftIO $ js_remove_child child (coerce parent)
  parentNode node = liftIO $ fmap Node . nullableToMaybe <$> js_parent_node node
  addEventListener (EventType et) listener target = liftIO $ js_add_event_listener (toJSString $ toS et) listener target

  removeEventListener (EventType et) listener@(EventListener clb) target = liftIO $ do
    js_remove_event_listener (toJSString $ toS et) listener target
    releaseCallback clb

instance MonadBrowserDOM BrowserDOM where
  insertBefore newNode sibling parent = liftIO $ js_insert_before newNode sibling (coerce parent)
  appendChild child parent = liftIO $ js_append_child child (coerce parent)
  replaceChild newChild oldChild parent = liftIO $ js_replace_child newChild oldChild (coerce parent)
  nextSibling node = liftIO $ fmap Node . nullableToMaybe <$> js_next_sibling node
  windowToEventTarget w = pure (coerce w)
  documentToNode d = pure (coerce d)
  window = liftIO $ js_get_window
  document w = liftIO $ js_get_document w
  querySelector (QuerySelector qs) parent = liftIO $ fmap Element . nullableToMaybe <$> js_query_selector (toJSString $ toS qs) (coerce parent)
  readyState doc = liftIO $ (fromMaybe ReadyState.Loading . ReadyState.parse . toS . fromJSString) <$> js_ready_state doc
  readStorageItem kind key = liftIO $ fmap foreignToString . nullableToMaybe <$> js_storage_read (storageName kind) (jsKey key)
  writeStorageItem kind key text = liftIO $ js_storage_write (storageName kind) (jsKey key) (jsKey text)
  removeStorageItem kind key = liftIO $ js_storage_remove (storageName kind) (jsKey key)

  -- `key(i)` rather than a list, because a list would have to be marshalled
  -- and this is what a store offers: the indices are stable for as long as
  -- nothing is added or removed, which is as much as a store promises anyway.
  storageItemKeys kind = liftIO $ do
    count <- foreignToInt <$> js_storage_length (storageName kind)
    catMaybes <$> for [0 .. count - 1] (\ix -> fmap foreignToString . nullableToMaybe <$> js_storage_key (storageName kind) ix)

instance MonadAttributes BrowserDOM where
  setAttribute ns (AttrName name) val el = liftIO $ js_set_attribute (maybe jsNull (toJSString . toS . unNamespace) ns) (toJSString $ toS name) (toJSString $ toS val) el
  setProperty (PropName name) val el = liftIO $ js_set_property (toJSString $ toS name) (propValueToJSVal val) el
  propertyEquals (PropName name) val el = liftIO $ unsafeRefEq <$> js_unsafe_get_property (toJSString $ toS name) el <*> pure (propValueToJSVal val)
  removeProperty (PropName name) el = liftIO $ js_remove_property (toJSString $ toS name) el
  removeAttribute ns (AttrName name) el = liftIO $ js_remove_attribute (maybe jsNull (toJSString . toS . unNamespace) ns) (toJSString $ toS name) el
  hasAttribute ns (AttrName name) el = liftIO $ js_has_attribute (maybe jsNull (toJSString . toS . unNamespace) ns) (toJSString $ toS name) el

-- | A key or a value, as the string the shim takes.
jsKey :: Text -> JSVal
jsKey = toJSString . toS

-- | Which store the browser is being asked for, as the string the shim
-- switches on.
storageName :: StorageKind -> JSVal
storageName = \case
  LocalStorage -> toJSString "local"
  SessionStorage -> toJSString "session"

propValueToJSVal :: PropValue a -> JSVal
propValueToJSVal (IntProp x) = toJSInt $ fromIntegral x
propValueToJSVal (NumProp x) = js_toJSNum x
propValueToJSVal (BoolProp x) = js_toJSBool x
propValueToJSVal (TxtProp x) = toJSString $ toS x
propValueToJSVal (ViaTxtProp f x) = toJSString $ toS $ f x
