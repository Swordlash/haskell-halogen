{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-orphans #-}
module Halogen.VDom.DOM.Monad.WASM (module Halogen.VDom.DOM.Monad.WASM) where 

#if defined(wasm32_HOST_ARCH)
import HPrelude
import Halogen.VDom.Types
import Web.DOM.Element
import Web.DOM.Internal.Types
import Web.DOM.ParentNode
import Web.Event.Event
import Web.HTML.Common
import Web.HTML.HTMLDocument.ReadyState as ReadyState

import Data.Foreign.WASM
import GHC.Wasm.Prim 

import Halogen.VDom.DOM.Monad.Common

foreign import javascript "wrapper sync" asyncCallback1 :: (JSVal -> IO ()) -> IO JSVal -- TODO async? doesn't work
foreign import javascript unsafe "null" jsNull :: JSVal

releaseCallback :: JSVal -> IO ()
releaseCallback = freeJSVal

-- implementation of MonadDOM for IO

foreign import javascript unsafe "js_create_text_node" js_create_text_node :: JSString -> Document -> IO Node
foreign import javascript unsafe "js_set_text_content" js_set_text_content :: JSString -> Node -> IO ()
foreign import javascript unsafe "js_create_element" js_create_element :: JSVal -> JSString -> Document -> IO Element
foreign import javascript unsafe "js_insert_before" js_insert_before :: Node -> Node -> ParentNode -> IO ()
foreign import javascript unsafe "js_get_window" js_get_window :: IO Window
foreign import javascript unsafe "js_get_document" js_get_document :: Window -> IO HTMLDocument
foreign import javascript unsafe "js_append_child" js_append_child :: Node -> ParentNode -> IO ()
foreign import javascript unsafe "js_replace_child" js_replace_child :: Node -> Node -> ParentNode -> IO ()
foreign import javascript unsafe "js_insert_child_ix" js_insert_child_ix :: Int -> Node -> ParentNode -> IO ()
foreign import javascript unsafe "js_remove_child" js_remove_child :: Node -> ParentNode -> IO ()
foreign import javascript unsafe "js_parent_node" js_parent_node :: Node -> IO (Nullable ParentNode)
foreign import javascript unsafe "js_next_sibling" js_next_sibling :: Node -> IO (Nullable Node)
foreign import javascript unsafe "js_set_attribute" js_set_attribute :: JSVal -> JSString -> JSString -> Element -> IO ()
foreign import javascript unsafe "js_set_property" js_set_property :: JSString -> JSVal -> Element -> IO ()
foreign import javascript unsafe "js_unsafe_get_property" js_unsafe_get_property :: JSString -> Element -> IO JSVal
foreign import javascript unsafe "js_remove_property" js_remove_property :: JSString -> Element -> IO ()
foreign import javascript unsafe "js_remove_attribute" js_remove_attribute :: JSVal -> JSString -> Element -> IO ()
foreign import javascript unsafe "js_has_attribute" js_has_attribute :: JSVal -> JSString -> Element -> IO Bool
foreign import javascript unsafe "js_add_event_listener" js_add_event_listener :: JSString -> EventListener -> EventTarget -> IO ()
foreign import javascript unsafe "js_remove_event_listener" js_remove_event_listener :: JSString -> EventListener -> EventTarget -> IO ()
foreign import javascript unsafe "js_query_selector" js_query_selector :: JSString -> ParentNode -> IO (Nullable Element)
foreign import javascript unsafe "js_ready_state" js_ready_state :: HTMLDocument -> IO JSString

foreign import javascript unsafe "$1"
  js_toJSBool :: Bool -> JSVal

foreign import javascript unsafe "$1"
  js_toJSNum :: Double -> JSVal

foreign import javascript unsafe "console.log($1)"
  js_log :: JSString -> IO ()

instance MonadDOM IO where
  mkEventListener f = EventListener <$> asyncCallback1 (f . Event)

  window = js_get_window
  document = js_get_document
  createTextNode txt doc = js_create_text_node (toJSString $ toS txt) doc
  setTextContent txt node = js_set_text_content (toJSString $ toS txt) node
  createElement ns (ElemName name) doc = js_create_element (maybe jsNull (coerce . toJSString . toS . unNamespace) ns) (toJSString $ toS name) doc
  insertBefore newNode sibling parent = js_insert_before newNode sibling parent
  appendChild child parent = js_append_child child parent
  replaceChild newChild oldChild parent = js_replace_child newChild oldChild parent
  insertChildIx ix child parent = js_insert_child_ix ix child parent
  removeChild child parent = js_remove_child child parent
  parentNode node = fmap ParentNode . nullableToMaybe <$> js_parent_node node
  nextSibling node = fmap Node . nullableToMaybe <$> js_next_sibling node
  setAttribute ns (AttrName name) val el = js_set_attribute (maybe jsNull (coerce . toJSString . toS . unNamespace) ns) (toJSString $ toS name) (toJSString $ toS val) el
  setProperty (PropName name) val el = js_set_property (toJSString $ toS name) (propValueToJSVal val) el
  unsafeGetProperty (PropName name) el = unsafeFromForeign <$> js_unsafe_get_property (toJSString $ toS name) el
  removeProperty (PropName name) el = js_remove_property (toJSString $ toS name) el
  removeAttribute ns (AttrName name) el = js_remove_attribute (maybe jsNull (coerce . toJSString . toS . unNamespace) ns) (toJSString $ toS name) el
  hasAttribute ns (AttrName name) el = js_has_attribute (maybe jsNull (coerce . toJSString . toS . unNamespace) ns) (toJSString $ toS name) el
  addEventListener (EventType et) listener target = js_add_event_listener (toJSString $ toS et) listener target
  
  removeEventListener (EventType et) listener@(EventListener clb) target = do
    js_remove_event_listener (toJSString $ toS et) listener target
    releaseCallback clb

  querySelector (QuerySelector qs) parent = fmap Element . nullableToMaybe <$> js_query_selector (toJSString $ toS qs) parent
  readyState doc = (fromMaybe ReadyState.Loading . ReadyState.parse . toS . fromJSString) <$> js_ready_state doc
  log txt = js_log (toJSString $ toS txt)

propValueToJSVal :: PropValue a -> JSVal
propValueToJSVal (IntProp x) = toJSInt $ fromIntegral x
propValueToJSVal (NumProp x) = js_toJSNum x
propValueToJSVal (BoolProp x) = js_toJSBool x
propValueToJSVal (TxtProp x) = coerce $ toJSString $ toS x
propValueToJSVal (ViaTxtProp f x) = coerce $ toJSString $ toS $ f x
#endif