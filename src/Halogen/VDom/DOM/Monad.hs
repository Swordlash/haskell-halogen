{-# LANGUAGE CPP #-}

module Halogen.VDom.DOM.Monad where

import HPrelude
import Halogen.VDom.Types
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element
import Web.DOM.Internal.Types
import Web.DOM.ParentNode
import Web.Event.Event
import Web.HTML.Common
import Web.HTML.HTMLDocument.ReadyState as ReadyState
import Web.UIEvent.MouseEvent

#if defined(javascript_HOST_ARCH)
import Data.Foreign
import GHC.JS.Prim
import GHC.JS.Foreign.Callback
#endif

data PropValue val where
  IntProp :: (Integral a) => !a -> PropValue a
  NumProp :: !Double -> PropValue Double
  BoolProp :: !Bool -> PropValue Bool
  TxtProp :: !Text -> PropValue Text
  ViaTxtProp :: (a -> Text) -> a -> PropValue a

class (Monad m) => MonadDOM m where
  mkEventListener :: (Event -> m ()) -> m EventListener

  createTextNode :: Text -> Document -> m Node
  setTextContent :: Text -> Node -> m ()
  createElement :: Maybe Namespace -> ElemName -> Document -> m Element
  insertBefore :: Node -> Node -> ParentNode -> m ()
  appendChild :: Node -> ParentNode -> m ()
  replaceChild :: Node -> Node -> ParentNode -> m ()
  insertChildIx :: Int -> Node -> ParentNode -> m ()
  removeChild :: Node -> ParentNode -> m ()
  parentNode :: Node -> m (Maybe ParentNode)
  nextSibling :: Node -> m (Maybe Node)
  setAttribute :: Maybe Namespace -> AttrName -> Text -> Element -> m ()
  setProperty :: PropName a -> PropValue a -> Element -> m ()
  unsafeGetProperty :: PropName a -> Element -> m a
  removeProperty :: PropName a -> Element -> m ()
  removeAttribute :: Maybe Namespace -> AttrName -> Element -> m ()
  hasAttribute :: Maybe Namespace -> AttrName -> Element -> m Bool

  addEventListener :: EventType -> EventListener -> EventTarget -> m ()
  removeEventListener :: EventType -> EventListener -> EventTarget -> m ()

  window :: m Window
  document :: Window -> m HTMLDocument

  querySelector :: QuerySelector -> ParentNode -> m (Maybe Element)
  readyState :: HTMLDocument -> m ReadyState
  log :: Text -> m ()

mouseHandler :: (MouseEvent -> a) -> Event -> a
mouseHandler = coerce

elementToNode :: Element -> Node
elementToNode = coerce

toEventTarget :: a -> EventTarget
toEventTarget = unsafeCoerce

#if defined(javascript_HOST_ARCH)
-- implementation of MonadDOM for IO

foreign import javascript unsafe "js_create_text_node" js_create_text_node :: JSVal -> Document -> IO Node
foreign import javascript unsafe "js_set_text_content" js_set_text_content :: JSVal -> Node -> IO ()
foreign import javascript unsafe "js_create_element" js_create_element :: JSVal -> JSVal -> Document -> IO Element
foreign import javascript unsafe "js_insert_before" js_insert_before :: Node -> Node -> ParentNode -> IO ()
foreign import javascript unsafe "js_get_window" js_get_window :: IO Window
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

foreign import javascript unsafe "(($1) => { console.log($1); })"
  js_log :: JSVal -> IO ()

instance MonadDOM IO where
  mkEventListener f = EventListener <$> asyncCallback1 (f . Event)

  window = js_get_window
  document = js_get_document
  createTextNode txt doc = js_create_text_node (toJSString $ toS txt) doc
  setTextContent txt node = js_set_text_content (toJSString $ toS txt) node
  createElement ns (ElemName name) doc = js_create_element (maybe jsNull (toJSString . toS . unNamespace) ns) (toJSString $ toS name) doc
  insertBefore newNode sibling parent = js_insert_before newNode sibling parent
  appendChild child parent = js_append_child child parent
  replaceChild newChild oldChild parent = js_replace_child newChild oldChild parent
  insertChildIx ix child parent = js_insert_child_ix ix child parent
  removeChild child parent = js_remove_child child parent
  parentNode node = fmap ParentNode . nullableToMaybe <$> js_parent_node node
  nextSibling node = fmap Node . nullableToMaybe <$> js_next_sibling node
  setAttribute ns (AttrName name) val el = js_set_attribute (maybe jsNull (toJSString . toS . unNamespace) ns) (toJSString $ toS name) (toJSString $ toS val) el
  setProperty (PropName name) val el = js_set_property (toJSString $ toS name) (propValueToJSVal val) el
  unsafeGetProperty (PropName name) el = unsafeFromForeign <$> js_unsafe_get_property (toJSString $ toS name) el
  removeProperty (PropName name) el = js_remove_property (toJSString $ toS name) el
  removeAttribute ns (AttrName name) el = js_remove_attribute (maybe jsNull (toJSString . toS . unNamespace) ns) (toJSString $ toS name) el
  hasAttribute ns (AttrName name) el = js_has_attribute (maybe jsNull (toJSString . toS . unNamespace) ns) (toJSString $ toS name) el
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
propValueToJSVal (TxtProp x) = toJSString $ toS x
propValueToJSVal (ViaTxtProp f x) = toJSString $ toS $ f x


#endif
