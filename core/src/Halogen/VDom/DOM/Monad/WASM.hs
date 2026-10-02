{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'MonadDOM' for the GHC WebAssembly backend.
--
-- The instance is an orphan because the class lives in
-- "Halogen.VDom.DOM.Monad.Class". Exactly one backend module is compiled into
-- the package (the cabal file selects on @arch@), so the instances can never
-- overlap.
module Halogen.VDom.DOM.Monad.WASM () where

#if defined(wasm32_HOST_ARCH)
import Halogen.JSBits (wasmJS, Safety (..))
#endif

import Data.Foreign
import GHC.Conc (ThreadStatus (..), threadStatus)
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

-- Both backends use the same jsbits; WASM embeds them into its JSFFI module.
$( wasmJS
     ["jsbits/monad_dom.js", "jsbits/web_browser.js"]
     [ ("js_create_text_node", "js_create_text_node", Unsafe, [t|JSVal -> Document -> IO Node|])
     , ("js_set_text_content", "js_set_text_content", Unsafe, [t|JSVal -> Node -> IO ()|])
     , ("js_create_element", "js_create_element", Unsafe, [t|JSVal -> JSVal -> Document -> IO Element|])
     , ("js_insert_before", "js_insert_before", Unsafe, [t|Node -> Node -> ParentNode -> IO ()|])
     , ("js_get_window", "js_get_window", Unsafe, [t|IO Window|])
     , ("js_storage_read", "js_storage_read", Unsafe, [t|JSVal -> JSVal -> IO (Nullable JSVal)|])
     , ("js_storage_write", "js_storage_write", Unsafe, [t|JSVal -> JSVal -> JSVal -> IO ()|])
     , ("js_storage_remove", "js_storage_remove", Unsafe, [t|JSVal -> JSVal -> IO ()|])
     , ("js_storage_length", "js_storage_length", Unsafe, [t|JSVal -> IO (Foreign Int)|])
     , ("js_storage_key", "js_storage_key", Unsafe, [t|JSVal -> Int -> IO (Nullable JSVal)|])
     , ("js_get_document", "js_get_document", Unsafe, [t|Window -> IO HTMLDocument|])
     , ("js_append_child", "js_append_child", Unsafe, [t|Node -> ParentNode -> IO ()|])
     , ("js_replace_child", "js_replace_child", Unsafe, [t|Node -> Node -> ParentNode -> IO ()|])
     , ("js_insert_child_ix", "js_insert_child_ix", Unsafe, [t|Int -> Node -> ParentNode -> IO ()|])
     , ("js_remove_child", "js_remove_child", Unsafe, [t|Node -> ParentNode -> IO ()|])
     , ("js_parent_node", "js_parent_node", Unsafe, [t|Node -> IO (Nullable ParentNode)|])
     , ("js_next_sibling", "js_next_sibling", Unsafe, [t|Node -> IO (Nullable Node)|])
     , ("js_set_attribute", "js_set_attribute", Unsafe, [t|JSVal -> JSVal -> JSVal -> Element -> IO ()|])
     , ("js_set_property", "js_set_property", Unsafe, [t|JSVal -> JSVal -> Element -> IO ()|])
     , ("js_remove_property", "js_remove_property", Unsafe, [t|JSVal -> Element -> IO ()|])
     , ("js_remove_attribute", "js_remove_attribute", Unsafe, [t|JSVal -> JSVal -> Element -> IO ()|])
     , ("js_has_attribute", "js_has_attribute", Unsafe, [t|JSVal -> JSVal -> Element -> IO Bool|])
     , ("js_add_event_listener", "js_add_event_listener", Unsafe, [t|JSVal -> EventListener -> EventTarget -> IO ()|])
     , ("js_remove_event_listener", "js_remove_event_listener", Unsafe, [t|JSVal -> EventListener -> EventTarget -> IO ()|])
     , ("js_query_selector", "js_query_selector", Unsafe, [t|JSVal -> ParentNode -> IO (Nullable Element)|])
     , ("js_ready_state", "js_ready_state", Unsafe, [t|HTMLDocument -> IO JSVal|])
     , ("js_property_equals", "js_property_equals", Unsafe, [t|JSVal -> JSVal -> Element -> IO Bool|])
     ]
 )

foreign import javascript unsafe "null" js_null :: JSVal

foreign import javascript unsafe "$1" js_toJSInt :: Int -> JSVal

foreign import javascript unsafe "$1" js_toJSBool :: Bool -> JSVal

foreign import javascript unsafe "$1" js_toJSNum :: Double -> JSVal

-- A listener is a sync export: the browser runs listeners while it
-- dispatches an event, and re-entering Haskell from a synchronous JSFFI
-- import (a component that dispatches an event from 'liftEffect') is only
-- supported for sync exports. See 'continueAsync' for a listener that waits.
foreign import javascript "wrapper sync" js_mk_event_listener :: (JSVal -> IO ()) -> IO JSVal

-- | Run a listener on a thread of its own until it ends or waits, and
-- return to the browser then: what it does before it first waits happens
-- during the dispatch ('preventDefault' counts), and the rest afterwards, as
-- with the JavaScript backend's 'ContinueAsync' callbacks. A sync export
-- itself must not wait for the page's event loop, which cannot run until it
-- returns.
--
-- What the listener leaves to run (its own rest, the workers it started)
-- needs the scheduler to run again after the export has returned, which a
-- sync export does not arrange by itself: a thread waiting for an async
-- import brings the runtime back once the promise settles, and the
-- scheduler then runs whatever is runnable.
continueAsync :: IO () -> IO ()
continueAsync io = do
  t <- forkIO io
  resume <- forkIO (evaluate =<< js_resume_later)
  untilWaiting t
  untilWaiting resume
  where
    untilWaiting t =
      threadStatus t >>= \case
        ThreadRunning -> yield >> untilWaiting t
        _ -> pass

foreign import javascript safe "undefined" js_resume_later :: IO ()

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

  mkEventListener f = liftIO $ EventListener <$> js_mk_event_listener (continueAsync . runBrowserDOM . f . Event)

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
  readStorageItem kind key = liftIO $ fmap foreignToString . nullableToMaybe <$> js_storage_read (storageName kind) (jsStringVal key)
  writeStorageItem kind key text = liftIO $ js_storage_write (storageName kind) (jsStringVal key) (jsStringVal text)
  removeStorageItem kind key = liftIO $ js_storage_remove (storageName kind) (jsStringVal key)

  -- `key(i)` rather than a list, because a list would have to be marshalled
  -- and this is what a store offers: the indices are stable for as long as
  -- nothing is added or removed, which is as much as a store promises anyway.
  storageItemKeys kind = liftIO $ do
    count <- foreignToInt <$> js_storage_length (storageName kind)
    catMaybes <$> for [0 .. count - 1] (\ix -> fmap foreignToString . nullableToMaybe <$> js_storage_key (storageName kind) ix)

-- | Which store the browser is being asked for, as the string the jsbits switch on.
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
