{-# LANGUAGE CPP #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The page's JavaScript functions, imported once for both browser
-- backends: "Test.Hspec.Halogen.Internal.Wasm" and
-- "Test.Hspec.Halogen.Internal.JS" build on them. The safe ones await the
-- runner and throw what it rejects with.
module Test.Hspec.Halogen.Internal.Bindings
  ( js_has_document
  , js_test_args
  , js_remove_leftovers
  , js_test_done
  , js_create_container
  , js_create_container_in
  , js_remove
  , js_query_selector
  , js_query_selector_all
  , js_act
  , js_drag
  , js_press
  , js_settle
  , js_focus
  , js_blur
  , js_text_content
  , js_get_property
  , js_get_attribute
  , js_outer_html
  , js_is_visible
  , js_is_connected
#if defined(wasm32_HOST_ARCH)
  , js_same_element
  , js_is_null
  , js_length
  , js_index
#endif
  )
where

#if defined(javascript_HOST_ARCH)
import GHC.JS.Prim (JSVal)
#else
import GHC.Wasm.Prim (JSVal)
#endif
import Halogen.JSBits (Safety (..), browserJS)
import Protolude
import Web.DOM.Internal.Types (Element (..))

$( browserJS
    ["jsbits/page.js"]
    ( [ ("js_has_document", "halogen_test_has_document", Unsafe, [t|IO Bool|])
    , ("js_test_args", "halogen_test_test_args", Unsafe, [t|IO JSVal|])
    , ("js_remove_leftovers", "halogen_test_remove_leftovers", Unsafe, [t|IO ()|])
    , ("js_test_done", "halogen_test_test_done", Unsafe, [t|Int -> IO ()|])
    , ("js_create_container", "halogen_test_create_container", Unsafe, [t|IO JSVal|])
    , ("js_create_container_in", "halogen_test_create_container_in", Unsafe, [t|JSVal -> IO JSVal|])
    , ("js_remove", "halogen_test_remove", Unsafe, [t|JSVal -> IO ()|])
    , ("js_query_selector", "halogen_test_query_selector", Unsafe, [t|Element -> JSVal -> IO JSVal|])
    , ("js_query_selector_all", "halogen_test_query_selector_all", Unsafe, [t|Element -> JSVal -> IO JSVal|])
    , ("js_act", "halogen_test_act", Safe, [t|JSVal -> Element -> JSVal -> IO ()|])
    , ("js_drag", "halogen_test_drag", Safe, [t|Element -> Element -> IO ()|])
    , ("js_press", "halogen_test_press", Safe, [t|JSVal -> IO ()|])
    , ("js_settle", "halogen_test_settle", Safe, [t|IO ()|])
    , ("js_focus", "halogen_test_focus", Unsafe, [t|Element -> IO ()|])
    , ("js_blur", "halogen_test_blur", Unsafe, [t|Element -> IO ()|])
    , ("js_text_content", "halogen_test_text_content", Unsafe, [t|Element -> IO JSVal|])
    , ("js_get_property", "halogen_test_get_property", Unsafe, [t|Element -> JSVal -> IO JSVal|])
    , ("js_get_attribute", "halogen_test_get_attribute", Unsafe, [t|Element -> JSVal -> IO JSVal|])
    , ("js_outer_html", "halogen_test_outer_html", Unsafe, [t|Element -> IO JSVal|])
    , ("js_is_visible", "halogen_test_is_visible", Unsafe, [t|Element -> IO Bool|])
    , ("js_is_connected", "halogen_test_is_connected", Unsafe, [t|Element -> IO Bool|])
    ]
#if defined(wasm32_HOST_ARCH)
        -- The JavaScript backend has its own ways to these.
        <> [ ("js_same_element", "halogen_test_same_element", Unsafe, [t|Element -> Element -> Bool|])
    , ("js_is_null", "halogen_test_is_null", Unsafe, [t|JSVal -> Bool|])
    , ("js_length", "halogen_test_length", Unsafe, [t|JSVal -> Int|])
    , ("js_index", "halogen_test_index", Unsafe, [t|JSVal -> Int -> JSVal|])
    ]
#endif
    )
 )
