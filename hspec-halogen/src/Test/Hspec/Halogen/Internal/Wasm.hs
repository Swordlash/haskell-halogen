{-# LANGUAGE TemplateHaskell #-}

-- | The page, as the WebAssembly backend reaches it. Compiled only there;
-- "Test.Hspec.Halogen.Internal.Page" re-exports it, or stands in for it
-- elsewhere. Its JavaScript comes from the same jsbits as the JavaScript backend.
module Test.Hspec.Halogen.Internal.Wasm
  ( inBrowser
  , runnerArgs
  , reportDone
  , removeLeftovers
  , createContainer
  , createContainerIn
  , removeElement
  , querySelector
  , querySelectorAll
  , act
  , pressKey
  , focusElement
  , blurElement
  , settleTasks
  , textContentOf
  , propertyOf
  , attributeOf
  , outerHTMLOf
  , checkVisibility
  , sameElement
  , isConnected
  )
where

import GHC.Wasm.Prim (JSString (..), JSVal, fromJSString, toJSString)
import Halogen.JSBits (Safety (..), wasmJS)
import Protolude
import Web.DOM.Internal.Types (Element (..))

$( wasmJS
     ["jsbits/page.js"]
     [ ("js_has_document", "halogen_test_has_document", Unsafe, [t|IO Bool|])
     , ("js_test_args", "halogen_test_test_args", Unsafe, [t|IO JSVal|])
     , ("js_remove_leftovers", "halogen_test_remove_leftovers", Unsafe, [t|IO ()|])
     , ("js_test_done", "halogen_test_test_done", Unsafe, [t|Int -> IO ()|])
     , ("js_create_container", "halogen_test_create_container", Unsafe, [t|IO JSVal|])
     , ("js_create_container_in", "halogen_test_create_container_in", Unsafe, [t|JSVal -> IO JSVal|])
     , ("js_remove", "halogen_test_remove", Unsafe, [t|JSVal -> IO ()|])
     , ("js_query_selector", "halogen_test_query_selector", Unsafe, [t|Element -> JSVal -> IO JSVal|])
     , ("js_query_selector_all", "halogen_test_query_selector_all", Unsafe, [t|Element -> JSVal -> IO JSVal|])
     , ("js_act", "halogen_test_act", Safe, [t|JSVal -> Element -> JSVal -> IO ()|])
     , ("js_press", "halogen_test_press", Safe, [t|JSVal -> IO ()|])
     , ("js_settle", "halogen_test_settle", Safe, [t|IO ()|])
     , ("js_focus", "halogen_test_focus", Unsafe, [t|Element -> IO ()|])
     , ("js_blur", "halogen_test_blur", Unsafe, [t|Element -> IO ()|])
     , ("js_text_content", "halogen_test_text_content", Unsafe, [t|Element -> IO JSVal|])
     , ("js_get_property", "halogen_test_get_property", Unsafe, [t|Element -> JSVal -> IO JSVal|])
     , ("js_get_attribute", "halogen_test_get_attribute", Unsafe, [t|Element -> JSVal -> IO JSVal|])
     , ("js_outer_html", "halogen_test_outer_html", Unsafe, [t|Element -> IO JSVal|])
     , ("js_is_visible", "halogen_test_is_visible", Unsafe, [t|Element -> IO Bool|])
     , ("js_same_element", "halogen_test_same_element", Unsafe, [t|Element -> Element -> Bool|])
     , ("js_is_connected", "halogen_test_is_connected", Unsafe, [t|Element -> IO Bool|])
     , ("js_is_null", "halogen_test_is_null", Unsafe, [t|JSVal -> Bool|])
     , ("js_length", "halogen_test_length", Unsafe, [t|JSVal -> Int|])
     , ("js_index", "halogen_test_index", Unsafe, [t|JSVal -> Int -> JSVal|])
     ]
 )

-- | Whether there is a page to run in.
inBrowser :: IO Bool

-- | hspec's arguments, when the test runner passed some. Browser GHCi passes
-- none: there @:main@ sets them.
runnerArgs :: IO (Maybe [Text])

-- | Tell the runner how many tests failed.
reportDone :: Int -> IO ()

-- | Remove the containers of a run that was interrupted before cleaning up.
removeLeftovers :: IO ()

-- | A fresh container at the end of the body.
createContainer :: IO Element

-- | A fresh container at the end of another.
createContainerIn :: Element -> IO Element
removeElement :: Element -> IO ()
querySelector :: Element -> Text -> IO (Maybe Element)
querySelectorAll :: Element -> Text -> IO [Element]

-- | Have the runner click, type into or clear an element, and wait until it
-- has.
act :: Text -> Element -> Text -> IO ()

-- | Have the runner press a key on the focused element, and wait until it has.
pressKey :: Text -> IO ()
focusElement :: Element -> IO ()
blurElement :: Element -> IO ()

-- | Wait until every task the page has queued at normal priority has run.
settleTasks :: IO ()
textContentOf :: Element -> IO Text

-- | A property rendered with JavaScript's @String()@.
propertyOf :: Element -> Text -> IO Text
attributeOf :: Element -> Text -> IO (Maybe Text)
outerHTMLOf :: Element -> IO Text
checkVisibility :: Element -> IO Bool

-- | Whether two handles are the same node (JavaScript's @===@).
sameElement :: Element -> Element -> Bool

-- | Whether the element is still in the document.
isConnected :: Element -> IO Bool

inBrowser = js_has_document

runnerArgs = do
  args <- js_test_args
  pure $ if js_is_null args then Nothing else Just (map fromJS (fromJSVals args))

reportDone = js_test_done

removeLeftovers = js_remove_leftovers

createContainer = Element <$> js_create_container

createContainerIn (Element parent) = Element <$> js_create_container_in parent

removeElement (Element element) = js_remove element

querySelector scope selector = do
  found <- js_query_selector scope (jsText selector)
  pure $ if js_is_null found then Nothing else Just (Element found)

querySelectorAll scope selector = map Element . fromJSVals <$> js_query_selector_all scope (jsText selector)

act action element argument = awaitJS $ js_act (jsText action) element (jsText argument)

pressKey key = awaitJS $ js_press (jsText key)

focusElement = js_focus

blurElement = js_blur

settleTasks = awaitJS js_settle

textContentOf element = fromJS <$> js_text_content element

propertyOf element name = fromJS <$> js_get_property element (jsText name)

attributeOf element name = do
  value <- js_get_attribute element (jsText name)
  pure $ if js_is_null value then Nothing else Just (fromJS value)

outerHTMLOf element = fromJS <$> js_outer_html element

checkVisibility = js_is_visible

sameElement = js_same_element

isConnected = js_is_connected

-- | Wait for an async ("safe") import to finish. Its result comes back as a
-- thunk, and the thread only blocks on the promise when that is forced, so a
-- @()@ nobody looks at would let the test run on while the page still acts.
awaitJS :: IO () -> IO ()
awaitJS action = action >>= evaluate

jsText :: Text -> JSVal
jsText text = case toJSString (toS text) of JSString value -> value

fromJS :: JSVal -> Text
fromJS = toS . fromJSString . JSString

fromJSVals :: JSVal -> [JSVal]
fromJSVals array = map (js_index array) [0 .. js_length array - 1]

-- Outside the runner (a page opened by hand) there is nobody to tell.

-- Without the runner's bridge, fall back to synthetic events: close enough
-- for most components, though focus and key events differ from a user's.
