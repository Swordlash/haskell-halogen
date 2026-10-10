-- | The page, as GHC's JavaScript backend reaches it. Compiled only there;
-- "Test.Hspec.Halogen.Internal.Page" re-exports it. Its imports are in
-- "Test.Hspec.Halogen.Internal.Bindings", shared with the WebAssembly
-- backend; the safe ones are @interruptible@ there, awaiting the runner and
-- throwing what it rejects with.
module Test.Hspec.Halogen.Internal.JS
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
  , drag
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

import GHC.JS.Prim (JSVal, fromJSArray, fromJSString, isNull, isUndefined, toJSString)
import Protolude
import Test.Hspec.Halogen.Internal.Bindings
import System.IO.Unsafe (unsafePerformIO)
import Web.DOM.Internal.Types (Element (..))

-- | Whether there is a page to run in. A test binary is a Node script too,
-- and run as one it has no document.
inBrowser :: IO Bool
inBrowser = js_has_document

-- | hspec's arguments, when the test runner passed some.
runnerArgs :: IO (Maybe [Text])
runnerArgs = do
  args <- js_test_args
  if absent args then pure Nothing else Just . map fromJS <$> fromJSArray args

reportDone :: Int -> IO ()
reportDone = js_test_done

removeLeftovers :: IO ()
removeLeftovers = js_remove_leftovers

createContainer :: IO Element
createContainer = Element <$> js_create_container

createContainerIn :: Element -> IO Element
createContainerIn (Element parent) = Element <$> js_create_container_in parent

removeElement :: Element -> IO ()
removeElement (Element element) = js_remove element

querySelector :: Element -> Text -> IO (Maybe Element)
querySelector scope selector = do
  found <- js_query_selector scope (jsText selector)
  pure $ if absent found then Nothing else Just (Element found)

querySelectorAll :: Element -> Text -> IO [Element]
querySelectorAll scope selector = map Element <$> (fromJSArray =<< js_query_selector_all scope (jsText selector))

act :: Text -> Element -> Text -> IO ()
act action element argument = js_act (jsText action) element (jsText argument)

drag :: Element -> Element -> IO ()
drag = js_drag

pressKey :: Text -> IO ()
pressKey key = js_press (jsText key)

focusElement :: Element -> IO ()
focusElement = js_focus

blurElement :: Element -> IO ()
blurElement = js_blur

settleTasks :: IO ()
settleTasks = js_settle >>= evaluate

textContentOf :: Element -> IO Text
textContentOf element = fromJS <$> js_text_content element

propertyOf :: Element -> Text -> IO Text
propertyOf element name = fromJS <$> js_get_property element (jsText name)

attributeOf :: Element -> Text -> IO (Maybe Text)
attributeOf element name = do
  value <- js_get_attribute element (jsText name)
  pure $ if absent value then Nothing else Just (fromJS value)

outerHTMLOf :: Element -> IO Text
outerHTMLOf element = fromJS <$> js_outer_html element

checkVisibility :: Element -> IO Bool
checkVisibility = js_is_visible

-- The same node or not never changes, so reading it is pure.
sameElement :: Element -> Element -> Bool
sameElement a b = unsafePerformIO (js_same_element a b) == 1

isConnected :: Element -> IO Bool
isConnected = js_is_connected

absent :: JSVal -> Bool
absent value = isNull value || isUndefined value

jsText :: Text -> JSVal
jsText = toJSString . toS

fromJS :: JSVal -> Text
fromJS = toS . fromJSString

foreign import javascript unsafe "((a, b) => halogen_test_same_element(a, b) ? 1 : 0)"
  js_same_element :: Element -> Element -> IO Int
