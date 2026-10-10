{-# LANGUAGE InterruptibleFFI #-}

-- | The page, as GHC's JavaScript backend reaches it. Compiled only there;
-- "Test.Hspec.Halogen.Internal.Page" re-exports it. (It is a module of its
-- own for the same reason as "Test.Hspec.Halogen.Internal.Wasm": its
-- JavaScript is in multi-line strings, whose line-end backslashes CPP would
-- eat.)
--
-- What the WebAssembly backend does with an async import, this one does with
-- an @interruptible@ one: the JavaScript gets a continuation, @$c@, as its last
-- argument, and the Haskell thread waits until it is called. A continuation
-- cannot throw, so an import that can fail hands it an error message, or
-- @null@ when all went well, and 'awaitOk' throws the message.
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
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit.Lang (assertFailure)
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
act action element argument = awaitOk $ js_act (jsText action) element (jsText argument)

drag :: Element -> Element -> IO ()
drag source target = awaitOk $ js_drag source target

pressKey :: Text -> IO ()
pressKey key = awaitOk $ js_press (jsText key)

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

-- | Run an import that reports failure as a message, and throw it.
awaitOk :: IO JSVal -> IO ()
awaitOk action = do
  outcome <- action
  unless (absent outcome) $ assertFailure (fromJSString outcome)

absent :: JSVal -> Bool
absent value = isNull value || isUndefined value

jsText :: Text -> JSVal
jsText = toJSString . toS

fromJS :: JSVal -> Text
fromJS = toS . fromJSString

foreign import javascript unsafe "halogen_test_has_document"
  js_has_document :: IO Bool

foreign import javascript unsafe "halogen_test_test_args"
  js_test_args :: IO JSVal

-- Outside the runner (a page opened by hand) there is nobody to tell.
foreign import javascript unsafe "halogen_test_test_done"
  js_test_done :: Int -> IO ()

foreign import javascript unsafe "halogen_test_remove_leftovers"
  js_remove_leftovers :: IO ()

foreign import javascript unsafe "halogen_test_create_container"
  js_create_container :: IO JSVal

foreign import javascript unsafe "halogen_test_create_container_in"
  js_create_container_in :: JSVal -> IO JSVal

foreign import javascript unsafe "halogen_test_remove"
  js_remove :: JSVal -> IO ()

foreign import javascript unsafe "halogen_test_query_selector"
  js_query_selector :: Element -> JSVal -> IO JSVal

foreign import javascript unsafe "halogen_test_query_selector_all"
  js_query_selector_all :: Element -> JSVal -> IO JSVal

-- Without the runner's bridge, fall back to synthetic events: close enough
-- for most components, though focus and key events differ from a user's.
foreign import javascript interruptible "((a1, a2, a3, done) => { halogen_test_act(a1, a2, a3).then(() => done(null), error => done(String(error?.message ?? error))); })"
  js_act :: JSVal -> Element -> JSVal -> IO JSVal

foreign import javascript interruptible "((a1, a2, done) => { halogen_test_drag(a1, a2).then(() => done(null), error => done(String(error?.message ?? error))); })"
  js_drag :: Element -> Element -> IO JSVal

foreign import javascript interruptible "((a1, done) => { halogen_test_press(a1).then(() => done(null), error => done(String(error?.message ?? error))); })"
  js_press :: JSVal -> IO JSVal

foreign import javascript interruptible "((done) => { halogen_test_settle().then(() => done(), error => done(String(error?.message ?? error))); })"
  js_settle :: IO ()

foreign import javascript unsafe "halogen_test_focus"
  js_focus :: Element -> IO ()

foreign import javascript unsafe "halogen_test_blur"
  js_blur :: Element -> IO ()

foreign import javascript unsafe "halogen_test_text_content"
  js_text_content :: Element -> IO JSVal

foreign import javascript unsafe "halogen_test_get_property"
  js_get_property :: Element -> JSVal -> IO JSVal

foreign import javascript unsafe "halogen_test_get_attribute"
  js_get_attribute :: Element -> JSVal -> IO JSVal

foreign import javascript unsafe "halogen_test_outer_html"
  js_outer_html :: Element -> IO JSVal

foreign import javascript unsafe "halogen_test_is_visible"
  js_is_visible :: Element -> IO Bool

foreign import javascript unsafe "((a, b) => halogen_test_same_element(a, b) ? 1 : 0)"
  js_same_element :: Element -> Element -> IO Int

foreign import javascript unsafe "halogen_test_is_connected"
  js_is_connected :: Element -> IO Bool
