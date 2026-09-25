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

foreign import javascript unsafe "(() => typeof document !== 'undefined')"
  js_has_document :: IO Bool

foreign import javascript unsafe "(() => globalThis.__halogenTestArgs ?? null)"
  js_test_args :: IO JSVal

-- Outside the runner (a page opened by hand) there is nobody to tell.
foreign import javascript unsafe "((failures) => { globalThis.__halogenTest?.done?.(failures); })"
  js_test_done :: Int -> IO ()

foreign import javascript unsafe "(() => { document.querySelectorAll('.halogen-test-root').forEach((root) => root.remove()); })"
  js_remove_leftovers :: IO ()

foreign import javascript unsafe "(() => document.body.appendChild(Object.assign(document.createElement('div'), {className: 'halogen-test-root'})))"
  js_create_container :: IO JSVal

foreign import javascript unsafe "((parent) => parent.appendChild(document.createElement('div')))"
  js_create_container_in :: JSVal -> IO JSVal

foreign import javascript unsafe "((element) => { element.remove(); })"
  js_remove :: JSVal -> IO ()

foreign import javascript unsafe "((scope, selector) => scope.querySelector(selector))"
  js_query_selector :: Element -> JSVal -> IO JSVal

foreign import javascript unsafe "((scope, selector) => Array.from(scope.querySelectorAll(selector)))"
  js_query_selector_all :: Element -> JSVal -> IO JSVal

-- Without the runner's bridge, fall back to synthetic events: close enough
-- for most components, though focus and key events differ from a user's.
foreign import javascript interruptible
  "((action, element, text, $c) => {\
  \  const target = 'halogen-test-' + (globalThis.__halogenTestTargets = (globalThis.__halogenTestTargets ?? 0) + 1);\
  \  element.setAttribute('data-halogen-test-target', target);\
  \  const finish = (error) => { element.removeAttribute('data-halogen-test-target'); $c(error); };\
  \  const bridge = globalThis.__halogenTest;\
  \  if (bridge) {\
  \    bridge.act(action, '[data-halogen-test-target=\"' + target + '\"]', text)\
  \      .then(() => finish(null), (error) => finish(String(error?.message ?? error)));\
  \    return;\
  \  }\
  \  if (action === 'click') { element.click(); }\
  \  else {\
  \    element.focus();\
  \    element.value = action === 'type' ? element.value + text : '';\
  \    element.dispatchEvent(new Event('input', {bubbles: true}));\
  \  }\
  \  finish(null);\
  \})"
  js_act :: JSVal -> Element -> JSVal -> IO JSVal

foreign import javascript interruptible
  "((key, $c) => {\
  \  const bridge = globalThis.__halogenTest;\
  \  if (bridge) {\
  \    bridge.press(key).then(() => $c(null), (error) => $c(String(error?.message ?? error)));\
  \    return;\
  \  }\
  \  const target = document.activeElement ?? document.body;\
  \  for (const type of ['keydown', 'keyup']) target.dispatchEvent(new KeyboardEvent(type, {key, bubbles: true}));\
  \  $c(null);\
  \})"
  js_press :: JSVal -> IO JSVal

foreign import javascript interruptible
  "(($c) => {\
  \  const resume = () => $c();\
  \  if (globalThis.scheduler) scheduler.postTask(resume, {priority: 'background'});\
  \  else setTimeout(resume, 0);\
  \})"
  js_settle :: IO ()

foreign import javascript unsafe "((element) => { element.focus(); })"
  js_focus :: Element -> IO ()

foreign import javascript unsafe "((element) => { element.blur(); })"
  js_blur :: Element -> IO ()

foreign import javascript unsafe "((element) => element.textContent ?? '')"
  js_text_content :: Element -> IO JSVal

foreign import javascript unsafe "((element, name) => String(element[name]))"
  js_get_property :: Element -> JSVal -> IO JSVal

foreign import javascript unsafe "((element, name) => element.getAttribute(name))"
  js_get_attribute :: Element -> JSVal -> IO JSVal

foreign import javascript unsafe "((element) => element.outerHTML)"
  js_outer_html :: Element -> IO JSVal

foreign import javascript unsafe "((element) => element.checkVisibility())"
  js_is_visible :: Element -> IO Bool

foreign import javascript unsafe "((a, b) => a === b ? 1 : 0)"
  js_same_element :: Element -> Element -> IO Int

foreign import javascript unsafe "((element) => element.isConnected)"
  js_is_connected :: Element -> IO Bool
