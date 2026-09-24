-- | The page, as the WebAssembly backend reaches it. Compiled only there;
-- "Test.Hspec.Halogen.Internal.Page" re-exports it, or stands in for it
-- elsewhere. (It is a module of its own because the JavaScript below is kept
-- in multi-line strings, whose line-end backslashes CPP would eat.)
module Test.Hspec.Halogen.Internal.Wasm
  ( inBrowser
  , runnerArgs
  , reportDone
  , removeLeftovers
  , createContainer
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
  )
where

import GHC.Wasm.Prim (JSString (..), JSVal, fromJSString, toJSString)
import Protolude
import Web.DOM.Internal.Types (Element (..))

-- | Whether there is a page to run in.
inBrowser :: Bool

-- | hspec's arguments, when the test runner passed some. Browser GHCi passes
-- none: there @:main@ sets them.
runnerArgs :: IO (Maybe [Text])

-- | Tell the runner how many tests failed.
reportDone :: Int -> IO ()

-- | Remove the containers of a run that was interrupted before cleaning up.
removeLeftovers :: IO ()

-- | A fresh container at the end of the body.
createContainer :: IO Element
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

inBrowser = True

runnerArgs = do
  args <- js_test_args
  pure $ if js_is_null args then Nothing else Just (map fromJS (fromJSVals args))

reportDone = js_test_done

removeLeftovers = js_remove_leftovers

createContainer = Element <$> js_create_container

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

foreign import javascript unsafe "globalThis.__halogenTestArgs ?? null"
  js_test_args :: IO JSVal

foreign import javascript unsafe "document.querySelectorAll('.halogen-test-root').forEach((root) => root.remove())"
  js_remove_leftovers :: IO ()

-- Outside the runner (a page opened by hand) there is nobody to tell.
foreign import javascript unsafe "globalThis.__halogenTest?.done?.($1)"
  js_test_done :: Int -> IO ()

foreign import javascript unsafe "document.body.appendChild(Object.assign(document.createElement('div'), {className: 'halogen-test-root'}))"
  js_create_container :: IO JSVal

foreign import javascript unsafe "$1.remove()"
  js_remove :: JSVal -> IO ()

foreign import javascript unsafe "$1.querySelector($2)"
  js_query_selector :: Element -> JSVal -> IO JSVal

foreign import javascript unsafe "Array.from($1.querySelectorAll($2))"
  js_query_selector_all :: Element -> JSVal -> IO JSVal

-- Without the runner's bridge, fall back to synthetic events: close enough
-- for most components, though focus and key events differ from a user's.
foreign import javascript safe
  "const target = 'halogen-test-' + (globalThis.__halogenTestTargets = (globalThis.__halogenTestTargets ?? 0) + 1);\
  \$2.setAttribute('data-halogen-test-target', target);\
  \try {\
  \  const bridge = globalThis.__halogenTest;\
  \  if (bridge) { await bridge.act($1, '[data-halogen-test-target=\"' + target + '\"]', $3); }\
  \  else if ($1 === 'click') { $2.click(); }\
  \  else {\
  \    $2.focus();\
  \    $2.value = $1 === 'type' ? $2.value + $3 : '';\
  \    $2.dispatchEvent(new Event('input', {bubbles: true}));\
  \  }\
  \} finally { $2.removeAttribute('data-halogen-test-target'); }"
  js_act :: JSVal -> Element -> JSVal -> IO ()

foreign import javascript safe
  "const bridge = globalThis.__halogenTest;\
  \if (bridge) { await bridge.press($1); }\
  \else {\
  \  const target = document.activeElement ?? document.body;\
  \  for (const type of ['keydown', 'keyup']) target.dispatchEvent(new KeyboardEvent(type, {key: $1, bubbles: true}));\
  \}"
  js_press :: JSVal -> IO ()

foreign import javascript safe
  "await new Promise((resolve) => globalThis.scheduler\
  \  ? scheduler.postTask(resolve, {priority: 'background'})\
  \  : setTimeout(resolve, 0));"
  js_settle :: IO ()

foreign import javascript unsafe "$1.focus()"
  js_focus :: Element -> IO ()

foreign import javascript unsafe "$1.blur()"
  js_blur :: Element -> IO ()

foreign import javascript unsafe "$1.textContent ?? ''"
  js_text_content :: Element -> IO JSVal

foreign import javascript unsafe "String($1[$2])"
  js_get_property :: Element -> JSVal -> IO JSVal

foreign import javascript unsafe "$1.getAttribute($2)"
  js_get_attribute :: Element -> JSVal -> IO JSVal

foreign import javascript unsafe "$1.outerHTML"
  js_outer_html :: Element -> IO JSVal

foreign import javascript unsafe "$1.checkVisibility()"
  js_is_visible :: Element -> IO Bool

foreign import javascript unsafe "$1 == null"
  js_is_null :: JSVal -> Bool

foreign import javascript unsafe "$1.length"
  js_length :: JSVal -> Int

foreign import javascript unsafe "$1[$2]"
  js_index :: JSVal -> Int -> JSVal
