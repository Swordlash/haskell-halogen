{-# LANGUAGE TemplateHaskell #-}

-- | Everything "Test.Hspec.Halogen" asks of the page it runs in, through the
-- functions in @jsbits/page.js@, imported once for both browser backends.
-- Those that wait for the page (the runner's actions, a settling) return a
-- promise, which is awaited here, and what it is rejected with is thrown as
-- a 'JSError'.
--
-- Natively the package still builds, so that the suites using it type-check
-- and the Haskell language server can load them, but nothing here can run:
-- 'inBrowser' is 'False' and the rest does nothing.
module Test.Hspec.Halogen.Internal.Page
  ( inBrowser
  , runnerArgs
  , runnerShard
  , reportDone
  , removeLeftovers
  , createContainer
  , createRootIn
  , createContainerIn
  , setInnerHTML
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

import Halogen.JSBits (JSText, JSVal, Safety (..), browserJS, fromJSText, jsValText, toJSText)
import Halogen.JSBits qualified as JSBits
import Halogen.JSBits.Promise (Promise (..), await)
import Protolude
import Web.DOM.Internal.Types (Element (..))

$( browserJS
     ["jsbits/page.js"]
     [ ("js_has_document", "halogen_test_has_document", Unsafe, [t|IO Bool|])
     , ("js_shard", "halogen_test_shard", Unsafe, [t|IO JSText|])
     , ("js_test_args", "halogen_test_test_args", Unsafe, [t|IO JSVal|])
     , ("js_remove_leftovers", "halogen_test_remove_leftovers", Unsafe, [t|IO ()|])
     , ("js_test_done", "halogen_test_test_done", Unsafe, [t|Int -> IO ()|])
     , ("js_create_container", "halogen_test_create_container", Unsafe, [t|IO JSVal|])
     , ("js_create_root_in", "halogen_test_create_root_in", Unsafe, [t|JSVal -> IO JSVal|])
     , ("js_create_container_in", "halogen_test_create_container_in", Unsafe, [t|JSVal -> IO JSVal|])
     , ("js_set_inner_html", "halogen_test_set_inner_html", Unsafe, [t|Element -> JSText -> IO ()|])
     , ("js_remove", "halogen_test_remove", Unsafe, [t|JSVal -> IO ()|])
     , ("js_query_selector", "halogen_test_query_selector", Unsafe, [t|Element -> JSText -> IO JSVal|])
     , ("js_query_selector_all", "halogen_test_query_selector_all", Unsafe, [t|Element -> JSText -> IO JSVal|])
     , ("js_act", "halogen_test_act", Unsafe, [t|JSText -> Element -> JSText -> IO Promise|])
     , ("js_drag", "halogen_test_drag", Unsafe, [t|Element -> Element -> IO Promise|])
     , ("js_press", "halogen_test_press", Unsafe, [t|JSText -> IO Promise|])
     , ("js_settle", "halogen_test_settle", Unsafe, [t|IO Promise|])
     , ("js_focus", "halogen_test_focus", Unsafe, [t|Element -> IO ()|])
     , ("js_blur", "halogen_test_blur", Unsafe, [t|Element -> IO ()|])
     , ("js_text_content", "halogen_test_text_content", Unsafe, [t|Element -> IO JSText|])
     , ("js_get_property", "halogen_test_get_property", Unsafe, [t|Element -> JSText -> IO JSText|])
     , ("js_get_attribute", "halogen_test_get_attribute", Unsafe, [t|Element -> JSText -> IO JSVal|])
     , ("js_outer_html", "halogen_test_outer_html", Unsafe, [t|Element -> IO JSText|])
     , ("js_is_visible", "halogen_test_is_visible", Unsafe, [t|Element -> IO Bool|])
     , ("js_is_connected", "halogen_test_is_connected", Unsafe, [t|Element -> IO Bool|])
     , ("js_same_element", "halogen_test_same_element", Unsafe, [t|Element -> Element -> Int|])
     , ("js_is_null", "halogen_test_is_null", Unsafe, [t|JSVal -> Int|])
     , ("js_length", "halogen_test_length", Unsafe, [t|JSVal -> Int|])
     , ("js_index", "halogen_test_index", Unsafe, [t|JSVal -> Int -> JSVal|])
     ]
 )

-- | Whether there is a page to run in. A test binary is a Node script too,
-- and run as one it has no document.
inBrowser :: IO Bool
inBrowser = js_has_document

-- | hspec's arguments, when the test runner passed some. Browser GHCi passes
-- none: there @:main@ sets them.
runnerArgs :: IO (Maybe [Text])
runnerArgs = do
  args <- js_test_args
  pure $ if absent args then Nothing else Just (map jsValText (elements args))

runnerShard :: IO Text
runnerShard = fromJSText <$> js_shard

-- | Tell the runner how many tests failed. Outside the runner (a page opened
-- by hand) there is nobody to tell.
reportDone :: Int -> IO ()
reportDone = js_test_done

-- | Remove the containers of a run that was interrupted before cleaning up.
removeLeftovers :: IO ()
removeLeftovers = js_remove_leftovers

-- | A fresh container for a page at the end of the body.
createContainer :: IO Element
createContainer = Element <$> js_create_container

-- | A fresh container for a page at the end of another element, marked as
-- the body's are, for 'removeLeftovers'.
createRootIn :: Element -> IO Element
createRootIn (Element parent) = Element <$> js_create_root_in parent

-- | A fresh container at the end of another.
createContainerIn :: Element -> IO Element
createContainerIn (Element parent) = Element <$> js_create_container_in parent

-- | Replace what an element holds with this HTML, parsed as the browser
-- parses a page.
setInnerHTML :: Element -> Text -> IO ()
setInnerHTML element html = js_set_inner_html element (toJSText html)

removeElement :: Element -> IO ()
removeElement (Element element) = js_remove element

querySelector :: Element -> Text -> IO (Maybe Element)
querySelector scope selector = do
  found <- js_query_selector scope (toJSText selector)
  pure $ if absent found then Nothing else Just (Element found)

querySelectorAll :: Element -> Text -> IO [Element]
querySelectorAll scope selector = map Element . elements <$> js_query_selector_all scope (toJSText selector)

-- | Have the runner click, type into or clear an element, and wait until it
-- has. Without the runner's bridge the page does it itself, with synthetic
-- events (page.js's in-page mode): what the page's listeners see is much the
-- same, but the events are untrusted, and only the browser's own reactions
-- page.js reproduces happen.
act :: Text -> Element -> Text -> IO ()
act action element argument = awaitJS $ js_act (toJSText action) element (toJSText argument)

-- | Have the runner drag one element onto another, and wait until it has
-- (without the runner, with synthetic pointer and drag events).
drag :: Element -> Element -> IO ()
drag source target = awaitJS $ js_drag source target

-- | Have the runner press a key on the focused element, and wait until it has
-- (without the runner, with synthetic keyboard events).
pressKey :: Text -> IO ()
pressKey key = awaitJS $ js_press (toJSText key)

focusElement :: Element -> IO ()
focusElement = js_focus

blurElement :: Element -> IO ()
blurElement = js_blur

-- | Wait until every task the page has queued at normal priority has run.
settleTasks :: IO ()
settleTasks = awaitJS js_settle

textContentOf :: Element -> IO Text
textContentOf element = fromJSText <$> js_text_content element

-- | A property rendered with JavaScript's @String()@.
propertyOf :: Element -> Text -> IO Text
propertyOf element name = fromJSText <$> js_get_property element (toJSText name)

attributeOf :: Element -> Text -> IO (Maybe Text)
attributeOf element name = do
  value <- js_get_attribute element (toJSText name)
  pure $ if absent value then Nothing else Just (jsValText value)

outerHTMLOf :: Element -> IO Text
outerHTMLOf element = fromJSText <$> js_outer_html element

checkVisibility :: Element -> IO Bool
checkVisibility = js_is_visible

-- | Whether two handles are the same node (JavaScript's @===@).
sameElement :: Element -> Element -> Bool
sameElement a b = js_same_element a b == 1

-- | Whether the element is still in the document.
isConnected :: Element -> IO Bool
isConnected = js_is_connected

-- | Start something in the page and wait until it is done, throwing what it
-- failed with (a Playwright action the runner could not perform, say) as a
-- 'Halogen.JSBits.Promise.JSError', which fails the test with its name and
-- message. Natively nothing was started, and there is nothing to wait for.
awaitJS :: IO Promise -> IO ()
awaitJS start
  | not JSBits.inBrowser = pass
  | otherwise = start >>= await >>= either throwIO (const pass)

-- | @null@ or @undefined@: natively, always.
absent :: JSVal -> Bool
absent value = js_is_null value /= 0

elements :: JSVal -> [JSVal]
elements array = map (js_index array) [0 .. js_length array - 1]
