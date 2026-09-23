module Test.GHCJS (spec) where

import Prelude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

import Data.Foreign
import Data.Maybe (isNothing)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Halogen.VDom.DOM.Monad qualified as DOM
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)
import Web.DOM.Internal.Types (Node (..), EventListener (..))
import Web.DOM.ParentNode (ParentNode (..))
import Web.Event.Event qualified as Event

data DOMFixture

#if defined(javascript_HOST_ARCH)
foreign import javascript unsafe "(() => { return true; })" js_true :: Foreign Bool
foreign import javascript unsafe "(() => { return false; })" js_false :: Foreign Bool
foreign import javascript unsafe "(() => { return 'hello'; })" js_string :: Foreign String
foreign import javascript unsafe "(() => { return 42; })" js_int :: Foreign Int
foreign import javascript unsafe "(() => { return { answer: 42 }; })" js_object :: Foreign ()
foreign import javascript unsafe "(() => { return null; })" js_nullish :: Nullable ()
foreign import javascript unsafe "(() => { const child = {}; const sibling = { previousSibling: child }; return { child, sibling, parent: { lastChild: child, appendChild() { throw new Error('unexpected appendChild'); }, insertBefore() { throw new Error('unexpected insertBefore'); } } }; })" js_dom_fixture :: Foreign DOMFixture
foreign import javascript unsafe "((x) => x.child)" js_fixture_child_foreign :: Foreign DOMFixture -> Foreign Node
foreign import javascript unsafe "((x) => x.sibling)" js_fixture_sibling_foreign :: Foreign DOMFixture -> Foreign Node
foreign import javascript unsafe "((x) => x.parent)" js_fixture_parent_foreign :: Foreign DOMFixture -> Foreign ParentNode
foreign import javascript unsafe "((cb) => { const t = new EventTarget(); t.addEventListener('submit', cb); return !t.dispatchEvent(new Event('submit', {cancelable:true})); })" js_cancelled :: EventListener -> IO Bool
foreign import javascript unsafe "((cb) => { const t = new EventTarget(); let reached = false; t.addEventListener('click', cb); t.addEventListener('click', () => { reached = true; }); t.dispatchEvent(new Event('click')); return !reached; })" js_stopped :: EventListener -> IO Bool
#else
foreign import javascript unsafe "true" js_true :: Foreign Bool
foreign import javascript unsafe "false" js_false :: Foreign Bool
foreign import javascript unsafe "'hello'" js_string :: Foreign String
foreign import javascript unsafe "42" js_int :: Foreign Int
foreign import javascript unsafe "({ answer: 42 })" js_object :: Foreign ()
foreign import javascript unsafe "null" js_nullish :: Nullable ()
foreign import javascript unsafe "const child = {}; const sibling = { previousSibling: child }; return { child, sibling, parent: { lastChild: child, appendChild() { throw new Error('unexpected appendChild'); }, insertBefore() { throw new Error('unexpected insertBefore'); } } }" js_dom_fixture :: Foreign DOMFixture
foreign import javascript unsafe "$1.child" js_fixture_child_foreign :: Foreign DOMFixture -> Foreign Node
foreign import javascript unsafe "$1.sibling" js_fixture_sibling_foreign :: Foreign DOMFixture -> Foreign Node
foreign import javascript unsafe "$1.parent" js_fixture_parent_foreign :: Foreign DOMFixture -> Foreign ParentNode
foreign import javascript unsafe "const t = new EventTarget(); t.addEventListener('submit', $1); return !t.dispatchEvent(new Event('submit', {cancelable:true}));" js_cancelled :: EventListener -> IO Bool
foreign import javascript unsafe "const t = new EventTarget(); let reached = false; t.addEventListener('click', $1); t.addEventListener('click', () => { reached = true; }); t.dispatchEvent(new Event('click')); return !reached;" js_stopped :: EventListener -> IO Bool
#endif

js_fixture_child :: Foreign DOMFixture -> Node
js_fixture_child = Node . js_fixture_child_foreign

js_fixture_sibling :: Foreign DOMFixture -> Node
js_fixture_sibling = Node . js_fixture_sibling_foreign

js_fixture_parent :: Foreign DOMFixture -> Node
js_fixture_parent = Node . js_fixture_parent_foreign

spec :: Spec
spec = describe "GHCJS FFI" $ do
  it "cancels the default action before dispatch returns" $ do
    listener <- DOM.runBrowserDOM $ DOM.mkEventListener Event.preventDefault
    assertEqual "canceled" True =<< js_cancelled listener
  it "stops later listeners during the same dispatch" $ do
    listener <- DOM.runBrowserDOM $ DOM.mkEventListener Event.stopImmediatePropagation
    assertEqual "stopped" True =<< js_stopped listener
  it "continues a blocking event handler after canceling the event" $ do
    finished <- newIORef False
    listener <- DOM.runBrowserDOM $ DOM.mkEventListener $ \event -> do
      Event.preventDefault event
      liftIO $ threadDelay 10_000
      liftIO $ writeIORef finished True
    assertEqual "canceled before blocking" True =<< js_cancelled listener
    threadDelay 50_000
    assertEqual "handler resumed" True =<< readIORef finished
  it "converts true to True" $
    assertWith "foreignToBool should convert true to True" (foreignToBool js_true)
  it "converts false to False" $
    assertWith "foreignToBool should convert false to False" (not (foreignToBool js_false))
  it "converts strings" $
    assertEqual "foreignToString" "hello" (foreignToString js_string)
  it "converts integers" $
    assertEqual "foreignToInt" 42 (foreignToInt js_int)
  it "reads object properties" $
    assertEqual "readProp" (Just 42) (readProp "answer" (Just . foreignToInt) js_object)
  it "recognizes null" $
    assertWith "nullableToMaybe should recognize null" (isNothing $ nullableToMaybe js_nullish)
  it "does not append a child already in the final position"
    $ DOM.runBrowserDOM
    $ DOM.appendChild (js_fixture_child js_dom_fixture) (js_fixture_parent js_dom_fixture)
  it "does not insert a child already before the reference node"
    $ DOM.runBrowserDOM
    $ DOM.insertBefore (js_fixture_child js_dom_fixture) (js_fixture_sibling js_dom_fixture) (js_fixture_parent js_dom_fixture)

#else

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "GHCJS FFI" $ pure ()

#endif
