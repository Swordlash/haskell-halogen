module Test.GHCJS (spec) where

import Prelude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

import Data.Foreign
import Data.Maybe (isNothing)
import Halogen.VDom.DOM.Monad qualified as DOM
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)
import Web.DOM.Internal.Types (Node (..))
import Web.DOM.ParentNode (ParentNode (..))

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
#endif

js_fixture_child :: Foreign DOMFixture -> Node
js_fixture_child = Node . js_fixture_child_foreign

js_fixture_sibling :: Foreign DOMFixture -> Node
js_fixture_sibling = Node . js_fixture_sibling_foreign

js_fixture_parent :: Foreign DOMFixture -> ParentNode
js_fixture_parent = ParentNode . js_fixture_parent_foreign

spec :: Spec
spec = describe "GHCJS FFI" $ do
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
  it "does not append a child already in the final position" $
    (DOM.appendChild (js_fixture_child js_dom_fixture) (js_fixture_parent js_dom_fixture) :: IO ())
  it "does not insert a child already before the reference node" $
    (DOM.insertBefore (js_fixture_child js_dom_fixture) (js_fixture_sibling js_dom_fixture) (js_fixture_parent js_dom_fixture) :: IO ())

#else

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "GHCJS FFI" $ pure ()

#endif
