{-# LANGUAGE CPP #-}

-- | Tests for the VDom build/patch/halt machinery, run against the in-memory
-- DOM in "Halogen.VDom.DOM.Monad.Native".
--
-- These are the first tests to drive 'buildVDom' and 'step' directly. Before
-- the native backend existed there was nowhere to run them: the only 'MonadDOM'
-- instances needed a browser, and the driver-level test deliberately supplies
-- its own DOM-free 'RenderSpec'.
--
-- The properties worth holding onto are about /identity/, not just output —
-- patching is supposed to mutate the nodes already in the tree rather than
-- rebuild them, and that is exactly what a serialised snapshot cannot see.
module Test.NativeDom (spec) where

import Prelude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "native VDom" $ pure ()

#else

import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Text (Text)
import Data.Void (Void, absurd)
import Halogen.VDom.DOM (VDomSpec (..), buildVDom)
import Halogen.VDom.DOM.Monad (MemDOM (..), appendChild, propertyEquals, runMemDOM, setProperty)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.DOM.Prop (Prop (..), PropValue (..), buildProp)
import Halogen.VDom.Machine (Step, extract)
import Halogen.VDom.Machine qualified as M
import Halogen.VDom.Types (ElemName (..), VDom (..))
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)
import Web.DOM.Internal.Types (Document, Element, Node)
import Web.Event.Event (EventType (..))
import Web.HTML.Common (AttrName (..))

-- | The tests never build widgets, so the widget type is uninhabited.
type TestVDom = VDom [Prop Text] Void

type TestStep = Step MemDOM TestVDom Node

-- | A spec over a fresh document, plus the sink that collects whatever the
-- handlers emit.
newSpec :: IO (VDomSpec MemDOM [Prop Text] Void, IORef [Text])
newSpec = do
  doc <- N.newDocument
  emitted <- newIORef []
  let vspec =
        VDomSpec
          { buildWidget = \_ -> absurd
          , buildAttributes = buildProp (\msg -> liftIO (modifyIORef' emitted (<> [msg])))
          , document = N.fromNative doc :: Document
          }
  pure (vspec, emitted)

-- The machinery runs in MemDOM; the assertions are ordinary IO, so each of
-- the three machine operations is unwrapped once here rather than at every
-- call in the spec below.
build :: VDomSpec MemDOM [Prop Text] Void -> TestVDom -> IO TestStep
build vspec = runMemDOM . buildVDom vspec

step :: TestStep -> TestVDom -> IO TestStep
step s = runMemDOM . M.step s

halt :: TestStep -> IO ()
halt = runMemDOM . M.halt

-- | The rendered node of a step, as HTML.
snapshot :: TestStep -> IO Text
snapshot = N.renderToText . N.toNative . extract

-- | The identity of the rendered node. Stable across a patch exactly when the
-- machinery reused the node instead of replacing it.
identOf :: TestStep -> Int
identOf s = (N.toNative (extract s)).ident

childIdents :: TestStep -> IO [Int]
childIdents s = map (.ident) <$> N.childNodes (N.toNative (extract s))

el :: Text -> [Prop Text] -> [TestVDom] -> TestVDom
el name props = Elem Nothing (ElemName name) props

keyed :: Text -> [(Text, TestVDom)] -> TestVDom
keyed name = Keyed Nothing (ElemName name) []

attr :: Text -> Text -> Prop Text
attr name = Attribute Nothing (AttrName name)

item :: Text -> TestVDom
item t = el "li" [] [Text t]

spec :: Spec
spec = describe "native VDom" $ do
  describe "building" $ do
    it "renders nested elements and text" $ do
      (vspec, _) <- newSpec
      s <- build vspec $ el "div" [attr "class" "box"] [el "p" [] [Text "hello"], Text "!"]
      assertEqual "markup"
        "<div class=\"box\"><p>hello</p>!</div>"
        =<< snapshot s

    it "escapes text and attribute values" $ do
      (vspec, _) <- newSpec
      s <- build vspec $ el "div" [attr "title" "a\"b&c"] [Text "1 < 2 & 3 > 2"]
      assertEqual "markup"
        "<div title=\"a&quot;b&amp;c\">1 &lt; 2 &amp; 3 &gt; 2</div>"
        =<< snapshot s

    it "omits the closing tag for void elements" $ do
      (vspec, _) <- newSpec
      s <- build vspec $ el "div" [] [el "br" [] [], el "img" [attr "src" "x.png"] []]
      assertEqual "markup" "<div><br><img src=\"x.png\"></div>" =<< snapshot s

  describe "patching" $ do
    it "mutates the existing node when the tag is unchanged" $ do
      (vspec, _) <- newSpec
      s0 <- build vspec $ el "div" [attr "class" "before"] [Text "one"]
      s1 <- step s0 $ el "div" [attr "class" "after"] [Text "two"]
      assertEqual "node is reused" (identOf s0) (identOf s1)
      assertEqual "markup" "<div class=\"after\">two</div>" =<< snapshot s1

    it "replaces the node when the tag changes" $ do
      (vspec, _) <- newSpec
      s0 <- build vspec $ el "div" [] [Text "x"]
      s1 <- step s0 $ el "span" [] [Text "x"]
      assertWith "node is not reused" (identOf s0 /= identOf s1)
      assertEqual "markup" "<span>x</span>" =<< snapshot s1

    it "removes attributes that are gone from the new vdom" $ do
      (vspec, _) <- newSpec
      s0 <- build vspec $ el "div" [attr "id" "a", attr "title" "t"] []
      s1 <- step s0 $ el "div" [attr "id" "a"] []
      assertEqual "markup" "<div id=\"a\"></div>" =<< snapshot s1

    it "updates a text node in place" $ do
      (vspec, _) <- newSpec
      s0 <- build vspec $ Text "before"
      s1 <- step s0 $ Text "after"
      assertEqual "node is reused" (identOf s0) (identOf s1)
      assertEqual "markup" "after" =<< snapshot s1

    it "grows and shrinks unkeyed children" $ do
      (vspec, _) <- newSpec
      s0 <- build vspec $ el "ul" [] [el "li" [] [Text "a"]]
      s1 <- step s0 $ el "ul" [] [el "li" [] [Text "a"], el "li" [] [Text "b"]]
      assertEqual "grown" "<ul><li>a</li><li>b</li></ul>" =<< snapshot s1
      s2 <- step s1 $ el "ul" [] [el "li" [] [Text "z"]]
      assertEqual "shrunk" "<ul><li>z</li></ul>" =<< snapshot s2

  -- Properties are the branch of diffProp gated on unsafeRefEq': structurally
  -- equal but separately allocated PropValues are ref-unequal, so these patches
  -- all take the fall-through path rather than the short-circuit.
  describe "properties" $ do
    it "sets properties without serialising them into markup" $ do
      (vspec, _) <- newSpec
      s <- build vspec $ el "input" [Property "value" (TxtProp "typed")] []
      assertEqual "markup" "<input>" =<< snapshot s
      assertEqual "property is set" [("value", "typed")]
        =<< N.propertyList (N.toNative (extract s))

    it "updates and prunes properties across a patch" $ do
      (vspec, _) <- newSpec
      s0 <- build vspec $ el "input" [Property "value" (TxtProp "one"), Property "tabindex" (IntProp (1 :: Int))] []
      assertEqual "initial" [("tabindex", "1"), ("value", "one")]
        =<< N.propertyList (N.toNative (extract s0))
      s1 <- step s0 $ el "input" [Property "value" (TxtProp "two")] []
      assertEqual "updated and pruned" [("value", "two")]
        =<< N.propertyList (N.toNative (extract s1))

    -- The "value" special case in diffProp consults propertyEquals to avoid
    -- clobbering an input the user is typing into. The JS backend compares
    -- JSVals by reference here; wasm uses === and so does this backend.
    it "compares properties by value, not by reference" $ do
      element <- N.fromNative <$> N.newElement Nothing (ElemName "input")
      runMemDOM $ setProperty "value" (TxtProp "abc") (element :: Element)
      assertWith "a distinct but equal value compares equal"
        =<< runMemDOM (propertyEquals "value" (TxtProp ("ab" <> "c")) element)
      assertWith "a different value compares unequal" . not
        =<< runMemDOM (propertyEquals "value" (TxtProp "abd") element)

    -- The browser holds a JS number here, and `1 === "1"` is false. A slot
    -- that compared rendered text would wrongly call these equal and skip the
    -- write.
    it "does not conflate a numeric property with its string spelling" $ do
      element <- N.fromNative <$> N.newElement Nothing (ElemName "input")
      runMemDOM $ setProperty "value" (IntProp (1 :: Int)) (element :: Element)
      assertWith "the same number compares equal"
        =<< runMemDOM (propertyEquals "value" (IntProp (1 :: Int)) element)
      assertWith "the string \"1\" does not" . not
        =<< runMemDOM (propertyEquals "value" (TxtProp "1") element)

  describe "keyed reconciliation" $ do
    it "retains each child's node across a reorder" $ do
      (vspec, _) <- newSpec
      s0 <- build vspec $ keyed "ul" [("a", item "A"), ("b", item "B"), ("c", item "C")]
      before <- childIdents s0
      s1 <- step s0 $ keyed "ul" [("c", item "C"), ("a", item "A"), ("b", item "B")]
      assertEqual "children are the same nodes, reordered"
        [before !! 2, before !! 0, before !! 1]
        =<< childIdents s1
      assertEqual "markup" "<ul><li>C</li><li>A</li><li>B</li></ul>" =<< snapshot s1

    it "retains surviving keys when one is removed" $ do
      (vspec, _) <- newSpec
      s0 <- build vspec $ keyed "ul" [("a", item "A"), ("b", item "B"), ("c", item "C")]
      before <- childIdents s0
      s1 <- step s0 $ keyed "ul" [("a", item "A"), ("c", item "C")]
      assertEqual "only the removed key is gone"
        [before !! 0, before !! 2]
        =<< childIdents s1

    it "retains existing keys when a new one is inserted in the middle" $ do
      (vspec, _) <- newSpec
      s0 <- build vspec $ keyed "ul" [("a", item "A"), ("c", item "C")]
      before <- childIdents s0
      s1 <- step s0 $ keyed "ul" [("a", item "A"), ("b", item "B"), ("c", item "C")]
      after <- childIdents s1
      assertEqual "old keys keep their nodes"
        [before !! 0, before !! 1]
        [after !! 0, after !! 2]
      assertWith "the new key is a new node" (after !! 1 `notElem` before)
      assertEqual "markup" "<ul><li>A</li><li>B</li><li>C</li></ul>" =<< snapshot s1

    it "updates a retained child's content without replacing it" $ do
      (vspec, _) <- newSpec
      s0 <- build vspec $ keyed "ul" [("a", item "A")]
      before <- childIdents s0
      s1 <- step s0 $ keyed "ul" [("a", item "A prime")]
      assertEqual "node is reused" before =<< childIdents s1
      assertEqual "markup" "<ul><li>A prime</li></ul>" =<< snapshot s1

  describe "duplicate props" $ do
    it "settles on the last of two props with the same key, and stays there" $ do
      (vspec, _) <- newSpec
      let vdom = el "div" [attr "class" "a", attr "class" "b"] []
      s0 <- build vspec vdom
      assertEqual "build" "<div class=\"b\"></div>" =<< snapshot s0
      s1 <- step s0 vdom
      assertEqual "first patch" "<div class=\"b\"></div>" =<< snapshot s1
      s2 <- step s1 vdom
      assertEqual "second patch" "<div class=\"b\"></div>" =<< snapshot s2

    it "treats two children under one key as one child" $ do
      (vspec, _) <- newSpec
      let vdom = keyed "ul" [("a", item "first"), ("a", item "second"), ("b", item "other")]
      s0 <- build vspec vdom
      assertEqual "build" "<ul><li>second</li><li>other</li></ul>" =<< snapshot s0
      s1 <- step s0 vdom
      assertEqual "patch" "<ul><li>second</li><li>other</li></ul>" =<< snapshot s1

  describe "event handlers" $ do
    it "registers a listener for each handler prop" $ do
      (vspec, _) <- newSpec
      s <- build vspec $ el "button" [Handler (EventType "click") (const (Just "clicked"))] []
      assertEqual "listener registered" ["click"]
        =<< N.listenerTypes (N.toNative (extract s))

    it "removes the listener when the handler prop is dropped" $ do
      (vspec, _) <- newSpec
      s0 <- build vspec $ el "button" [Handler (EventType "click") (const (Just "clicked"))] []
      s1 <- step s0 $ el "button" [] []
      assertEqual "listener removed" [] =<< N.listenerTypes (N.toNative (extract s1))

  describe "halting" $
    it "detaches the rendered subtree from its parent" $ do
      (vspec, _) <- newSpec
      s <- build vspec $ el "div" [] [el "span" [] [Text "x"]]
      root <- N.newElement Nothing (ElemName "root")
      runMemDOM $ appendChild (extract s) (N.fromNative root)
      assertEqual "attached" 1 . length =<< N.childNodes root
      halt s
      assertEqual "detached" 0 . length =<< N.childNodes root

#endif
