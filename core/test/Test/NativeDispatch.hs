{-# LANGUAGE CPP #-}

-- | The in-memory DOM as a test drives a component with it: an event
-- dispatched at a node travels up the tree as in a page, with the node as
-- its target, a handler reads the tree through the @Web.*@ types (whether
-- an element is connected, contains another, its attributes, the element a
-- selector finds around it), and the test reads the tree with selectors.
module Test.NativeDispatch (spec) where

import Prelude

#if !defined(HALOGEN_WASI) && (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "dispatching on the in-memory DOM" $ pure ()

#else

import Control.Exception (try)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get, modify)
import Data.Aeson (Value, object, (.=))
import Data.Coerce (coerce)
import Data.Foldable (for_, traverse_)
import Data.Foreign (foreignToBool, foreignToString, readProp)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust, isNothing)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (JSError (..), JSVal)
import Halogen.Query.Event qualified as HQE
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad qualified as DOM
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.Driver (runUI)
import Halogen.VDom.Types (ElemName (..), Namespace (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Web.DOM.Element qualified as Element
import Web.DOM.Internal.Types (Element (..), EventListener, HTMLElement (..), Node, toNode)
import Web.Event.Event (Event, EventTarget, EventType (..))
import Web.Event.Event qualified as Event
import Web.HTML.Common (AttrName (..))
import Web.HTML.HTMLInputElement (HTMLInputElement (..))
import Web.UIEvent.KeyboardEvent qualified as KE
import Web.UIEvent.MouseEvent qualified as ME

--------------------------------------------------------------------------------
-- A tree built by hand
--------------------------------------------------------------------------------

-- | An element with attributes and children, made through the DOM monad as
-- the reconciler makes one.
el :: Text -> [(Text, Text)] -> [IO N.NativeNode] -> IO N.NativeNode
el = elIn Nothing

elIn :: Maybe Namespace -> Text -> [(Text, Text)] -> [IO N.NativeNode] -> IO N.NativeNode
elIn ns tag attributes children = do
  node <- N.newElement ns (ElemName tag)
  runMemDOM $ for_ attributes $ \(k, v) -> DOM.setAttribute Nothing (AttrName k) v (N.fromNative node :: Element)
  for_ children $ \make -> make >>= (`appendTo` node)
  pure node

txt :: Text -> IO N.NativeNode
txt t = N.toNative <$> runMemDOM (DOM.createTextNode t (N.fromNative N.ambientDocument))

appendTo :: N.NativeNode -> N.NativeNode -> IO ()
appendTo child parent = runMemDOM (DOM.appendChild (N.fromNative child :: Node) (N.fromNative parent :: Node))

-- | The tree the selector table and the dispatch tests read, in the body of
-- a document of its own; each element names itself in @data-t@.
page :: IO (N.NativeNode, N.NativeNode)
page = do
  doc <- N.newDocument
  root <-
    el
      "div"
      [("data-t", "root"), ("id", "root"), ("class", "box main"), ("data-k", "a b"), ("lang", "en-US")]
      [ el
          "ul"
          [("data-t", "list"), ("class", "list")]
          [ el "li" [("data-t", "li1"), ("class", "item"), ("data-id", "1")] [txt "one"]
          , el "li" [("data-t", "li2"), ("class", "item unread"), ("data-id", "2")] [el "a" [("data-t", "a2"), ("href", "/p/2")] [txt "two"]]
          , el "li" [("data-t", "li3"), ("class", "item"), ("data-id", "3")] [el "span" [("data-t", "span3")] [txt "three"]]
          ]
      , el "p" [("data-t", "p")] [el "a" [("data-t", "ax"), ("href", "/x y"), ("title", "Hello World")] [txt "x"]]
      , elIn (Just svg) "svg" [("data-t", "svg")] [elIn (Just svg) "foreignObject" [("data-t", "fo")] []]
      , el "button" [("data-t", "off"), ("disabled", "")] [el "span" [("data-t", "offspan")] [txt "off"]]
      , el
          "fieldset"
          [("data-t", "fieldset"), ("disabled", "")]
          [ el "legend" [("data-t", "legend")] [el "button" [("data-t", "inlegend")] []]
          , el "button" [("data-t", "infieldset")] []
          ]
      , el "button" [("data-t", "on")] []
      ]
  bodies <- N.querySelectorAll "body" doc
  for_ bodies (appendTo root)
  pure (doc, root)
  where
    svg = Namespace "http://www.w3.org/2000/svg"

label :: N.NativeNode -> IO Text
label node = maybe "?" id <$> N.attribute "data-t" node

labels :: [N.NativeNode] -> IO [Text]
labels = traverse label

named :: Text -> N.NativeNode -> IO N.NativeNode
named t root =
  N.querySelector ("[data-t=\"" <> t <> "\"]") root >>= \case
    Just n -> pure n
    Nothing -> fail ("no element " <> T.unpack t)

-- | What a selector that is refused throws: its name, and whether its
-- message names the selector.
refusal :: Text -> N.NativeNode -> IO (Maybe (Text, Bool))
refusal selector root =
  try (N.querySelectorAll selector root) >>= \case
    Left (e :: JSError) -> pure (Just (e.name, T.pack (show selector) `T.isInfixOf` e.message))
    Right _ -> pure Nothing

--------------------------------------------------------------------------------
-- Listeners added by hand
--------------------------------------------------------------------------------

-- | A listener on a node that notes its name, the target's and the current
-- target's, and then does something to the event.
listen :: IORef [Text] -> Text -> Text -> (Event -> MemDOM ()) -> N.NativeNode -> IO EventListener
listen heard eventType name act node = runMemDOM $ do
  l <- DOM.mkEventListener $ \ev -> do
    t <- liftIO (maybe (pure "-") (label . N.toNative) (Event.target ev))
    c <- liftIO (maybe (pure "-") (labelOrDocument . N.toNative) (Event.currentTarget ev))
    liftIO (modifyIORef' heard (<> [name <> "@" <> c <> "<" <> t]))
    act ev
  DOM.addEventListener (EventType eventType) l (N.fromNative node :: EventTarget)
  pure l
  where
    labelOrDocument n = N.attribute "data-t" n >>= maybe (pure "document") pure

-- | What reads of a node's handle find.
handle :: N.NativeNode -> JSVal
handle n = coerce (N.fromNative n :: Element)

none :: Event -> MemDOM ()
none _ = pure ()

--------------------------------------------------------------------------------
-- Components
--------------------------------------------------------------------------------

data Popup = Popup {holder :: H.RefLabel, open :: Bool}

data PopupAction = Watch | Close | Reopen

-- | A popup that closes on a press outside it and on Escape, written as a
-- page's is: a listener on the document for the press, which asks whether
-- the popup is still in the page and whether the press was inside it, and
-- the window's keys, leaving an Escape something prevented alone.
popup :: H.Component H.VoidF () Void MemDOM
popup =
  H.component
    (\() -> (\h -> Popup {holder = h, open = True}) <$> H.newRefLabel "holder")
    render
    [ H.onInitialize (act Watch)
    , H.onAction act
    ]
  where
    render :: Popup -> H.ComponentHTML PopupAction Empty MemDOM
    render s =
      HH.div_
        [ HH.div
            [HP.ref s.holder, HP.class_ (HH.ClassName "panel")]
            [ HH.button [HP.class_ (HH.ClassName "inside")] [HH.text "inside"]
            , HH.input [HP.class_ (HH.ClassName "keeps"), HE.preventingDefault (HE.matching (HE.keys ["Escape"]) (HE.onKeyDown (const Reopen)))]
            ]
        , HH.p [HP.class_ (HH.ClassName "state")] [HH.text (if s.open then "open" else "closed")]
        ]
    act = \case
      Watch -> do
        s <- get
        found <- H.getRef s.holder
        for_ found $ \holder -> do
          presses <- H.liftEffect $ do
            doc <- DOM.window >>= DOM.document
            events <- HS.lowerEmitter (HQE.eventListener (EventType "pointerdown") (N.fromNative (N.toNative doc)) Just)
            pure (pressedOutside holder events)
          void (H.subscribe (Close <$ presses))
          keys <- H.liftEffect $ do
            w <- DOM.window
            target <- DOM.windowToEventTarget w
            HS.lowerEmitter (HQE.eventListener (EventType "keydown") target Just)
          void (H.subscribe (Close <$ escapes holder keys))
      Close -> modify $ \s -> s {open = False}
      Reopen -> modify $ \s -> s {open = True}

pressedOutside :: Element -> HS.Emitter IO Event -> HS.Emitter IO ()
pressedOutside holder (HS.Emitter register) =
  HS.makeEmitter $ \push -> do
    HS.Subscription stop <- register $ \press -> do
      connected <- holder.isConnected
      case Event.target press >>= Element.fromEventTarget of
        Just target | connected -> do
          inside <- holder.contains (toNode target)
          unless inside (push ())
        _ -> pure ()
    pure stop

escapes :: Element -> HS.Emitter IO Event -> HS.Emitter IO ()
escapes holder (HS.Emitter register) =
  HS.makeEmitter $ \push -> do
    HS.Subscription stop <- register $ \pressed -> do
      connected <- holder.isConnected
      when (connected && KE.key (KE.KeyboardEvent pressed) == "Escape" && not (Event.defaultPreventedNow pressed)) (push ())
    pure stop

-- | A container whose one click handler follows the links inside it, as a
-- page of rendered Markdown does (pull request 121's, now on a real tree).
delegated :: H.Component H.VoidF () Void MemDOM
delegated =
  H.component
    (\() -> pure "")
    render
    [H.onAction (\href -> modify (const href))]
  where
    render :: Text -> H.ComponentHTML Text Empty MemDOM
    render followed =
      HH.a
        [HP.href "/outer"]
        [ HH.div
            [ HP.class_ (HH.ClassName "links")
            , HE.preventingDefault (HE.matching (HE.within "a[href]" HE.plainLeftClick) (HE.handler' (EventType "click") followed'))
            ]
            [ HH.a [HP.href "/p/1", HP.class_ (HH.ClassName "one")] [HH.span [HP.class_ (HH.ClassName "inner")] [HH.text "one"]]
            , HH.a [HP.href "/p/2", HP.target "_blank", HP.class_ (HH.ClassName "two")] [HH.text "two"]
            , HH.a [HP.href "/p/3", HP.download "", HP.class_ (HH.ClassName "three")] [HH.text "three"]
            , HH.a [HP.class_ (HH.ClassName "bare")] [HH.text "no address"]
            , HH.p [HP.class_ (HH.ClassName "prose")] [HH.text "prose"]
            ]
        , HH.p [HP.class_ (HH.ClassName "followed")] [HH.text followed]
        ]
    followed' event = do
      target <- Event.target event >>= Element.fromEventTarget
      anchor <- Element.closestNow target "a[href]"
      Element.getAttributeNow anchor "href"

data Note = Note {ident :: Int, title :: Text, unread :: Bool}

data Inbox = MarkRead | Typed Text | Outer | Inner

-- | A list of notifications with a button that marks them read, which is
-- disabled once none is unread, and a field whose input it keeps.
inbox :: H.Component H.VoidF () Void MemDOM
inbox =
  H.component
    (\() -> pure ([Note 1 "First" True, Note 2 "Second" False, Note 3 "Third" True], "", [] :: [Text]))
    render
    [ H.onAction $ \case
        MarkRead -> modify $ \(notes, typed, clicks) -> (map (\n -> n {unread = False}) notes, typed, clicks)
        Typed t -> modify $ \(notes, _, clicks) -> (notes, t, clicks)
        Outer -> modify $ \(notes, typed, clicks) -> (notes, typed, clicks <> ["outer"])
        Inner -> modify $ \(notes, typed, clicks) -> (notes, typed, clicks <> ["inner"])
    ]
  where
    render :: ([Note], Text, [Text]) -> H.ComponentHTML Inbox Empty MemDOM
    render (notes, typed, clicks) =
      HH.div
        [HE.onClick (const Outer)]
        [ HH.ul_
            [ HH.li
                [HP.classes (map HH.ClassName ("notification" : ["unread" | n.unread])), HP.attr (HH.AttrName "data-id") (T.pack (show n.ident))]
                [HH.text n.title]
            | n <- notes
            ]
        , HH.button [HP.class_ (HH.ClassName "mark"), HP.disabled (not (any (.unread) notes)), HE.onClick (const MarkRead)] [HH.text "Mark all read"]
        , HH.button [HP.class_ (HH.ClassName "stops"), HE.stoppingPropagation (HE.onClick (const Inner))] [HH.text "stops"]
        , HH.input [HP.class_ (HH.ClassName "field"), HE.onValueInput Typed]
        , HH.p [HP.class_ (HH.ClassName "typed")] [HH.text typed]
        , HH.p [HP.class_ (HH.ClassName "clicks")] [HH.text (T.intercalate "," clicks)]
        ]

-- | A handler whose declared match names a selector the in-memory DOM does
-- not understand.
hovering :: H.Component H.VoidF () Void MemDOM
hovering =
  H.component
    (\() -> pure ())
    render
    [H.onAction (\() -> pure ())]
  where
    render :: () -> H.ComponentHTML () Empty MemDOM
    render () =
      HH.div
        [HE.matching (HE.within "a:hover" HE.plainLeftClick) (HE.onClick (const ()))]
        [HH.a [HP.href "/h", HP.class_ (HH.ClassName "link")] [HH.text "link"]]

mount :: H.Component H.VoidF () Void MemDOM -> IO (N.NativeNode, IO ())
mount component = do
  container <- N.newContainer
  H.HalogenSocket {H.dispose = dispose} <- runMemDOM (runUI component () (N.fromNative container))
  pure (container, runMemDOM dispose >> (N.fromNative container :: Element).remove)

one :: Text -> N.NativeNode -> IO N.NativeNode
one selector root =
  N.querySelector selector root >>= \case
    Just n -> pure n
    Nothing -> fail ("nothing matches " <> T.unpack selector)

textOf :: Text -> N.NativeNode -> IO Text
textOf selector root = N.textContent =<< one selector root

click :: Value
click = object ["button" .= (0 :: Int)]

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "the in-memory DOM, driven and read as a page" $ do
  describe "selectors" $ do
    it "find what each kind of selector matches" $ do
      (_, root) <- page
      let table =
            [ ("li", ["li1", "li2", "li3"])
            , ("LI", ["li1", "li2", "li3"])
            , ("#root", [])
            , ("ul *", ["li1", "li2", "a2", "li3", "span3"])
            , (".item", ["li1", "li2", "li3"])
            , (".item.unread", ["li2"])
            , ("li.unread a", ["a2"])
            , ("[href]", ["a2", "ax"])
            , ("[data-id='2']", ["li2"])
            , ("[data-id=\"3\"]", ["li3"])
            , ("a[href^='/p']", ["a2"])
            , ("a[href$=\"2\"]", ["a2"])
            , ("a[href*='x y']", ["ax"])
            , ("a[title='hello world']", [])
            , ("a[title='hello world' i]", ["ax"])
            , ("[data-k~=b]", [])
            , ("ul > li", ["li1", "li2", "li3"])
            , ("div li", ["li1", "li2", "li3"])
            , ("div > li", [])
            , ("ul > li > a", ["a2"])
            , ("li a, p a", ["a2", "ax"])
            , ("p a , li > a", ["a2", "ax"])
            , ("li:not(.unread)", ["li1", "li3"])
            , ("li:not(.unread, [data-id='3'])", ["li1"])
            , ("foreignObject", ["fo"])
            , ("foreignobject", [])
            , ("svg > foreignObject", ["fo"])
            , ("[data-t=r\\6f ot] ul", ["list"])
            ]
      for_ table $ \(selector, expected) -> do
        found <- labels =<< N.querySelectorAll selector root
        (selector, found) `shouldBe` (selector, expected)
      -- The root itself is not inside itself; from its document it is found.
      doc <- maybe (fail "no parent") pure =<< (readIORef . (.parentRef) =<< pure root)
      (labels =<< N.querySelectorAll "#root[data-k~=b][lang|=en]" doc) `shouldReturn` ["root"]
      (fmap label <$> N.querySelector ".item" root) >>= sequence >>= (`shouldBe` Just "li1")

    it "refuse what they do not understand, loudly, naming the selector" $ do
      (_, root) <- page
      for_ ["li:first-child", "li + li", "li ~ li", "a::before", "svg|a", "li:not(ul li)", "a:hover"] $ \selector ->
        (selector,) <$> refusal selector root >>= (`shouldBe` (selector, Just ("NotSupportedError", True)))
      for_ ["li[", "", ">", "a,", ", a", "[data-id=1]", ".1a", "a[title='open", "li > ", "a[title=x y]"] $ \selector ->
        (selector,) <$> refusal selector root >>= (`shouldBe` (selector, Just ("SyntaxError", True)))

    it "find the nearest element around a node, the node itself first" $ do
      (_, root) <- page
      a2 <- named "a2" root
      traverse label (N.toNative <$> Element.closestNow (N.fromNative a2) "li") `shouldReturn` Just "li2"
      (fmap N.toNative <$> (N.fromNative a2 :: Element).closest "ul.list") >>= traverse label >>= (`shouldBe` Just "list")
      (fmap N.toNative <$> (N.fromNative a2 :: Element).closest "a") >>= traverse label >>= (`shouldBe` Just "a2")
      (isNothing <$> (N.fromNative a2 :: Element).closest "section") `shouldReturn` True

  describe "an element's reads" $ do
    it "say whether it is in a document, and stop saying so once it is taken out" $ do
      (_, root) <- page
      a2 <- named "a2" root
      (N.fromNative a2 :: Element).isConnected `shouldReturn` True
      detached <- el "div" [] [el "span" [] []]
      [inner] <- N.childNodes detached
      (N.fromNative inner :: Element).isConnected `shouldReturn` False
      li2 <- named "li2" root
      (N.fromNative li2 :: Element).remove
      (N.fromNative a2 :: Element).isConnected `shouldReturn` False
      N.isConnected li2 `shouldReturn` False
      container <- N.newContainer
      N.isConnected container `shouldReturn` True

    it "give the document's head and body, to put a node in" $ do
      document <- runMemDOM (DOM.window >>= DOM.document)
      style <- N.newElement Nothing (ElemName "style")
      void ((coerce document.head :: Element).appendChild (toNode (N.fromNative style :: Element)))
      N.isConnected style `shouldReturn` True
      Just found <- N.querySelector "head > style" N.ambientDocument
      (found == style) `shouldBe` True
      Just body <- document.body
      (isJust <$> (coerce body :: Element).closest "html") `shouldReturn` True
      (N.fromNative style :: Element).remove

    it "say what they contain, a node itself included" $ do
      (_, root) <- page
      [list, a2, li3] <- traverse (`named` root) ["list", "a2", "li3"]
      let contains a b = (N.fromNative a :: Element).contains (N.fromNative b)
      contains list a2 `shouldReturn` True
      contains a2 a2 `shouldReturn` True
      contains a2 list `shouldReturn` False
      contains li3 a2 `shouldReturn` False

    it "read attributes, text and the selectors' answers through the Web types" $ do
      (_, root) <- page
      ax <- named "ax" root
      let e = N.fromNative ax :: Element
      e.getAttribute "href" `shouldReturn` Just "/x y"
      e.getAttribute "HREF" `shouldReturn` Just "/x y"
      e.hasAttribute "title" `shouldReturn` True
      e.getAttribute "download" `shouldReturn` Nothing
      Element.getAttributeNow e "title" `shouldBe` Just "Hello World"
      e.textContent `shouldReturn` "x"
      Element.textContent (N.fromNative root) >>= (`shouldBe` "onetwothreexoff")
      (isNothing <$> e.querySelector "b") `shouldReturn` True
      e.setAttribute "data-x" "1"
      N.attribute "data-x" ax `shouldReturn` Just "1"
      e.textContent_set "changed"
      N.textContent ax `shouldReturn` "changed"

    it "read a property that reflects an attribute as the attribute, and not one that does not" $ do
      button <- N.newElement Nothing (ElemName "button")
      input <- N.newElement Nothing (ElemName "input")
      runMemDOM $ do
        DOM.setProperty "disabled" (DOM.BoolProp True) (N.fromNative button :: Element)
        DOM.setProperty "className" (DOM.TxtProp "a b") (N.fromNative button :: Element)
        DOM.setProperty "hidden" (DOM.BoolProp False) (N.fromNative button :: Element)
        DOM.setProperty "value" (DOM.TxtProp "typed") (N.fromNative input :: Element)
        DOM.setAttribute Nothing (AttrName "value") "initial" (N.fromNative input :: Element)
      N.attribute "disabled" button `shouldReturn` Just ""
      (N.fromNative button :: Element).getAttribute "disabled" `shouldReturn` Just ""
      N.attribute "class" button `shouldReturn` Just "a b"
      N.attribute "hidden" button `shouldReturn` Nothing
      wrapper <- el "div" [] []
      appendTo button wrapper
      (length <$> N.querySelectorAll ".b[disabled]" wrapper) `shouldReturn` 1
      (length <$> N.querySelectorAll "[hidden]" wrapper) `shouldReturn` 0
      -- A value property is what the reader typed, not the attribute.
      N.attribute "value" input `shouldReturn` Just "initial"
      readProp "value" (Just . foreignToString) (handle input) `shouldBe` Just "typed"
      (HTMLInputElement (coerce (N.fromNative input :: Element))).value `shouldReturn` "typed"
      (HTMLInputElement (coerce (N.fromNative input :: Element))).value_set "set"
      readProp "value" (Just . foreignToString) (handle input) `shouldBe` Just "set"
      -- An attribute the program writes is what the element has from then
      -- on, whatever property the reconciler set before.
      runMemDOM $ do
        DOM.setProperty "disabled" (DOM.BoolProp False) (N.fromNative button :: Element)
        DOM.setProperty "className" (DOM.TxtProp "a b") (N.fromNative button :: Element)
      (N.fromNative button :: Element).setAttribute "disabled" ""
      N.attribute "disabled" button `shouldReturn` Just ""
      (.disabled) <$> N.dispatchOutcome "click" (object ["button" .= (0 :: Int)]) button `shouldReturn` True
      readProp "disabled" (Just . foreignToBool) (handle button) `shouldBe` Just True
      (N.fromNative button :: Element).setAttribute "class" "c"
      (length <$> N.querySelectorAll ".c" wrapper) `shouldReturn` 1
      (length <$> N.querySelectorAll ".a" wrapper) `shouldReturn` 0
      -- An attribute reads as its property when no property is set.
      a <- el "a" [("href", "/h"), ("hidden", "")] []
      readProp "href" (Just . foreignToString) (handle a) `shouldBe` Just "/h"
      readProp "tagName" (Just . foreignToString) (handle a) `shouldBe` Just "A"
      (N.fromNative a :: Element).isConnected `shouldReturn` False

  describe "dispatch" $ do
    it "runs the listeners on the node and each one around it, up to the document" $ do
      (doc, root) <- page
      heard <- newIORef []
      [a2, li2, list] <- traverse (`named` root) ["a2", "li2", "list"]
      for_ [("a2", a2), ("li2", li2), ("root", root)] $ \(n, node) -> listen heard "click" n none node
      void (listen heard "click" "doc" none doc)
      void (listen heard "keydown" "other type" none list)
      outcome <- N.dispatchOutcome "click" click a2
      readIORef heard `shouldReturn` ["a2@a2<a2", "li2@li2<a2", "root@root<a2", "doc@document<a2"]
      outcome `shouldBe` N.Dispatched {defaultPrevented = False, heard = 4, disabled = False}

    it "stops after the node's listeners on stopPropagation, at once on stopImmediatePropagation" $ do
      (_, root) <- page
      heard <- newIORef []
      [a2, li2] <- traverse (`named` root) ["a2", "li2"]
      void (listen heard "click" "first" Event.stopPropagation li2)
      void (listen heard "click" "second" none li2)
      void (listen heard "click" "root" none root)
      N.dispatch "click" click a2
      readIORef heard `shouldReturn` ["first@li2<a2", "second@li2<a2"]
      writeIORef heard []
      li3 <- named "li3" root
      void (listen heard "click" "first" Event.stopImmediatePropagation li3)
      void (listen heard "click" "second" none li3)
      N.dispatch "click" click li3
      readIORef heard `shouldReturn` ["first@li3<li3"]

    it "remembers a prevented default, for the listeners after and for the test" $ do
      (_, root) <- page
      a2 <- named "a2" root
      li2 <- named "li2" root
      seen <- newIORef []
      -- A pure read reads the event as it is when it is forced: here.
      let note ev = let prevented = Event.defaultPreventedNow ev in prevented `seq` liftIO (modifyIORef' seen (<> [prevented]))
      void (listen' "click" (\ev -> note ev >> Event.preventDefault ev) a2)
      void (listen' "click" note li2)
      (.defaultPrevented) <$> N.dispatchOutcome "click" click a2 `shouldReturn` True
      readIORef seen `shouldReturn` [False, True]
      -- An event that cannot be cancelled stays unprevented.
      void (listen' "input" Event.preventDefault a2)
      (.defaultPrevented) <$> N.dispatchOutcome "input" (object []) a2 `shouldReturn` False
      (.defaultPrevented) <$> N.dispatchOutcome "click" (object ["cancelable" .= False]) a2 `shouldReturn` False

    it "keeps an event that does not bubble on its node, unless it says it bubbles" $ do
      (_, root) <- page
      heard <- newIORef []
      a2 <- named "a2" root
      for_ ["focus", "mouseenter"] $ \ty -> do
        void (listen heard ty "a2" none a2)
        void (listen heard ty "root" none root)
        N.dispatch ty (object []) a2
      readIORef heard `shouldReturn` ["a2@a2<a2", "a2@a2<a2"]
      N.dispatch "focus" (object ["bubbles" .= True]) a2
      readIORef heard >>= (`shouldBe` ["a2@a2<a2", "root@root<a2"]) . drop 2

    it "dispatches no click at a disabled control, or one in a disabled fieldset outside its legend" $ do
      (_, root) <- page
      heard <- newIORef []
      void (listen heard "click" "root" none root)
      void (listen heard "keydown" "root" none root)
      [off, infieldset, inlegend, on, offspan] <- traverse (`named` root) ["off", "infieldset", "inlegend", "on", "offspan"]
      N.dispatchOutcome "click" click off `shouldReturn` N.Dispatched {defaultPrevented = False, heard = 0, disabled = True}
      (.disabled) <$> N.dispatchOutcome "click" click infieldset `shouldReturn` True
      readIORef heard `shouldReturn` []
      (.disabled) <$> N.dispatchOutcome "click" click inlegend `shouldReturn` False
      (.disabled) <$> N.dispatchOutcome "click" click on `shouldReturn` False
      -- Another event, and a click on what is inside, are dispatched.
      N.dispatch "keydown" (object ["key" .= ("a" :: Text)]) off
      (.disabled) <$> N.dispatchOutcome "click" click offspan `shouldReturn` False
      readIORef heard `shouldReturn` ["root@root<inlegend", "root@root<on", "root@root<off", "root@root<offspan"]
      -- The property disables as the attribute does, and enables again.
      runMemDOM (DOM.setProperty "disabled" (DOM.BoolProp True) (N.fromNative on :: Element))
      (.disabled) <$> N.dispatchOutcome "click" click on `shouldReturn` True
      runMemDOM (DOM.setProperty "disabled" (DOM.BoolProp False) (N.fromNative off :: Element))
      (.disabled) <$> N.dispatchOutcome "click" click off `shouldReturn` False

    it "reaches no document from a node in none" $ do
      heard <- newIORef []
      detached <- el "div" [("data-t", "detached")] [el "span" [("data-t", "inner")] []]
      [inner] <- N.childNodes detached
      void (listen heard "click" "detached" none detached)
      ambient <- listen heard "click" "ambient" none N.ambientDocument
      N.dispatch "click" click inner
      readIORef heard `shouldReturn` ["detached@detached<inner"]
      runMemDOM (DOM.removeEventListener (EventType "click") ambient (N.fromNative N.ambientDocument :: EventTarget))

    it "goes on through the nodes it started with when a listener takes its node out or another listener off" $ do
      (_, root) <- page
      heard <- newIORef []
      [a2, li2] <- traverse (`named` root) ["a2", "li2"]
      void (listen heard "click" "a2" (\_ -> liftIO (N.fromNative a2 :: Element).remove) a2)
      later <- newIORef Nothing
      void (listen heard "click" "first" (\_ -> liftIO (readIORef later >>= traverse_ (\l -> runMemDOM (DOM.removeEventListener (EventType "click") l (N.fromNative li2 :: EventTarget))))) li2)
      writeIORef later . Just =<< listen heard "click" "second" none li2
      void (listen heard "click" "root" none root)
      N.dispatch "click" click a2
      readIORef heard `shouldReturn` ["a2@a2<a2", "first@li2<a2", "root@root<a2"]
      N.isConnected a2 `shouldReturn` False

    it "dispatches at a text node's element, as the reader's pointer does" $ do
      (_, root) <- page
      heard <- newIORef []
      a2 <- named "a2" root
      [text] <- N.childNodes a2
      void (listen heard "click" "a2" none a2)
      N.dispatch "click" click text
      readIORef heard `shouldReturn` ["a2@a2<a2"]
      -- One with no element around it keeps it, and is no element.
      loose <- txt "loose"
      targets <- newIORef []
      void $ listen' "click" (\ev -> liftIO (modifyIORef' targets (<> [(isJust (Event.target ev), isJust (Event.target ev >>= Element.fromEventTarget))]))) loose
      N.dispatch "click" click loose
      readIORef targets `shouldReturn` [(True, False)]

    it "reads the JSON's fields, and the class the type is of" $ do
      (_, root) <- page
      a2 <- named "a2" root
      seen <- newIORef []
      void $ listen' "keydown" (\ev -> liftIO (modifyIORef' seen (<> [(KE.key (KE.KeyboardEvent ev), isJust (KE.fromEvent ev), isJust (ME.fromEvent ev))]))) a2
      void $ listen' "click" (\ev -> liftIO (modifyIORef' seen (<> [(T.pack (show (ME.button (ME.MouseEvent ev))), isJust (KE.fromEvent ev), isJust (ME.fromEvent ev))]))) a2
      N.dispatch "keydown" (object ["key" .= ("Escape" :: Text), "target" .= object ["tagName" .= ("P" :: Text)]]) a2
      N.dispatch "click" (object ["button" .= (2 :: Int)]) a2
      readIORef seen `shouldReturn` [("Escape", True, False), ("2", False, True)]

  describe "a component mounted in a container" $ do
    it "closes a popup on a press outside it and on Escape, and not on a press inside" $ do
      (container, dispose) <- mount popup
      elsewhere <- N.newContainer
      let state = textOf ".state" container
      state `shouldReturn` "open"
      N.dispatch "pointerdown" click =<< one ".inside" container
      N.dispatch "pointerdown" click =<< one ".panel" container
      state `shouldReturn` "open"
      N.dispatch "pointerdown" click elsewhere
      state `shouldReturn` "closed"
      -- An Escape the field keeps (it prevents its default, and reopens) is
      -- left alone; one anywhere else closes.
      N.dispatch "keydown" (object ["key" .= ("Escape" :: Text)]) =<< one ".keeps" container
      state `shouldReturn` "open"
      N.dispatch "keydown" (object ["key" .= ("Enter" :: Text)]) =<< one ".inside" container
      state `shouldReturn` "open"
      N.dispatch "keydown" (object ["key" .= ("Escape" :: Text)]) =<< one ".inside" container
      state `shouldReturn` "closed"
      dispose
      -- Gone, it hears nothing more, and the document has no listener of it.
      N.dispatch "pointerdown" click elsewhere
      N.listenerTypes N.ambientDocument >>= (`shouldBe` []) . filter (`elem` ["pointerdown", "keydown"])

    it "follows a plain click on a link inside, judged on the link found in the tree" $ do
      (container, dispose) <- mount delegated
      let followed = textOf ".followed" container
      -- The text inside the link is the target: the link is looked for
      -- from its parent element.
      [text] <- N.childNodes =<< one ".inner" container
      prevented <- (.defaultPrevented) <$> N.dispatchOutcome "click" click text
      followed `shouldReturn` "/p/1"
      prevented `shouldBe` True
      -- Another tab, a download, a key held, no address: not followed, and
      -- left to the page.
      for_ [(".two", click), (".three", click), (".one", object ["button" .= (0 :: Int), "metaKey" .= True]), (".bare", click)] $ \(selector, ev) -> do
        outcome <- N.dispatchOutcome "click" ev =<< one selector container
        (selector, outcome.defaultPrevented) `shouldBe` (selector, False)
      followed `shouldReturn` "/p/1"
      -- The prose is inside a link, but one around the container: no
      -- match.
      (.defaultPrevented) <$> (N.dispatchOutcome "click" click =<< one ".prose" container) `shouldReturn` False
      followed `shouldReturn` "/p/1"
      N.dispatch "click" click =<< one ".one" container
      followed `shouldReturn` "/p/1"
      dispose

    it "is read with selectors, its text and its attributes after an action" $ do
      (container, dispose) <- mount inbox
      (length <$> N.querySelectorAll ".notification.unread" container) `shouldReturn` 2
      (traverse N.textContent =<< N.querySelectorAll "li.unread" container) `shouldReturn` ["First", "Third"]
      (traverse (N.attribute "data-id") =<< N.querySelectorAll "li:not(.unread)" container) `shouldReturn` [Just "2"]
      mark <- one "button.mark" container
      N.attribute "disabled" mark `shouldReturn` Nothing
      N.dispatch "click" click mark
      N.querySelectorAll ".notification.unread" container >>= (`shouldBe` 0) . length
      N.attribute "disabled" mark `shouldReturn` Just ""
      textOf ".clicks" container `shouldReturn` "outer"
      -- Disabled now, it dispatches no click, and nothing around hears one.
      (.disabled) <$> N.dispatchOutcome "click" click mark `shouldReturn` True
      textOf ".clicks" container `shouldReturn` "outer"
      -- A handler that stops propagation keeps the container's from it.
      N.dispatch "click" click =<< one ".stops" container
      textOf ".clicks" container `shouldReturn` "outer,inner"
      -- Typing: the field's value set, then its input dispatched.
      field <- one ".field" container
      (HTMLInputElement (coerce (N.fromNative field :: Element))).value_set "hello"
      N.dispatch "input" (object []) field
      textOf ".typed" container `shouldReturn` "hello"
      dispose

    it "throws for a declared match whose selector it does not understand, rather than match nothing" $ do
      (container, dispose) <- mount hovering
      outcome <- try (N.dispatch "click" click =<< one ".link" container)
      case outcome of
        Left (e :: JSError) -> (e.name, ":hover" `T.isInfixOf` e.message) `shouldBe` ("NotSupportedError", True)
        Right () -> fail "the click was dispatched as if nothing matched"
      dispose

    it "lets a handler end the tree while the event is being dispatched" $ do
      (container, dispose) <- mount inbox
      heard <- newIORef []
      mark <- one "button.mark" container
      void (listen' "click" (\_ -> liftIO dispose) mark)
      void (listen heard "click" "container" none container)
      N.dispatch "click" click mark
      readIORef heard >>= (`shouldBe` 1) . length
      N.childNodes container >>= (`shouldBe` 0) . length
  where
    listen' ty act node = do
      ignored <- newIORef []
      listen ignored ty "" act node
#endif
