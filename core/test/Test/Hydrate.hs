{-# LANGUAGE CPP #-}

-- | 'Halogen.VDom.Driver.hydrateUI' in the in-memory document: a component
-- adopts the tree a component rendered there before it, keeping the nodes
-- that match what it renders and repairing the ones that do not.
--
-- Natively there is no HTML parser, so what is adopted is a tree 'runUI'
-- rendered and gave up: rendered, disposed of (which takes it out of its
-- container), put back with its listeners taken off, as a page that arrived
-- with it would have it. The mismatches a parsed page brings with it
-- (comments, nodes the parser made up) are what the browser suite tests.
module Test.Hydrate (spec) where

import Prelude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "hydration" $ pure ()

#else

import Control.Monad.State.Class (modify, put)
import Data.Foldable (for_)
import Data.IORef
import Data.Row (Empty, type (.==))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Static (renderToHtml)
import Halogen.VDom.DOM.Monad (MemDOM, appendChild, createTextNode, insertBefore, runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.Driver (hydrateUI, runUI)
import Halogen.VDom.Types (ElemName (..))
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)
import Unsafe.Coerce (unsafeCoerce)

type Slots = ("child" .== H.Slot H.VoidF Void ())

type Html = H.ComponentHTML () Slots MemDOM

child :: H.Component H.VoidF () Void MemDOM
child =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ()
      , render = \_ -> HH.em_ [HH.text "the child"] :: H.ComponentHTML () Empty MemDOM
      , eval = H.mkEval H.defaultEval
      }

data Count = Increment

-- | Something of everything: a listener, adjacent texts, a keyed list, a
-- memoized part and a child in a slot.
counter :: H.Component H.VoidF () Void MemDOM
counter =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure (0 :: Int)
      , render = \n ->
          HH.div
            [HP.class_ (HH.ClassName "counter")]
            [ HH.button [HE.onClick (const Increment)] [HH.text "+"]
            , HH.p_ [HH.text "count ", HH.text (T.pack (show n))]
            , HH.keyed (ElemName "ul") [] [(k, HH.li_ [HH.text k]) | k <- ["a", "b"]]
            , HH.lazy (\label -> HH.span_ [HH.text label]) "memoized"
            , HH.slot_ "child" () child ()
            ]
            :: H.ComponentHTML Count Slots MemDOM
      , eval = H.mkEval H.defaultEval {H.handleAction = \Increment -> modify (+ 1)}
      }

data Reorder = Reverse

-- | A keyed list that a click turns round.
reorderable :: H.Component H.VoidF () Void MemDOM
reorderable =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ["a", "b", "c"]
      , render = \keys ->
          HH.div_
            [ HH.button [HE.onClick (const Reverse)] [HH.text "reverse"]
            , HH.keyed (ElemName "ul") [] [(k, HH.li_ [HH.text k]) | k <- keys]
            ]
            :: H.ComponentHTML Reorder Empty MemDOM
      , eval = H.mkEval H.defaultEval {H.handleAction = \Reverse -> modify (reverse :: [Text] -> [Text])}
      }

-- | A component that renders this and nothing else.
static :: Html -> H.Component H.VoidF () Void MemDOM
static html =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ()
      , render = \_ -> html
      , eval = H.mkEval H.defaultEval
      }

-- | One that renders one text, then another once it has initialized.
initializing :: H.Component H.VoidF () Void MemDOM
initializing =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure "initial"
      , render = \txt -> HH.p_ [HH.text txt] :: H.ComponentHTML () Empty MemDOM
      , eval = H.mkEval H.defaultEval {H.handleAction = \() -> put "initialized", H.initialize = Just ()}
      }

-- | A container holding what the component rendered there, as a page that
-- arrived with it would: the component is disposed of and its tree put back
-- without the listeners it had.
prerendered :: H.Component H.VoidF () Void MemDOM -> IO N.NativeNode
prerendered component = do
  container <- N.newElement Nothing (ElemName "div")
  H.HalogenSocket {dispose} <- runMemDOM (runUI component () (N.fromNative container))
  rendered <- N.childNodes container
  runMemDOM dispose
  for_ rendered $ \node -> do
    stripListeners node
    runMemDOM (appendChild (N.fromNative node) (N.fromNative container))
  pure container
  where
    stripListeners node = do
      writeIORef node.listeners []
      mapM_ stripListeners =<< N.childNodes node

hydrate :: H.Component H.VoidF () Void MemDOM -> N.NativeNode -> IO ()
hydrate component container = () <$ runMemDOM (hydrateUI component () (N.fromNative container))

-- | Every node under this one, depth first.
descendants :: N.NativeNode -> IO [N.NativeNode]
descendants node = do
  children <- N.childNodes node
  concat <$> traverse (\c -> (c :) <$> descendants c) children

idents :: N.NativeNode -> IO [Int]
idents node = map (.ident) <$> descendants node

markup :: N.NativeNode -> IO Text
markup container = mconcat <$> (traverse N.renderToText =<< N.childNodes container)

elementsNamed :: Text -> N.NativeNode -> IO [N.NativeNode]
elementsNamed name node = filter named <$> descendants node
  where
    named n = n.kind == N.ElementNode Nothing (ElemName name)

textsUnder :: N.NativeNode -> IO [N.NativeNode]
textsUnder node = filter ((== N.TextNode) . (.kind)) <$> descendants node

-- | Run the click listeners an element has, as a click would.
click :: N.NativeNode -> IO ()
click node = do
  listeners <- readIORef node.listeners
  assertWith "the element has a click listener" (any ((== "click") . (.eventType)) listeners)
  for_ [l | l <- listeners, l.eventType == "click"] $ \l -> runMemDOM (l.fire (unsafeCoerce ()))

-- | The component adopts the HTML another one left, and ends with what it
-- renders itself.
repairs :: Html -> Html -> IO (N.NativeNode, [Int])
repairs before after = do
  container <- prerendered (static before)
  idsBefore <- idents container
  hydrate (static after) container
  expected <- renderToHtml (static after) ()
  markup container >>= assertEqual "the tree is the component's" expected
  pure (container, idsBefore)

spec :: Spec
spec = describe "hydration" $ do
  it "adopts every node of a tree that matches, leaving the HTML as it was" $ do
    container <- prerendered counter
    before <- idents container
    html <- markup container
    hydrate counter container
    idents container >>= assertEqual "the same nodes" before
    markup container >>= assertEqual "the same HTML" html

  it "attaches the listeners, and patches the adopted nodes" $ do
    container <- prerendered counter
    before <- idents container
    hydrate counter container
    [button] <- elementsNamed "button" container
    click button
    click button
    [p] <- elementsNamed "p" container
    N.renderToText p >>= assertEqual "the count" "<p>count <!---->2</p>"
    idents container >>= assertEqual "the same nodes" before

  it "hydrates a child in its slot" $ do
    container <- prerendered counter
    [em] <- elementsNamed "em" container
    hydrate counter container
    [em'] <- elementsNamed "em" container
    assertEqual "the child's element" em.ident em'.ident

  it "keeps the keys of an adopted keyed list, moving its nodes on a patch" $ do
    container <- prerendered reorderable
    items <- elementsNamed "li" container
    hydrate reorderable container
    [button] <- elementsNamed "button" container
    click button
    elementsNamed "li" container >>= assertEqual "the same items, turned round" (reverse (map (.ident) items)) . map (.ident)
    [ul] <- elementsNamed "ul" container
    N.renderToText ul >>= assertEqual "the list" "<ul><li>c</li><li>b</li><li>a</li></ul>"

  it "runs initializers, whose renders patch the adopted nodes" $ do
    container <- prerendered initializing
    before <- idents container
    hydrate initializing container
    markup container >>= assertEqual "the initializer's render" "<p>initialized</p>"
    idents container >>= assertEqual "the same nodes" before

  it "replaces an element whose tag differs, keeping its parent" $ do
    (container, before) <- repairs (HH.div_ [HH.span_ [HH.text "x"]]) (HH.div_ [HH.p_ [HH.text "x"]])
    [root] <- N.childNodes container
    assertWith "the div is kept" (root.ident `elem` before)
    [p] <- elementsNamed "p" container
    assertWith "the p is new" (p.ident `notElem` before)

  it "builds a child the tree is missing" $ do
    (container, before) <- repairs (HH.ul_ [HH.li_ [HH.text "a"]]) (HH.ul_ [HH.li_ [HH.text "a"], HH.li_ [HH.text "b"]])
    [a, b] <- elementsNamed "li" container
    assertWith "the first item is kept" (a.ident `elem` before)
    assertWith "the second is new" (b.ident `notElem` before)

  it "removes a child the component does not render" $ do
    (container, before) <- repairs (HH.ul_ [HH.li_ [HH.text "a"], HH.li_ [HH.text "b"]]) (HH.ul_ [HH.li_ [HH.text "a"]])
    [a] <- elementsNamed "li" container
    assertWith "the first item is kept" (a.ident `elem` before)

  it "sets a text that differs, keeping its node" $ do
    (container, before) <- repairs (HH.p_ [HH.text "old"]) (HH.p_ [HH.text "new"])
    [txt] <- textsUnder container
    assertWith "the text node is kept" (txt.ident `elem` before)

  it "builds an empty text the page has no node for, adopting what follows it" $ do
    (container, before) <- repairs (HH.div_ [HH.span_ [HH.text "x"]]) (HH.div_ [HH.text "", HH.span_ [HH.text "x"]])
    [spanEl] <- elementsNamed "span" container
    assertWith "the span is kept" (spanEl.ident `elem` before)

  it "builds a child component the page does not have, where its slot is" $ do
    (container, before) <-
      repairs
        (HH.div_ [HH.span_ [HH.text "before"]])
        (HH.div_ [HH.span_ [HH.text "before"], HH.slot_ "child" () child (), HH.span_ [HH.text "after"]])
    [before', _] <- elementsNamed "span" container
    assertWith "the span is kept" (before'.ident `elem` before)

  it "removes what is around the component's HTML in its container" $ do
    container <- prerendered counter
    [root] <- N.childNodes container
    runMemDOM $ do
      blank <- createTextNode "\n  " (N.fromNative container)
      insertBefore blank (N.fromNative root) (N.fromNative container)
      trailing <- createTextNode "\n" (N.fromNative container)
      appendChild trailing (N.fromNative container)
    html <- N.renderToText root
    hydrate counter container
    [root'] <- N.childNodes container
    assertEqual "the root is kept" root.ident root'.ident
    markup container >>= assertEqual "the HTML" html

  it "replaces a root whose tag differs, and takes out what follows it" $ do
    container <- prerendered (static (HH.span_ [HH.text "x"]))
    runMemDOM $ do
      surplus <- createTextNode "surplus" (N.fromNative container)
      appendChild surplus (N.fromNative container)
    hydrate (static (HH.p_ [HH.text "x"])) container
    markup container >>= assertEqual "the component's root alone" "<p>x</p>"

  it "builds into an empty container, as runUI would" $ do
    container <- N.newElement Nothing (ElemName "div")
    hydrate counter container
    expected <- renderToHtml counter ()
    markup container >>= assertEqual "the component's HTML" expected

#endif
