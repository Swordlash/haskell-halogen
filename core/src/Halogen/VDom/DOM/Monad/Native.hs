{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'MonadDOM' for native GHC, backed by an in-memory document tree.
--
-- There is no browser here, so this module /is/ the DOM: a mutable tree of
-- 'NativeNode' records that reproduces the subset of the DOM the VDom machinery
-- actually calls. That makes two things possible that the JavaScript and wasm
-- backends cannot do — running the whole driver under @cabal test@ with no
-- cross-compiler, and serialising a rendered tree to HTML with 'renderToText'.
--
-- The instance deliberately mirrors the /guards/ the browser backends carry
-- (@parent.lastChild !== child@ and friends), not just their effects: a patch
-- that is a no-op in the browser must be a no-op here too, or these tests would
-- be verifying different behaviour from the one that ships.
--
-- All of @Node@, @Element@, @ParentNode@, @Document@, @HTMLElement@ and
-- @EventTarget@ are newtypes over @Foreign tag@, which is @Any@ on native, and
-- the library freely 'coerce's between them. So they must all share one runtime
-- representation, which is why 'NativeNode' is a single sum-shaped record
-- rather than separate element and text types.
--
-- The instance is an orphan because the class lives in
-- "Halogen.VDom.DOM.Monad.Class". Exactly one backend module is compiled into
-- the package (the cabal file selects on @arch@), so the instances can never
-- overlap.
module Halogen.VDom.DOM.Monad.Native
  ( -- * The in-memory tree
    NativeNode (..)
  , NodeKind (..)
  , PropScalar (..)
  , Listener (..)

    -- * Constructing documents
  , newDocument
  , newElement
  , toNative
  , fromNative

    -- * Inspection
  , childNodes
  , attributeList
  , propertyList
  , listenerTypes

    -- * Serialisation
  , renderToText
  )
where

import Control.Exception.Safe (throwString)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import HPrelude
import Halogen.VDom.DOM.Monad.Class
import Halogen.VDom.DOM.Monad.Mem
import Halogen.VDom.Types
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Internal.Types
import Web.DOM.Internal.Types qualified as DOMTypes
import Web.DOM.ParentNode
import Web.Event.Event
import Web.Event.Internal.Types qualified as EventTypes
import Web.HTML.Common
import Web.HTML.HTMLDocument.ReadyState as ReadyState

-- | What a node is. Text nodes carry their data in 'NativeNode'\'s @content@;
-- elements use @attrs@, @props@, @kids@ and @listeners@.
data NodeKind
  = ElementNode (Maybe Namespace) ElemName
  | TextNode
  | DocumentNode
  deriving (Eq, Show)

-- | What a property looks like once it is on the element.
--
-- This mirrors the browser, not 'PropValue': @propValueToJSVal@ collapses the
-- five 'PropValue' shapes onto three JavaScript ones, with @IntProp@ and
-- @NumProp@ both becoming numbers and @TxtProp@ and @ViaTxtProp@ both becoming
-- strings. Modelling the result rather than the input is what lets
-- @propertyEquals@ reproduce @===@ exactly.
data PropScalar
  = ScalarInt Integer
  | ScalarNum Double
  | ScalarBool Bool
  | ScalarText Text
  deriving (Show)

-- | @===@ semantics: numbers compare numerically across the two integral and
-- fractional spellings, and nothing compares equal across kinds — in
-- particular @1@ and @\"1\"@ do not.
instance Eq PropScalar where
  ScalarInt a == ScalarInt b = a == b
  ScalarNum a == ScalarNum b = a == b
  ScalarInt a == ScalarNum b = fromInteger a == b
  ScalarNum a == ScalarInt b = a == fromInteger b
  ScalarBool a == ScalarBool b = a == b
  ScalarText a == ScalarText b = a == b
  _ == _ = False

data Listener = Listener
  { ident :: Int
  , eventType :: Text
  , fire :: Event -> MemDOM ()
  }

-- | One node in the in-memory document.
--
-- @ident@ gives nodes identity: the browser backends guard their mutations on
-- reference equality, and this is how those guards are reproduced.
data NativeNode = NativeNode
  { ident :: Int
  , kind :: NodeKind
  , content :: IORef Text
  , attrs :: IORef (Map (Maybe Text, Text) Text)
  , props :: IORef (Map Text PropScalar)
  , kids :: IORef [NativeNode]
  , parentRef :: IORef (Maybe NativeNode)
  , listeners :: IORef [Listener]
  }

instance Eq NativeNode where
  a == b = a.ident == b.ident

-- | Every node and listener draws from one counter, so identity is comparable
-- across documents.
{-# NOINLINE identSource #-}
identSource :: IORef Int
identSource = unsafePerformIO (newIORef 0)

nextIdent :: IO Int
nextIdent = atomicModifyIORef' identSource $ \n -> (n + 1, n)

-- | The ambient document. @window@ and @document@ take no arguments, so the
-- class can only be satisfied by a global — but tests that want isolation
-- should build their own root with 'newDocument' and mount into that rather
-- than touching this one.
{-# NOINLINE ambientDocument #-}
ambientDocument :: NativeNode
ambientDocument = unsafePerformIO newDocument

-- | A fresh document containing @\<html>\<head>\<\/head>\<body>\<\/body>\<\/html>@,
-- so 'Halogen.IO.Util.awaitBody' finds a body the way it would in a browser.
newDocument :: IO NativeNode
newDocument = do
  doc <- newNode DocumentNode
  html <- newNode (ElementNode Nothing "html")
  head_ <- newNode (ElementNode Nothing "head")
  body <- newNode (ElementNode Nothing "body")
  attach html doc
  attach head_ html
  attach body html
  pure doc
  where
    attach child parent = do
      modifyIORef' parent.kids (<> [child])
      writeIORef child.parentRef (Just parent)

-- | A detached element, for mounting a component into during a test.
newElement :: Maybe Namespace -> ElemName -> IO NativeNode
newElement ns name = newNode (ElementNode ns name)

newNode :: NodeKind -> IO NativeNode
newNode k = do
  i <- nextIdent
  NativeNode i k
    <$> newIORef ""
    <*> newIORef mempty
    <*> newIORef mempty
    <*> newIORef []
    <*> newIORef Nothing
    <*> newIORef []

-- | Reinterpret one of the DOM newtypes as a node. Sound because they are all
-- @newtype X = X (Foreign X)@ and @Foreign tag = Foreign Any@ on native.
toNative :: a -> NativeNode
toNative = unsafeCoerce

fromNative :: NativeNode -> a
fromNative = unsafeCoerce

-- | @EventListener@ is the one DOM newtype that does not hold a node, so it
-- gets its own pair. Same reasoning: it is @Any@ underneath.
toListener :: DOMTypes.EventListener -> Listener
toListener = unsafeCoerce

toForeignTarget :: a -> EventTypes.EventTarget
toForeignTarget = unsafeCoerce

fromListener :: Listener -> DOMTypes.EventListener
fromListener = unsafeCoerce

--------------------------------------------------------------------------------
-- Tree mutation
--------------------------------------------------------------------------------

-- | Detach from the current parent, if any. The DOM does this implicitly on
-- every insertion, which is what makes a keyed reorder a sequence of moves
-- rather than a sequence of duplications.
detach :: NativeNode -> IO ()
detach node =
  readIORef node.parentRef >>= \case
    Nothing -> pass
    Just parent -> do
      modifyIORef' parent.kids (filter (/= node))
      writeIORef node.parentRef Nothing

-- | @insertBefore child ref parent@, with @Nothing@ meaning append.
insertNative :: NativeNode -> Maybe NativeNode -> NativeNode -> IO ()
insertNative child mref parent = do
  detach child
  siblings <- readIORef parent.kids
  case mref of
    Nothing -> writeIORef parent.kids (siblings <> [child])
    Just ref
      | ref `notElem` siblings ->
          throwString "Halogen.VDom.DOM.Monad.Native: insertBefore reference node is not a child of the parent"
      | otherwise ->
          writeIORef parent.kids $ concatMap (\s -> if s == ref then [child, s] else [s]) siblings
  writeIORef child.parentRef (Just parent)

lastChild :: NativeNode -> IO (Maybe NativeNode)
lastChild parent = lastMay <$> readIORef parent.kids

-- | The node at @ix@ among the parent's children, or 'Nothing' past the end —
-- matching @childNodes.item(ix)@, which returns @null@ out of range.
childAt :: Int -> NativeNode -> IO (Maybe NativeNode)
childAt ix parent = (`atMay` ix) <$> readIORef parent.kids

previousSibling :: NativeNode -> IO (Maybe NativeNode)
previousSibling node = siblingBy (\before _ -> lastMay before) node

nextSiblingNative :: NativeNode -> IO (Maybe NativeNode)
nextSiblingNative = siblingBy (\_ after -> head after)

siblingBy :: ([NativeNode] -> [NativeNode] -> Maybe NativeNode) -> NativeNode -> IO (Maybe NativeNode)
siblingBy pick node =
  readIORef node.parentRef >>= \case
    Nothing -> pure Nothing
    Just parent -> do
      siblings <- readIORef parent.kids
      let (before, after) = break (== node) siblings
      pure $ pick before (drop 1 after)

--------------------------------------------------------------------------------
-- Inspection
--------------------------------------------------------------------------------

childNodes :: NativeNode -> IO [NativeNode]
childNodes node = readIORef node.kids

attributeList :: NativeNode -> IO [((Maybe Text, Text), Text)]
attributeList node = M.toAscList <$> readIORef node.attrs

propertyList :: NativeNode -> IO [(Text, Text)]
propertyList node = map (second scalarText) . M.toAscList <$> readIORef node.props

listenerTypes :: NativeNode -> IO [Text]
listenerTypes node = sort . map (.eventType) <$> readIORef node.listeners

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | Serialise a node and its descendants to HTML.
--
-- Properties are not serialised: the browser sets them on the live object
-- rather than reflecting them into markup, and the same is true here.
renderToText :: NativeNode -> IO Text
renderToText node = case node.kind of
  TextNode -> escapeText <$> readIORef node.content
  DocumentNode -> renderChildren node
  ElementNode _ (ElemName name) -> do
    attributes <- attributeList node
    let open = "<" <> name <> foldMap renderAttribute attributes
    if name `elem` voidElements
      then pure (open <> ">")
      else do
        inner <- renderChildren node
        pure $ open <> ">" <> inner <> "</" <> name <> ">"
  where
    renderChildren n = foldMap identity <$> (traverse renderToText =<< readIORef n.kids)

    renderAttribute ((ns, name), value) =
      " " <> maybe "" (<> ":") ns <> name <> "=\"" <> escapeAttribute value <> "\""

-- | Elements that cannot have children and so take no closing tag.
voidElements :: [Text]
voidElements =
  [ "area"
  , "base"
  , "br"
  , "col"
  , "embed"
  , "hr"
  , "img"
  , "input"
  , "link"
  , "meta"
  , "source"
  , "track"
  , "wbr"
  ]

escapeText :: Text -> Text
escapeText = T.concatMap $ \case
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  c -> T.singleton c

escapeAttribute :: Text -> Text
escapeAttribute = T.concatMap $ \case
  '&' -> "&amp;"
  '<' -> "&lt;"
  '"' -> "&quot;"
  c -> T.singleton c

--------------------------------------------------------------------------------
-- Selectors
--------------------------------------------------------------------------------

-- | Depth-first search for the first node matching a selector.
--
-- Only the three selector shapes the library itself uses are supported: a tag
-- name, @#id@ and @.class@. Anything else finds nothing.
queryNative :: Text -> NativeNode -> IO (Maybe NativeNode)
queryNative selector root = go root
  where
    go node = do
      children <- readIORef node.kids
      firstMatch children

    firstMatch [] = pure Nothing
    firstMatch (c : cs) = do
      hit <- matches c
      if hit then pure (Just c) else go c >>= maybe (firstMatch cs) (pure . Just)

    matches node = case node.kind of
      ElementNode _ (ElemName name) -> case T.uncons selector of
        Just ('#', wanted) -> (== Just wanted) <$> attributeOrProperty "id" node
        Just ('.', wanted) -> do
          classes <- attributeOrProperty "class" node
          pure $ maybe False (elem wanted . T.words) classes
        _ -> pure (name == selector)
      _ -> pure False

    -- @id@ and @class@ reach the DOM as either an attribute or a property
    -- depending on how the VDom node was built, so check both.
    attributeOrProperty name node = do
      attributes <- readIORef node.attrs
      case M.lookup (Nothing, name) attributes of
        Just v -> pure (Just v)
        Nothing -> fmap scalarText . M.lookup name <$> readIORef node.props

--------------------------------------------------------------------------------
-- The instance
--------------------------------------------------------------------------------

-- | The value the element ends up holding, mirroring @propValueToJSVal@.
propScalar :: PropValue a -> PropScalar
propScalar = \case
  IntProp x -> ScalarInt (toInteger x)
  NumProp x -> ScalarNum x
  BoolProp x -> ScalarBool x
  TxtProp x -> ScalarText x
  ViaTxtProp f x -> ScalarText (f x)

-- | How a property reads as text, for the inspection helpers and for the
-- @#id@\/@.class@ selectors. Not used for equality — 'PropScalar' is.
scalarText :: PropScalar -> Text
scalarText = \case
  ScalarInt x -> show x
  ScalarNum x -> show x
  ScalarBool x -> if x then "true" else "false"
  ScalarText x -> x

instance MonadDOM MemDOM where
  type DomNode MemDOM = DOMTypes.Node
  type DomElement MemDOM = DOMTypes.Element
  type DomDocument MemDOM = DOMTypes.Document
  type DomEventListener MemDOM = DOMTypes.EventListener
  type DomEventTarget MemDOM = EventTypes.EventTarget

  elementToNode el = pure (coerce el)
  elementToEventTarget el = pure (toForeignTarget el)

  -- The event type is not known until addEventListener; it is filled in there.
  mkEventListener f = liftIO $ do
    i <- nextIdent
    pure $ fromListener $ Listener {ident = i, eventType = "", fire = f}

  createTextNode txt _ = liftIO $ do
    node <- newNode TextNode
    writeIORef node.content txt
    pure (fromNative node)

  setTextContent txt node = liftIO $ writeIORef (toNative node).content txt

  createElement ns name _ = liftIO $ fromNative <$> newElement ns name

  -- The browser backends guard each of these on reference equality; reproduce
  -- the guards so a no-op patch stays a no-op here too.
  insertBefore inserted sibling parent = liftIO $ do
    let child = toNative inserted
        ref = toNative sibling
    already <- previousSibling ref
    when (already /= Just child) $ insertNative child (Just ref) (toNative parent)

  appendChild child parent = liftIO $ do
    let node = toNative child
        p = toNative parent
    end <- lastChild p
    when (end /= Just node) $ insertNative node Nothing p

  replaceChild newChild oldChild parent = liftIO $ do
    let new = toNative newChild
        old = toNative oldChild
    when (new /= old) $ do
      insertNative new (Just old) (toNative parent)
      detach old

  insertChildIx ix child parent = liftIO $ do
    let node = toNative child
        p = toNative parent
    occupant <- childAt ix p
    when (occupant /= Just node) $ insertNative node occupant p

  removeChild child _ = liftIO $ detach (toNative child)

  parentNode node = liftIO $ fmap fromNative <$> readIORef (toNative node).parentRef
  nextSibling node = liftIO $ fmap fromNative <$> nextSiblingNative (toNative node)

  setAttribute ns (AttrName name) val el =
    liftIO
      $ modifyIORef' (toNative el).attrs
      $ M.insert (unNamespace <$> ns, name) val

  removeAttribute ns (AttrName name) el =
    liftIO
      $ modifyIORef' (toNative el).attrs
      $ M.delete (unNamespace <$> ns, name)

  hasAttribute ns (AttrName name) el =
    liftIO
      $ M.member (unNamespace <$> ns, name)
      <$> readIORef (toNative el).attrs

  setProperty (PropName name) val el =
    liftIO
      $ modifyIORef' (toNative el).props
      $ M.insert name (propScalar val)

  propertyEquals (PropName name) val el =
    liftIO
      $ (== Just (propScalar val))
      . M.lookup name
      <$> readIORef (toNative el).props

  removeProperty (PropName name) el =
    liftIO
      $ modifyIORef' (toNative el).props
      $ M.delete name

  addEventListener (EventType ty) listener target =
    liftIO
      $ modifyIORef' (toNative target).listeners (<> [(toListener listener) {eventType = ty}])

  removeEventListener (EventType ty) listener target = liftIO $ do
    let gone = toListener listener
    modifyIORef' (toNative target).listeners
      $ filter (\l -> not (l.ident == gone.ident && l.eventType == ty))

-- Nothing to wait for: the tree is built synchronously.

-- | The in-memory tree answers the browser-shaped queries too, so that the
-- driver and Halogen.IO.Util compile and can be exercised natively. There is
-- no window here; 'ambientDocument' stands in for one.
instance MonadBrowserDOM MemDOM where
  windowToEventTarget w = pure (coerce w)
  documentToNode d = pure (coerce d)
  window = liftIO $ pure (fromNative ambientDocument)
  document _ = liftIO $ pure (fromNative ambientDocument)
  querySelector (QuerySelector selector) parent =
    liftIO
      $ fmap fromNative
      <$> queryNative selector (toNative parent)
  readyState _ = liftIO $ pure ReadyState.Complete
