{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'MonadDOM' for native GHC and standalone WASI, backed by an in-memory tree.
--
-- There is no browser here, so this module /is/ the DOM: a mutable tree of
-- 'NativeNode' records that reproduces the subset of the DOM the VDom machinery
-- actually calls. It runs the driver without a browser, in native tests or
-- standalone WASI commands, and serialises rendered trees with 'renderToText'.
--
-- The instance deliberately mirrors the /guards/ the browser backends carry
-- (@parent.lastChild !== child@ and friends), not just their effects: a patch
-- that is a no-op in the browser must be a no-op here too, or these tests would
-- be verifying different behaviour from the one that ships.
--
-- All of @Node@, @Element@, @Document@, @HTMLElement@, @Window@ and
-- @EventTarget@ are newtypes over @Foreign tag@, which natively is a value
-- holding what it was made from ("Halogen.JSBits.Value"), and the library
-- freely 'coerce's between them. So they must all hold the same thing,
-- which is why 'NativeNode' is a single sum-shaped record rather than
-- separate element and text types, and each of them holds one
-- ('NodeHandle').
--
-- = Testing a component on it
--
-- A native test mounts a component as a page does, acts on what it
-- rendered as the reader would, waits for the program, and reads the tree:
--
-- > container <- N.newContainer
-- > socket <- runMemDOM (runUI component input (N.fromNative container))
-- > Just button <- N.querySelector "button.mark" container
-- > N.dispatch "click" (object ["button" .= (0 :: Int)]) button
-- > Activity.waitQuiet
-- > unread <- N.querySelectorAll ".notification.unread" container
-- > disabled <- N.attribute "disabled" button
-- > runMemDOM socket.dispose
--
-- * Mount into 'newContainer', in the ambient document, so that what is
--   rendered is connected and an event reaches the listeners a component
--   put on the document or the window; an element made with 'newElement'
--   is in no document.
-- * 'dispatch' an event at a node: it bubbles as in a page, with the node
--   as its target, and the handlers' actions are handled before it returns
--   when the tree's loop is free. What a program then waits for in a
--   @liftIO@ is not done yet: wait for it with "Halogen.Activity"
--   ('Halogen.Activity.waitQuiet', or 'Halogen.Activity.waitIdle').
-- * Read the tree with 'querySelector', 'querySelectorAll', 'textContent'
--   and 'attribute', or serialise it with 'renderToText'; 'performedOn'
--   says what the program did that the tree does not show (a focus, a
--   dialog opened).
--
-- What a component reads of its elements through the @Web.*@ types is
-- answered from the tree too, so the same component code runs here as in
-- a page. There is no layout, no focus, no default action after an event (a
-- test sets a field's @value@ itself before it dispatches @input@), and no
-- capturing phase.
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
  , newContainer
  , ambientDocument
  , NodeHandle (..)

    -- * Events
  , event
  , dispatch
  , dispatchOutcome
  , Dispatched (..)

    -- * Reading the tree
  , querySelector
  , querySelectorAll
  , textContent
  , attribute
  , isConnected

    -- * Inspection
  , Performed (..)
  , performedOn
  , childNodes
  , attributeList
  , propertyList
  , listenerTypes

    -- * Serialisation
  , renderToText
  )
where

import Control.Exception.Safe (throwString)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Dynamic (fromDynamic)
import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import HPrelude hiding (Selector)
import Halogen.JSBits (JSError (..), JSVal, NativeObject (..), jsNull, jsValText, textValue, toJSBool, toJSDouble, toJSInt, withNativeObject)
import Halogen.JSBits.Native (Callback (..), JSVal (JSVal))
import Halogen.VDom.DOM.Monad.Class hiding (querySelector)
import Halogen.VDom.DOM.Monad.Class qualified as Class
import Halogen.VDom.DOM.Monad.Mem
import Halogen.VDom.DOM.Monad.Native.Selector (Refusal (..), Selector, Subject (..), matches, parseSelector)
import Halogen.VDom.Types
import Numeric (floatToDigits)
import System.IO.Unsafe (unsafePerformIO)
import Web.DOM.Internal.Types
import Web.DOM.Internal.Types qualified as DOMTypes
import Web.DOM.ParentNode
import Web.Event.Event hiding (defaultPrevented, target, type_)
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
  , performed :: IORef [Performed]
  -- ^ What the program did to the node through the @Web.*@ types, last
  -- first ('performedOn' gives them in order).
  }

-- | What a program did to a node through the @Web.*@ types (the fields
-- "Halogen.JSBits.Fields" writes), which a browser would have carried out:
-- a method that returns nothing, called by its JavaScript name (@focus@,
-- @showModal@, @scrollIntoView@), and a property assigned (the document's
-- @title@), with the value as text (empty for one that is not). The
-- in-memory DOM carries none of them out; it notes them, so that a test
-- sees that a component focused an element or opened a dialog.
data Performed
  = Called Text
  | Assigned Text Text
  deriving (Eq, Show)

-- | What the program did to the node through the @Web.*@ types, in order.
performedOn :: NativeNode -> IO [Performed]
performedOn node = reverse <$> readIORef node.performed

instance Eq NativeNode where
  a == b = a.ident == b.ident

-- | Every node and listener draws from one counter, so identity is comparable
-- across documents.
{-# NOINLINE identSource #-}
identSource :: IORef Int
identSource = unsafePerformIO (newIORef 0)

nextIdent :: IO Int
nextIdent = atomicModifyIORef' identSource $ \n -> (n + 1, n)

-- | The ambient document, which is also the window: what
-- 'Halogen.VDom.DOM.Monad.window' and 'Halogen.VDom.DOM.Monad.document'
-- give. They take no arguments, so the class can only be satisfied by a
-- global. A component that listens on the document or the window listens
-- on it, and 'newContainer' makes the elements to mount into in it; a test
-- that wants a tree of its own, which no component's document listener
-- hears, builds a root with 'newDocument' instead.
{-# NOINLINE ambientDocument #-}
ambientDocument :: NativeNode
ambientDocument = unsafePerformIO newDocument

-- | What the in-memory DOM keeps instead of the browser's stores.
--
-- One global, for the same reason 'ambientDocument' is one: the class hands a
-- backend a 'StorageKind' and nothing else to key on. It is never emptied
-- between components, which is the point — that is how a test watches
-- something persist across a remount — so a test that cares about starting
-- from nothing should 'Web.Storage.Storage.clear' first.
{-# NOINLINE ambientStorage #-}
ambientStorage :: IORef (Map StorageKind (Map Text Text))
ambientStorage = unsafePerformIO (newIORef M.empty)

-- | One of the two stores, empty until something is written to it.
store :: StorageKind -> Map StorageKind (Map Text Text) -> Map Text Text
store kind = M.findWithDefault M.empty kind

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

-- | A detached element: in no document, so not connected ('isConnected'),
-- and an event dispatched inside it reaches no document's listeners. For a
-- test of what is rendered and nothing else; 'newContainer' is the one to
-- mount a component that reads the page into.
newElement :: Maybe Namespace -> ElemName -> IO NativeNode
newElement ns name = newNode (ElementNode ns name)

-- | A new @div@ at the end of the ambient document's @body@, to mount a
-- component into for a test as a page would have it: what is rendered in it
-- is connected ('isConnected', which a popup asks before it acts), and an
-- event 'dispatch'ed inside it bubbles up to the document and the window,
-- where a component's 'Halogen.Query.Event.eventListener' on the document,
-- or a "Halogen.Query.Event.Window" emitter, hears it.
--
-- The ambient document is the one 'Halogen.VDom.DOM.Monad.window' and
-- 'Halogen.VDom.DOM.Monad.document' give, one for the whole program: every
-- container is in it, every component's document listeners are on it, and
-- an event that reaches it reaches all of them, as in one page. A test that
-- mounts components side by side whose document listeners must not hear
-- each other's events mounts them one after another, and disposes of each
-- (taking its container out with the DOM's @remove@ if it likes) before
-- the next.
newContainer :: IO NativeNode
newContainer = do
  container <- newElement Nothing "div"
  body <-
    documentChild "body" ambientDocument
      >>= maybe (throwString "Halogen.VDom.DOM.Monad.Native.newContainer: the ambient document has no body") pure
  insertNative container Nothing body
  pure container

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
    <*> newIORef []

-- | The value every handle of a node holds: the node, answering for itself
-- when the program uses a member of it through the @Web.*@ types
-- ("Halogen.JSBits.Fields"), and when a handler reads a property of it
-- ("Data.Foreign"'s @readProp@), as a page's node would, from the tree:
--
-- * __calls__ are noted ('Performed'), and the ones that change the tree
--   are carried out too: @setAttribute@, @removeAttribute@, @remove@
--   (@focus@, @showModal@, @scrollIntoView@ and the rest are only noted:
--   there is no focus, no layout and no top layer here);
-- * __assignments__ are noted and carried out: @textContent@ replaces what
--   is in an element, any other property is set as the reconciler sets one
--   (a text, a boolean or a number; a value of any other kind is only
--   noted);
-- * __methods with a result__ answer from the tree: @getAttribute@,
--   @hasAttribute@, @closest@, @querySelector@ (the selectors
--   "Halogen.VDom.DOM.Monad.Native.Selector" understands, and an exception
--   for any other), @contains@, @appendChild@, and the document's
--   @getElementById@;
-- * __reads__ answer from the tree: @isConnected@ ('isConnected'),
--   @textContent@, @tagName@, @nodeName@, @nodeType@, @parentElement@,
--   @nextElementSibling@, the document's @body@ and @head@, a property the
--   node has, and one that reflects an attribute ('attribute' says which);
-- * a checked cast ('Halogen.Query.getHTMLElementRef') finds the DOM
--   classes the node is an instance of, and the document is its own
--   @document@ (the in-memory document is the window too).
--
-- Anything else is declined, and has its inert answer: the active
-- element, a node's size and position, the page's address.
handleOf :: NativeNode -> JSVal
handleOf node = withNativeObject object (toForeign node)
  where
    object =
      NativeObject
        { call = \name arguments -> do
            noting (Called name)
            carryOut node name arguments
        , invoke = answerCall node
        , set = \name value -> do
            noting (Assigned name (jsValText value))
            assign node name value
        , get = \name -> case node.kind of
            DocumentNode | name == "document" -> Just (handleOf node)
            _ -> Nothing
        , live = readMember node
        , instanceOf = \klass -> case node.kind of
            ElementNode ns name -> elementInstanceOf ns name klass
            TextNode -> klass `elem` ["EventTarget", "Node", "CharacterData", "Text"]
            DocumentNode -> klass `elem` ["EventTarget", "Node", "Document", "HTMLDocument", "Window"]
        }
    noting done = atomicModifyIORef' node.performed (\before -> (done : before, ()))

-- | A method that returns nothing, carried out where it changes the tree.
carryOut :: NativeNode -> Text -> [JSVal] -> IO ()
carryOut node name arguments = case (node.kind, name, arguments) of
  (ElementNode {}, "setAttribute", [attr, value]) ->
    modifyIORef' node.attrs (M.insert (Nothing, attributeName node (jsValText attr)) (jsValText value))
  (ElementNode {}, "removeAttribute", [attr]) -> do
    let wanted = attributeName node (jsValText attr)
    modifyIORef' node.attrs (M.filterWithKey (\(_, n) _ -> n /= wanted))
  (_, "remove", []) -> detach node
  _ -> pass

-- | An assignment to a property, carried out.
assign :: NativeNode -> Text -> JSVal -> IO ()
assign node name value = case node.kind of
  TextNode | name `elem` ["textContent", "data", "nodeValue"] -> writeIORef node.content (jsValText value)
  ElementNode {} | name == "textContent" -> replaceText node (jsValText value)
  _ -> for_ (scalarOf value) $ \scalar -> modifyIORef' node.props (M.insert name scalar)
  where
    scalarOf (JSVal held) =
      (ScalarText <$> fromDynamic held)
        <|> (ScalarBool <$> fromDynamic held)
        <|> (ScalarInt . toInteger <$> (fromDynamic held :: Maybe Int))
        <|> (ScalarNum <$> fromDynamic held)

-- | What a method with a result answers, from the tree: 'Nothing' for a
-- method it does not answer, @'Just' 'jsNull'@ for an answer of @null@.
answerCall :: NativeNode -> Text -> [JSVal] -> IO (Maybe JSVal)
answerCall node name arguments = case (node.kind, name, arguments) of
  (ElementNode {}, "getAttribute", [attr]) -> Just . maybe jsNull textValue <$> attribute (jsValText attr) node
  (ElementNode {}, "hasAttribute", [attr]) -> Just . toJSBool . isJust <$> attribute (jsValText attr) node
  (ElementNode {}, "closest", [selector]) -> Just . maybe jsNull handleOf <$> closest (jsValText selector) node
  (_, "querySelector", [selector]) | node.kind /= TextNode -> Just . maybe jsNull handleOf <$> querySelector (jsValText selector) node
  (DocumentNode, "getElementById", [wanted]) -> do
    let byId e = (== Just (jsValText wanted)) <$> attribute "id" e
    Just . maybe jsNull handleOf . head <$> (filterM byId =<< descendantElements node)
  (_, "contains", [other]) -> Just . toJSBool <$> maybe (pure False) (contains node) (nodeOf other)
  (_, "appendChild", [child]) | node.kind /= TextNode -> case nodeOf child of
    Just c -> do
      insertNative c Nothing node
      pure (Just (handleOf c))
    Nothing -> pure Nothing
  _ -> pure Nothing

-- | The node a value is a handle of, if it is one.
nodeOf :: JSVal -> Maybe NativeNode
nodeOf (JSVal held) = fromDynamic held

-- | A property's value, read from the tree: 'Nothing' for one the
-- in-memory DOM does not keep (see 'handleOf').
readMember :: NativeNode -> Text -> IO (Maybe JSVal)
readMember node name = case (node.kind, name) of
  (_, "isConnected") -> Just . toJSBool <$> isConnected node
  (_, "nodeType") -> pure . Just . toJSInt $ case node.kind of
    ElementNode {} -> 1
    TextNode -> 3
    DocumentNode -> 9
  (_, "parentElement") -> Just . maybe jsNull handleOf <$> parentElement node
  (_, "parentNode") -> Just . maybe jsNull handleOf <$> readIORef node.parentRef
  (DocumentNode, "body") -> Just . maybe jsNull handleOf <$> documentChild "body" node
  (DocumentNode, "head") -> Just . maybe jsNull handleOf <$> documentChild "head" node
  (DocumentNode, "nodeName") -> pure (Just (textValue "#document"))
  (DocumentNode, _) -> fmap scalarValue . M.lookup name <$> readIORef node.props
  (TextNode, _)
    | name `elem` ["textContent", "data", "nodeValue", "wholeText"] -> Just . textValue <$> readIORef node.content
    | name == "nodeName" -> pure (Just (textValue "#text"))
    | otherwise -> pure Nothing
  (ElementNode ns (ElemName tag), _)
    | name `elem` ["tagName", "nodeName"] -> pure (Just (textValue (if isNothing (htmlIsNothing ns) then T.toUpper tag else tag)))
    | name == "localName" -> pure (Just (textValue tag))
    | name == "textContent" -> Just . textValue <$> textContent node
    | name == "nextElementSibling" -> Just . maybe jsNull handleOf <$> nextElement node
    | otherwise -> property name node

-- | A property of an element: the one the reconciler (or an assignment)
-- set, or else the one that reflects an attribute, from the attribute.
property :: Text -> NativeNode -> IO (Maybe JSVal)
property name node = do
  props <- readIORef node.props
  case M.lookup name props of
    Just v -> pure (Just (scalarValue v))
    Nothing -> case M.lookup name reflections of
      Just (Plain attr) -> Just . textValue . fromMaybe "" <$> attribute attr node
      Just (Presence attr) -> Just . toJSBool . isJust <$> writtenAttribute attr node
      _ -> pure Nothing

-- | A property's value as a value: what a read of it gives.
scalarValue :: PropScalar -> JSVal
scalarValue = \case
  ScalarText t -> textValue t
  ScalarBool b -> toJSBool b
  ScalarInt i -> toJSInt (fromInteger i)
  ScalarNum d -> toJSDouble d

-- | The DOM's handles that stand for a node: each holds the 'NativeNode'
-- in its value, so taking one apart finds the node it was made from, and a
-- handle that holds anything else (a stub's result) is reported as the
-- mistake it is rather than read as a node.
class NodeHandle a where
  toNative :: a -> NativeNode
  fromNative :: NativeNode -> a

instance NodeHandle Node where
  toNative (Node value) = unsafeFromForeign value
  fromNative = Node . handleOf

instance NodeHandle Element where
  toNative (Element value) = unsafeFromForeign value
  fromNative = Element . handleOf

instance NodeHandle HTMLElement where
  toNative (HTMLElement value) = unsafeFromForeign value
  fromNative = HTMLElement . handleOf

instance NodeHandle Document where
  toNative (Document value) = unsafeFromForeign value
  fromNative = Document . handleOf

instance NodeHandle HTMLDocument where
  toNative (HTMLDocument value) = unsafeFromForeign value
  fromNative = HTMLDocument . handleOf

instance NodeHandle Window where
  toNative (Window value) = unsafeFromForeign value
  fromNative = Window . handleOf

instance NodeHandle EventTypes.EventTarget where
  toNative (EventTypes.EventTarget value) = unsafeFromForeign value
  fromNative = EventTypes.EventTarget . handleOf

-- | @EventListener@ is the one DOM handle that does not hold a node: its
-- callback's value holds the 'Listener', which is where its identity is.
toListener :: DOMTypes.EventListener -> Listener
toListener (DOMTypes.EventListener (Callback value)) = unsafeFromForeign value

fromListener :: Listener -> DOMTypes.EventListener
fromListener = DOMTypes.EventListener . Callback . toForeign

-- | An event as a browser would hand it to a listener, with the properties
-- the JSON object has: what a test fires, so that a handler reading the
-- event ('Halogen.HTML.Events.onValueInput' reads
-- @currentTarget.value@) finds what it reads. Any JSON will do; one that
-- is not an object has no properties.
event :: Aeson.Value -> Event
event = Event . toForeign

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

-- | Serialise a node and its descendants to HTML: what the browser would
-- have to parse to build the same tree, which is what a page rendered on a
-- server ("Halogen.Static") sends ahead of its program.
--
-- __Properties.__ The reconciler sets most of what a component says about
-- an element as properties, which the browser keeps on the live object
-- rather than in markup, so they are written out here as the attribute
-- each one reflects, where HTML has one:
--
-- * renamed ones under their attribute's name: @className@ as @class@,
--   @htmlFor@ as @for@, @tabIndex@ as @tabindex@, @readOnly@ as @readonly@
--   and so on;
-- * boolean ones (@checked@, @disabled@, @selected@, @hidden@, @required@,
--   @multiple@, …) as an attribute with no value when the property is
--   true, and not at all when it is false, as JavaScript's truthiness would
--   have it for a value that is not a boolean;
-- * @draggable@ and @spellcheck@ as @\"true\"@ or @\"false\"@, and
--   @translate@ as @\"yes\"@ or @\"no\"@, which is how those attributes are
--   spelled;
-- * @value@ as the @value@ attribute, except on a @textarea@, whose value
--   is written as its text instead, and on a @select@, where the first
--   @option@ with that value is marked @selected@ (the others are not), as
--   setting the property does in a browser;
-- * the remaining reflected ones (@id@, @href@, @src@, @type@, @name@,
--   @placeholder@, @min@, @max@, …) under their own name, a number spelled
--   as JavaScript spells it (@1@, not @1.0@).
--
-- A property and an attribute that land on the same name are written once,
-- with the property's value: for @value@, @checked@ and @selected@ the
-- property is what the element holds now and the attribute only what it
-- started with. A property HTML has no attribute for (@selectedIndex@,
-- @indeterminate@, @valueAsNumber@, one a custom element defines, …) is
-- left out: markup cannot carry it, so a component that needs one in the
-- prerendered page sets the attribute instead.
--
-- __Namespaces.__ The HTML parser takes an element's namespace from its
-- tag, entering SVG at @svg@ and MathML at @math@, and never from an
-- @xmlns@ attribute, so none is written for them (as the browser's own
-- serialiser writes none); one the tree holds is written as any attribute
-- is. Attributes in the XLink, XML and XMLNS namespaces get the prefix the
-- HTML parser expects.
--
-- __Text.__ Two text nodes next to each other are separated by an empty
-- comment, @\<!---->@: the parser would merge them into one, and a client
-- that adopts the page has to know where the boundary was. Text is escaped,
-- a carriage return as @&#13;@ (the parser turns a bare one, or one before a
-- line feed, into a line feed), except inside @script@ and @style@ and the
-- other raw-text elements, whose text the parser does not unescape. There
-- adjacent text nodes are written together, since a comment would be part
-- of the text (and @\<!--@ starts one in a script), and a carriage return
-- is written as it is and read back as a line feed, which nothing in the
-- markup can prevent. A newline that starts a @textarea@ or a @pre@ is doubled, because
-- the parser drops the first.
--
-- __What HTML cannot say.__ Some trees no markup parses back into. Those
-- are refused, with an error that names what is wrong, so that a prerender
-- fails rather than ship a page other than the component's:
--
-- * a @plaintext@ element, whose text the parser never ends: everything
--   after it, the rest of the page included, would be its text;
-- * a raw-text element (@script@, @style@, @xmp@, @iframe@, @noembed@,
--   @noframes@) whose text holds the element's own end tag, which would end
--   it early, and which no escape can spell there: a script that needs
--   @\<\/script@ in a string writes it @\<\\\/script@ itself, as authors
--   do, since only it knows the text is a string;
-- * U+0000 in text or an attribute, which the parser reads as U+FFFD;
-- * an element in a namespace other than HTML's, SVG's and MathML's, the
--   only ones the parser puts an element in;
-- * an element the parser would not make where it is: an SVG element
--   outside an @svg@ (a @circle@ in a @div@), a MathML one outside a
--   @math@, or an HTML element inside SVG or MathML other than at an
--   integration point (@foreignObject@, @desc@ or @title@; @mi@, @mo@,
--   @mn@, @ms@, @mtext@ or @annotation-xml@), where the parser would make it
--   foreign or break out of the foreign content.
--
-- Others parse into a tree that differs from the component's only until the
-- program has started and set its state, and are written as they are:
--
-- * a carriage return in raw text comes back as a line feed;
-- * a @select@ (without @multiple@) whose value matches none of its options
--   has nothing marked @selected@, and the parser selects the first option,
--   which the page shows until the program sets the value (HTML has no way
--   to say \"none\"; with @multiple@, no @selected@ attribute means none);
-- * a @tr@ straight under a @table@ is put in a @tbody@ the parser makes up,
--   a node the component's tree does not have: write the @tbody@.
--
-- Neither list is the whole of it. The markup is written the way the
-- browser's own fragment serialisation writes it, with properties
-- reflected; the parser's tree-construction rules are not applied, so a
-- tree the DOM allows but the parser would build differently comes back
-- different, and a client adopting the page has to treat that as any other
-- mismatch. The common cases: an empty text node (@HH.text \"\"@), for
-- which the parser makes no node; a @p@ holding a block element, an @li@,
-- @dt@, @dd@ or @option@ that the next one ends, text or an element other
-- than a row's straight inside a @table@, which is moved out in front of
-- it, an element other than @option@ or @optgroup@ inside a @select@, an
-- @a@ inside an @a@ or a @button@ inside a @button@, and an element or
-- attribute name with capitals outside SVG and MathML, which the parser
-- lowercases.
renderToText :: NativeNode -> IO Text
renderToText node = do
  context <- maybe (pure topContext) (contextWithin topContext) =<< readIORef node.parentRef
  -- Materialise only the complete tree: doing this at each element copies
  -- its descendants again at every ancestor.
  TL.toStrict . TB.toLazyText <$> renderIn context node

-- | What serialising a node depends on besides the node: where it sits.
data Context = Context
  { parent :: Maybe (Maybe Text, Text)
  -- ^ The element the node is a child of, if any: its namespace
  -- ('Nothing' for HTML) and its name.
  , element :: Maybe Text
  -- ^ The HTML element the node is a child of, if it is one.
  , choice :: Maybe (Text, IORef Bool)
  -- ^ Inside a @select@ whose @value@ property is set: that value, and
  -- whether an @option@ has already taken it.
  }

topContext :: Context
topContext = Context {parent = Nothing, element = Nothing, choice = Nothing}

-- | The context for the children of a node that is itself in this context.
contextWithin :: Context -> NativeNode -> IO Context
contextWithin outer node = case node.kind of
  ElementNode ns (ElemName name) -> do
    let namespace = htmlIsNothing ns
        element = if isNothing namespace then Just name else Nothing
    choice <- case element of
      Just "select" ->
        M.lookup "value" <$> readIORef node.props >>= \case
          Just v -> Just . (scalarAttribute v,) <$> newIORef False
          Nothing -> pure Nothing
      Just "optgroup" -> pure outer.choice
      _ -> pure Nothing
    pure Context {parent = Just (namespace, name), element, choice}
  _ -> pure topContext

renderIn :: Context -> NativeNode -> IO TB.Builder
renderIn context node = case node.kind of
  TextNode -> do
    txt <- readIORef node.content
    refuseNul "a text" txt
    case context.element of
      Just raw | raw `elem` rawTextElements -> TB.fromText <$> rawText raw txt
      _ -> pure (escapeText txt)
  DocumentNode -> renderChildren topContext node
  ElementNode ns (ElemName name) -> do
    let namespace = htmlIsNothing ns
        html = isNothing namespace
    for_ namespace $ \uri ->
      unless (uri `elem` [svgNamespace, mathMLNamespace])
        $ throwString
        $ "renderToText: <"
        <> toS name
        <> "> is in the namespace "
        <> toS uri
        <> ", which HTML cannot put an element in (only SVG's and MathML's)"
    unless (parsedHere context namespace name)
      $ throwString
      $ "renderToText: the parser would not put <"
      <> toS name
      <> ">"
      <> maybe "" (\uri -> " (in " <> toS uri <> ")") namespace
      <> " "
      <> maybe "at the top" (\(pns, pn) -> "in <" <> toS pn <> ">" <> maybe "" (\uri -> " (in " <> toS uri <> ")") pns) context.parent
      <> ": an SVG element goes in an <svg>, a MathML one in a <math>, and HTML in HTML or an integration point"
    when (html && name == "plaintext")
      $ throwString "renderToText: HTML cannot end a <plaintext>, so it cannot be written: everything after it would be its text"
    attributes <- readIORef node.attrs
    properties <- readIORef node.props
    picked <- case (context.choice, html && name == "option") of
      (Just (wanted, taken), True) -> do
        mine <- (== wanted) <$> optionValue node attributes properties
        already <- readIORef taken
        when (mine && not already) $ writeIORef taken True
        pure $ Just (mine && not already)
      _ -> pure Nothing
    let ownsValue = html && name `elem` ["select", "textarea"]
        reflected =
          [ reflect r v
          | (prop, v) <- M.toAscList properties
          , not (ownsValue && prop == "value")
          , Just r <- [M.lookup prop reflections]
          ]
        merged =
          foldl' (\acc (key, v) -> M.alter (const v) (Nothing, key) acc) (Valued <$> attributes)
            $ reflected
            <> foldMap (\mine -> [("selected", if mine then Just Bare else Nothing)]) picked
        open = "<" <> TB.fromText name <> foldMap renderAttribute (M.toAscList merged) <> ">"
    for_ (M.toList merged) $ \case
      ((_, attr), Valued v) -> refuseNul ("the attribute " <> attr) v
      _ -> pass
    if html && name `elem` voidElements
      then pure open
      else do
        inner <- case (html && name == "textarea", M.lookup "value" properties) of
          (True, Just v) -> do
            refuseNul "a textarea's value" (scalarAttribute v)
            pure (escapeText (scalarAttribute v))
          _ -> do
            inside <- contextWithin context node
            renderChildren inside node
        let kept
              | html && name `elem` ["pre", "textarea", "listing"] =
                  -- Inspect the prefix without making a strict copy of the
                  -- subtree, and reuse the chunks when writing it out.
                  let txt = TB.toLazyText inner
                   in TB.fromLazyText (if "\n" `TL.isPrefixOf` txt then "\n" <> txt else txt)
              | otherwise = inner
        pure $ open <> kept <> "</" <> TB.fromText name <> ">"

-- | The children in turn, with an empty comment between two text nodes,
-- except in a raw-text element, where the comment would be text.
renderChildren :: Context -> NativeNode -> IO TB.Builder
renderChildren context parent = do
  children <- readIORef parent.kids
  rendered <- traverse (renderIn context) children
  let isText c = c.kind == TextNode
      raw = maybe False (`elem` rawTextElements) context.element
      separate a b = if not raw && isText a && isText b then "<!---->" else ""
      separators = "" : zipWith separate children (drop 1 children)
  pure $ mconcat (zipWith (<>) separators rendered)

-- | Whether the HTML parser, reading this element's tag where it is, would
-- make an element in its namespace. It enters SVG only at an @svg@ tag and
-- MathML only at a @math@ tag, from HTML or an integration point, and
-- leaves them only at the integration points (SVG's @foreignObject@,
-- @desc@ and @title@, MathML's @mi@, @mo@, @mn@, @ms@, @mtext@ and
-- @annotation-xml@): anywhere else an element is made in the namespace it
-- is in, or the foreign content is broken out of.
parsedHere :: Context -> Maybe Text -> Text -> Bool
parsedHere context namespace name = case namespace of
  Nothing -> inHtml
  Just uri
    | uri == svgNamespace -> (name == "svg" && (inHtml || annotation)) || inForeign svgNamespace
    | uri == mathMLNamespace -> (name == "math" && inHtml) || inForeign mathMLNamespace
    | otherwise -> False
  where
    inHtml = case context.parent of
      Nothing -> True
      Just (Nothing, _) -> True
      Just (Just pns, pn) -> integrationPoint pns pn
    inForeign uri = case context.parent of
      Just (Just pns, pn) -> pns == uri && not (integrationPoint pns pn)
      _ -> False
    annotation = context.parent == Just (Just mathMLNamespace, "annotation-xml")
    integrationPoint pns pn
      | pns == svgNamespace = pn `elem` ["foreignObject", "desc", "title"]
      | pns == mathMLNamespace = pn `elem` ["mi", "mo", "mn", "ms", "mtext", "annotation-xml"]
      | otherwise = False

-- | The tokenizer turns U+0000 into U+FFFD, and no character reference
-- spells it, so no markup holds one.
refuseNul :: Text -> Text -> IO ()
refuseNul what txt =
  when (T.any (== '\0') txt)
    $ throwString
    $ "renderToText: "
    <> toS what
    <> " holds U+0000, which the HTML parser reads as U+FFFD and no markup can write"

-- | HTML elements are created with no namespace and parsed into the XHTML
-- one: the two are the same place.
htmlIsNothing :: Maybe Namespace -> Maybe Text
htmlIsNothing = \case
  Just (Namespace uri) | uri /= xhtmlNamespace -> Just uri
  _ -> Nothing

xhtmlNamespace :: Text
xhtmlNamespace = "http://www.w3.org/1999/xhtml"

svgNamespace :: Text
svgNamespace = "http://www.w3.org/2000/svg"

mathMLNamespace :: Text
mathMLNamespace = "http://www.w3.org/1998/Math/MathML"

-- | What an attribute holds once serialised: a boolean one has no value.
data AttributeValue = Bare | Valued Text

renderAttribute :: ((Maybe Text, Text), AttributeValue) -> TB.Builder
renderAttribute ((ns, name), v) =
  " " <> TB.fromText qualified <> case v of
    Bare -> ""
    Valued txt -> "=\"" <> escapeAttribute txt <> "\""
  where
    -- An attribute set with a namespace is set under a qualified name, which
    -- usually carries its prefix already (@xlink:href@); the three
    -- namespaces the HTML serialiser knows get theirs if it does not.
    qualified = case ns of
      Just "http://www.w3.org/1999/xlink" -> prefixed "xlink"
      Just "http://www.w3.org/XML/1998/namespace" -> prefixed "xml"
      Just "http://www.w3.org/2000/xmlns/" | name /= "xmlns" -> prefixed "xmlns"
      _ -> name
    prefixed prefix
      | ":" `T.isInfixOf` name = name
      | otherwise = prefix <> ":" <> name

-- | How an option is matched against its select's value: its @value@, or
-- failing that its text with ASCII white space stripped and collapsed, as
-- in a browser. Only ASCII white space: a non-breaking space is part of the
-- value.
optionValue :: NativeNode -> Map (Maybe Text, Text) Text -> Map Text PropScalar -> IO Text
optionValue node attributes properties =
  case (scalarAttribute <$> M.lookup "value" properties) <|> M.lookup (Nothing, "value") attributes of
    Just v -> pure v
    Nothing -> T.intercalate " " . filter (not . T.null) . T.split (`elem` asciiWhitespace) <$> textOf node
  where
    asciiWhitespace = [' ', '\t', '\n', '\f', '\r'] :: [Char]
    textOf n = case n.kind of
      TextNode -> readIORef n.content
      _ -> mconcat <$> (traverse textOf =<< readIORef n.kids)

-- | How a property reflects into markup; see 'renderToText'.
data Reflection
  = -- | The value as text, under this attribute name.
    Plain Text
  | -- | A boolean attribute: present, with no value, when the property is
    -- truthy.
    Presence Text
  | -- | An enumerated attribute spelled @true@ or @false@.
    TrueFalse Text
  | -- | An enumerated attribute spelled @yes@ or @no@.
    YesNo Text

-- | The attribute a reflection writes, or 'Nothing' for none at all.
reflect :: Reflection -> PropScalar -> (Text, Maybe AttributeValue)
reflect r v = case r of
  Plain name -> (name, Just (Valued (scalarAttribute v)))
  Presence name -> (name, if truthy v then Just Bare else Nothing)
  TrueFalse name -> (name, Just (Valued (spell "true" "false")))
  YesNo name -> (name, Just (Valued (spell "yes" "no")))
  where
    spell yes no = case v of
      ScalarBool b -> if b then yes else no
      _ -> scalarAttribute v

-- | JavaScript's truthiness, for a boolean property given something else.
truthy :: PropScalar -> Bool
truthy = \case
  ScalarBool b -> b
  ScalarText t -> not (T.null t)
  ScalarInt i -> i /= 0
  ScalarNum d -> not (d == 0 || isNaN d)

-- | A property's value as the attribute it is reflected into would hold it:
-- what JavaScript's @String()@ makes of it.
scalarAttribute :: PropScalar -> Text
scalarAttribute = \case
  ScalarText t -> t
  ScalarInt i -> show i
  ScalarBool b -> if b then "true" else "false"
  ScalarNum d
    | isNaN d -> "NaN"
    | isInfinite d -> if d > 0 then "Infinity" else "-Infinity"
    | otherwise -> numberToString d

-- | A finite number as JavaScript's @Number::toString@ writes it: the
-- shortest digits that read back as the same number ('floatToDigits'
-- gives exactly those), in fixed notation for a decimal exponent from -6
-- up to 21, and otherwise as @d.ddde±n@. Zero, negative or not, is @0@.
numberToString :: Double -> Text
numberToString d
  | d == 0 = "0"
  | d < 0 = "-" <> numberToString (negate d)
  | k <= n && n <= 21 = digits <> T.replicate (n - k) "0"
  | 0 < n && n <= 21 = T.take n digits <> "." <> T.drop n digits
  | -6 < n && n <= 0 = "0." <> T.replicate (negate n) "0" <> digits
  | otherwise = T.take 1 digits <> fraction <> "e" <> (if n - 1 < 0 then "-" else "+") <> show (abs (n - 1))
  where
    (ds, n) = floatToDigits 10 d
    digits = T.pack (map intToDigit ds)
    k = length ds
    fraction = if k == 1 then "" else "." <> T.drop 1 digits

-- | The properties that have an HTML attribute, and how each is written.
reflections :: Map Text Reflection
reflections =
  M.fromList
    $ [(p, Plain p) | p <- sameName]
    <> [(p, Plain a) | (p, a) <- renamed]
    <> [(p, Presence (T.toLower p)) | p <- booleans]
    <> [("draggable", TrueFalse "draggable"), ("spellcheck", TrueFalse "spellcheck"), ("translate", YesNo "translate")]
  where
    sameName =
      concatMap
        T.words
        [ "abbr accept action allow alt autocapitalize autocomplete charset cite"
        , "cols content coords decoding dir download enctype headers height high"
        , "href hreflang id integrity kind label lang loading low max media"
        , "method min name nonce optimum pattern ping placeholder popover poster"
        , "preload rel role rows sandbox scope shape size sizes slot span src"
        , "srcdoc srclang srcset start step style target title type value width"
        , "wrap"
        ]
    renamed =
      [ ("acceptCharset", "accept-charset")
      , ("accessKey", "accesskey")
      , ("className", "class")
      , ("colSpan", "colspan")
      , ("contentEditable", "contenteditable")
      , ("crossOrigin", "crossorigin")
      , ("dateTime", "datetime")
      , ("dirName", "dirname")
      , ("enterKeyHint", "enterkeyhint")
      , ("fetchPriority", "fetchpriority")
      , ("formAction", "formaction")
      , ("formEnctype", "formenctype")
      , ("formMethod", "formmethod")
      , ("formTarget", "formtarget")
      , ("htmlFor", "for")
      , ("httpEquiv", "http-equiv")
      , ("inputMode", "inputmode")
      , ("maxLength", "maxlength")
      , ("minLength", "minlength")
      , ("referrerPolicy", "referrerpolicy")
      , ("rowSpan", "rowspan")
      , ("tabIndex", "tabindex")
      , ("useMap", "usemap")
      ]
    booleans =
      concatMap
        T.words
        [ "allowFullscreen async autofocus autoplay checked controls default"
        , "defer disabled formNoValidate hidden inert isMap loop multiple muted"
        , "noModule noValidate open playsInline readOnly required reversed"
        , "selected"
        ]

-- | Elements that cannot have children and so take no closing tag: the
-- ones HTML's fragment serialisation algorithm lists, obsolete ones
-- included.
voidElements :: [Text]
voidElements =
  [ "area"
  , "base"
  , "basefont"
  , "bgsound"
  , "br"
  , "col"
  , "embed"
  , "frame"
  , "hr"
  , "img"
  , "input"
  , "keygen"
  , "link"
  , "meta"
  , "param"
  , "source"
  , "track"
  , "wbr"
  ]

-- | Elements whose text the parser takes as it is, entities and all.
rawTextElements :: [Text]
rawTextElements = ["iframe", "noembed", "noframes", "plaintext", "script", "style", "xmp"]

-- | The text of a raw-text element, which cannot be escaped: only an end
-- tag of the element itself would end it early, and nothing can spell one
-- there that reads back the same, so text that holds one is refused. (In a
-- script @\<\\\/@ means the same as @\<\/@ inside a string but not
-- inside a regular expression, and only the script's author knows which it
-- is: one that needs @\<\/script@ in a string writes it @\<\\\/script@.)
rawText :: Text -> Text -> IO Text
rawText parent txt
  | any closes (drop 1 (T.splitOn "</" txt)) =
      throwString
        $ "renderToText: the text of a <"
        <> toS parent
        <> "> holds its end tag, which would end it early, and HTML cannot write it otherwise there"
  | otherwise = pure txt
  where
    -- What follows @\<\/@ ends the element when it is the element's name
    -- and then white space, @\/@ or @>@.
    closes s =
      T.toLower (T.take (T.length parent) s)
        == parent
        && maybe False ((`elem` ("\t\n\f\r />" :: [Char])) . fst) (T.uncons (T.drop (T.length parent) s))

escapeText :: Text -> TB.Builder
escapeText = escape False

escapeAttribute :: Text -> TB.Builder
escapeAttribute = escape True

-- Keep the ordinary prefix as one slice. Escape the remainder in whole
-- Text passes: a builder operation for every escape allocates too much when
-- escapes are dense. Ampersands go first so new references stay intact.
escape :: Bool -> Text -> TB.Builder
escape inAttribute txt =
  let (plain, rest) = T.break needsEscape txt
   in TB.fromText plain <> TB.fromText (escaped rest)
  where
    needsEscape c = c == '&' || c == '<' || c == '>' || c == '\xa0' || c == '\r' || (inAttribute && c == '"')
    escaped =
      (if inAttribute then T.replace "\"" "&quot;" else identity)
        . T.replace "\r" "&#13;"
        . T.replace "\xa0" "&nbsp;"
        . T.replace ">" "&gt;"
        . T.replace "<" "&lt;"
        . T.replace "&" "&amp;"

--------------------------------------------------------------------------------
-- Reading the tree
--------------------------------------------------------------------------------

-- | The first element inside the node, in document order, that the CSS
-- selector matches, as the DOM's @querySelector@ finds it: the selector is
-- matched against the whole tree, so @div p@ finds a @p@ inside the node
-- whose @div@ is around the node. The selectors understood are
-- "Halogen.VDom.DOM.Monad.Native.Selector"'s; any other throws a
-- 'JSError' that names it (a @NotSupportedError@ for a selector CSS has
-- and this does not, a @SyntaxError@ for one that is not CSS), never
-- finding nothing.
querySelector :: Text -> NativeNode -> IO (Maybe NativeNode)
querySelector selector node = do
  parsed <- selectorOf selector
  firstM (matchesSelector parsed) =<< descendantElements node

-- | Every element inside the node that the selector matches, in document
-- order (see 'querySelector').
querySelectorAll :: Text -> NativeNode -> IO [NativeNode]
querySelectorAll selector node = do
  parsed <- selectorOf selector
  filterM (matchesSelector parsed) =<< descendantElements node

-- | The text of the node: a text node's own, and the text of everything
-- inside an element or a document, in order, as the DOM's @textContent@
-- (whose document's is @null@).
textContent :: NativeNode -> IO Text
textContent node = case node.kind of
  TextNode -> readIORef node.content
  _ -> mconcat <$> (traverse textContent =<< readIORef node.kids)

-- | An attribute of an element as a page has it, which is the DOM's
-- @getAttribute@: 'Nothing' for one it does not have.
--
-- The in-memory DOM keeps an element's attributes and its properties
-- apart, as the reconciler sets them, where a page reflects most of the
-- properties HTML defines into their attribute. So the attribute is the
-- one written, or, if the element has the property that reflects it, the
-- property's: @className@ for @class@, @htmlFor@ for @for@, @id@, @href@,
-- @title@, … as text, and a boolean one (@disabled@, @hidden@, @open@,
-- @required@, @readOnly@ for @readonly@, …) as @""@ when it is true and
-- not at all when it is false, as 'renderToText' writes them. Four
-- properties a page does not reflect, because they are what the reader
-- changed rather than what the page said: @value@, @checked@, @selected@
-- and @muted@; their attributes are only the ones written (as in a page,
-- @input.value@ is not @getAttribute "value"@). The name is compared without
-- regard to case on an HTML element.
attribute :: Text -> NativeNode -> IO (Maybe Text)
attribute name node = case node.kind of
  ElementNode {} -> do
    let wanted = attributeName node name
    reflected <- case M.lookup wanted reflectingProperties of
      Just (prop, how) -> fmap (reflectedValue how) . M.lookup prop <$> readIORef node.props
      Nothing -> pure Nothing
    case reflected of
      Just fromProperty -> pure fromProperty
      Nothing -> writtenAttribute wanted node
  _ -> pure Nothing
  where
    reflectedValue how v = case reflect how v of
      (_, Just (Valued t)) -> Just t
      (_, Just Bare) -> Just ""
      (_, Nothing) -> Nothing

-- | An attribute as written (with 'setAttribute', or by the reconciler),
-- in any namespace, the one in none first.
writtenAttribute :: Text -> NativeNode -> IO (Maybe Text)
writtenAttribute wanted node = do
  attributes <- readIORef node.attrs
  let named = [v | ((_, n), v) <- M.toAscList attributes, attributeName node n == wanted]
  pure (M.lookup (Nothing, wanted) attributes <|> head named)

-- | An attribute's name as the element compares it: in lower case on an
-- HTML element, as written on any other.
attributeName :: NativeNode -> Text -> Text
attributeName node name = case node.kind of
  ElementNode ns _ | isNothing (htmlIsNothing ns) -> T.toLower name
  _ -> name

-- | The property that reflects each attribute, and how: every one
-- 'renderToText' writes but the four a page does not reflect (see
-- 'attribute').
reflectingProperties :: Map Text (Text, Reflection)
reflectingProperties =
  M.fromList
    [ (attributeOf how, (prop, how))
    | (prop, how) <- M.toList reflections
    , prop `notElem` ["value", "checked", "selected", "muted"]
    ]
  where
    attributeOf = \case
      Plain a -> a
      Presence a -> a
      TrueFalse a -> a
      YesNo a -> a

-- | Whether the node is in a document: whether the chain of its parents
-- reaches one, as the DOM's @isConnected@ says. A node under an element
-- made with 'newElement' is not, however deep, until that element is put
-- in a document; one under 'newContainer' is.
isConnected :: NativeNode -> IO Bool
isConnected node = case node.kind of
  DocumentNode -> pure True
  _ -> readIORef node.parentRef >>= maybe (pure False) isConnected

-- | Whether the other node is this one or inside it, as the DOM's
-- @contains@.
contains :: NativeNode -> NativeNode -> IO Bool
contains node other
  | node == other = pure True
  | otherwise = readIORef other.parentRef >>= maybe (pure False) (contains node)

-- | The node's parent, if it is an element.
parentElement :: NativeNode -> IO (Maybe NativeNode)
parentElement node =
  readIORef node.parentRef <&> \case
    Just p | isElement p -> Just p
    _ -> Nothing

isElement :: NativeNode -> Bool
isElement n = case n.kind of
  ElementNode {} -> True
  _ -> False

-- | The next element among the node's siblings.
nextElement :: NativeNode -> IO (Maybe NativeNode)
nextElement node =
  nextSiblingNative node >>= \case
    Nothing -> pure Nothing
    Just n
      | isElement n -> pure (Just n)
      | otherwise -> nextElement n

-- | A document's @body@ or @head@: the element of that name in its
-- @html@.
documentChild :: Text -> NativeNode -> IO (Maybe NativeNode)
documentChild name doc = do
  roots <- readIORef doc.kids
  case [r | r <- roots, isNamed "html" r] of
    html : _ -> find (isNamed name) <$> readIORef html.kids
    [] -> pure Nothing
  where
    isNamed wanted n = case n.kind of
      ElementNode ns (ElemName tag) -> isNothing (htmlIsNothing ns) && tag == wanted
      _ -> False

-- | Every element inside the node, in document order.
descendantElements :: NativeNode -> IO [NativeNode]
descendantElements node = do
  children <- readIORef node.kids
  concat <$> for children (\c -> if isElement c then (c :) <$> descendantElements c else pure [])

-- | The element itself or its nearest ancestor that the selector matches,
-- as the DOM's @closest@.
closest :: Text -> NativeNode -> IO (Maybe NativeNode)
closest selector node = do
  parsed <- selectorOf selector
  let go n
        | isElement n =
            matchesSelector parsed n >>= \case
              True -> pure (Just n)
              False -> parentElement n >>= maybe (pure Nothing) go
        | otherwise = pure Nothing
  go node

-- | The selector, or the exception that says why it is refused.
selectorOf :: Text -> IO Selector
selectorOf selector = case parseSelector selector of
  Right parsed -> pure parsed
  Left (Unsupported why) ->
    throwIO
      JSError
        { name = "NotSupportedError"
        , message = "the in-memory DOM does not understand the selector " <> show selector <> ": " <> why
        }
  Left (Malformed why) ->
    throwIO
      JSError
        { name = "SyntaxError"
        , message = "the selector " <> show selector <> " is not a valid selector: " <> why
        }

matchesSelector :: Selector -> NativeNode -> IO Bool
matchesSelector = matches subject
  where
    subject =
      Subject
        { tag = \n -> case n.kind of
            ElementNode _ (ElemName t) -> t
            _ -> ""
        , html = \n -> case n.kind of
            ElementNode ns _ -> isNothing (htmlIsNothing ns)
            _ -> False
        , attribute = flip attribute
        , parent = parentElement
        }

firstM :: (a -> IO Bool) -> [a] -> IO (Maybe a)
firstM _ [] = pure Nothing
firstM f (x : xs) = f x >>= \hit -> if hit then pure (Just x) else firstM f xs

-- | Replace what is in an element with one text node (none for empty
-- text), as setting its @textContent@ does.
replaceText :: NativeNode -> Text -> IO ()
replaceText n txt = do
  traverse_ detach =<< readIORef n.kids
  unless (T.null txt) $ do
    text <- newNode TextNode
    writeIORef text.content txt
    insertNative text Nothing n

--------------------------------------------------------------------------------
-- Dispatching an event
--------------------------------------------------------------------------------

-- | What became of an event 'dispatchOutcome' dispatched.
data Dispatched = Dispatched
  { defaultPrevented :: Bool
  -- ^ Whether a listener prevented its default (a handler's own
  -- @preventDefault@, or 'Halogen.HTML.Events.preventingDefault'), which a
  -- page would then not take.
  , heard :: Int
  -- ^ How many listeners ran.
  , disabled :: Bool
  -- ^ Whether it was a click on a disabled form control, which a page does
  -- not dispatch at all: then nothing ran.
  }
  deriving stock (Eq, Show)

-- | 'dispatchOutcome', for a test that does not ask what became of the
-- event.
dispatch :: Text -> Aeson.Value -> NativeNode -> IO ()
dispatch eventType fields node = void (dispatchOutcome eventType fields node)

-- | Dispatch an event of a type, with the fields of the JSON object (a
-- key's @key@, a click's @button@, @clientX@), at a node, as a page
-- dispatches one the reader caused: the listeners on the node run, then
-- those on each node around it in turn, up to the document and the window
-- if the node is in one ('newContainer'). Each listener runs before this
-- returns, in the order it was added, in 'MemDOM', on this thread; an
-- exception out of one ends the dispatch and is thrown here. A listener's
-- handler that hands an action to its component has it handled before this
-- returns when the component's loop is free, as in a page: what the
-- program then waits for in a @liftIO@ is not, and a test waits for it
-- with "Halogen.Activity".
--
-- What a listener reads of the event:
--
-- * @target@ is the node dispatched at, and @currentTarget@ the node whose
--   listener runs, as handles of the tree (@Element.fromEventTarget@,
--   @Element.closestNow@, @element.contains@ and the rest read the tree
--   through them); @type@, @bubbles@, @cancelable@ and @eventPhase@ are
--   the event's, and @defaultPrevented@ is whether a listener has
--   prevented it so far ('Web.Event.Event.defaultPreventedNow');
-- * everything else is the JSON's (@key@, @button@, @ctrlKey@,
--   @clientX@, @dataTransfer@, …), as for 'event'; a @target@ or
--   @currentTarget@ in it is not read;
-- * a checked cast finds the class the type is of: a @keydown@ is a
--   @KeyboardEvent@, a @click@ a @MouseEvent@, a @pointerdown@ a
--   @PointerEvent@ and a @MouseEvent@, a @focus@ a @FocusEvent@, a @paste@ a
--   @ClipboardEvent@, a @drop@ a @DragEvent@, and every one an @Event@.
--
-- @stopPropagation@ (a handler's own, or
-- 'Halogen.HTML.Events.stoppingPropagation') lets the rest of the
-- listeners on that node run and stops the event there;
-- @stopImmediatePropagation@ stops it at once. A handler's declared match
-- ('Halogen.HTML.Events.matching') is judged on the tree, as in a page: a
-- 'Halogen.HTML.Events.within' finds its element with the selector.
--
-- An event bubbles unless its JSON says @"bubbles": false@, or its type is
-- one that does not in a page and the JSON does not say @true@: @focus@,
-- @blur@, @mouseenter@, @mouseleave@, @pointerenter@, @pointerleave@,
-- @load@, @unload@, @error@, @abort@, @scroll@, @scrollend@, @resize@,
-- @toggle@, @invalid@, @cancel@, @close@ and a media element's events
-- (@play@, @pause@, @ended@, @timeupdate@, …). It is cancelable unless its
-- JSON says @"cancelable": false@, or its type is one a page cannot
-- prevent (@input@, @change@, @focusin@, @focusout@, @select@ and the
-- non-bubbling ones but @invalid@ and @cancel@): its @preventDefault@ then
-- does nothing.
--
-- An event dispatched at a text node is dispatched at its parent element,
-- which is where the reader's pointer is on the text: no event the reader
-- causes has a text node as its target (one with no parent element keeps
-- it).
--
-- The nodes it travels through are fixed when it is dispatched, and a
-- listener runs only if it is still on its node when its turn comes, as in
-- a page: a listener that takes its node out of the tree (a handler that
-- closes the popup it is in, or ends the whole tree) does not keep the
-- nodes around it from hearing the event, and one a listener removes
-- before its turn does not run. There is no capturing phase: the
-- in-memory DOM's listeners are all bubbling ones.
--
-- A @click@ dispatched at a disabled form control (a @button@, @input@,
-- @select@ or @textarea@ that has @disabled@, as an attribute or the
-- property, or is inside a @fieldset@ that has it and not in that
-- fieldset's first @legend@) is not dispatched, as a page does not
-- dispatch the reader's click there: nothing runs, and 'disabled' says so.
-- Other events are dispatched there as anywhere.
--
-- The window and the document are one node here (see 'newContainer'): the
-- window's listeners and the document's run in the order they were added,
-- and a @stopPropagation@ in either does not keep the other's from
-- running. Nothing does what a page does by default after the event (a
-- link followed, a checkbox ticked, a character typed): a test does that
-- itself (sets the field's @value@ property, then dispatches @input@).
dispatchOutcome :: Text -> Aeson.Value -> NativeNode -> IO Dispatched
dispatchOutcome eventType fields at = do
  -- What the reader points at or types into is an element: a text node is
  -- never the target of an event the reader causes.
  target <-
    if at.kind == TextNode
      then fromMaybe at <$> parentElement at
      else pure at
  dispatchAt eventType fields target

dispatchAt :: Text -> Aeson.Value -> NativeNode -> IO Dispatched
dispatchAt eventType fields target = do
  blocked <- if eventType == "click" then disabledControl target else pure False
  if blocked
    then pure Dispatched {defaultPrevented = False, heard = 0, disabled = True}
    else do
      ancestors <- ancestorsOf target
      prevented <- newIORef False
      stopped <- newIORef False
      stoppedNow <- newIORef False
      count <- newIORef (0 :: Int)
      let path = if bubbles then target : ancestors else [target]
          propagation = Propagation {prevented, stopped, stoppedNow}
          visit [] = pass
          visit (current : rest) = do
            registered <- filter ((== eventType) . (.eventType)) <$> readIORef current.listeners
            for_ registered $ \l -> do
              now <- readIORef stoppedNow
              -- A listener taken off its node before its turn does not run.
              still <- any (\m -> m.ident == l.ident && m.eventType == eventType) <$> readIORef current.listeners
              when (not now && still) $ do
                modifyIORef' count (+ 1)
                runMemDOM (l.fire (eventFor propagation current))
            done <- readIORef stopped
            unless done (visit rest)
      visit path
      Dispatched <$> readIORef prevented <*> readIORef count <*> pure False
  where
    flag name = case fields of
      Aeson.Object o -> case KeyMap.lookup name o of
        Just (Aeson.Bool b) -> Just b
        _ -> Nothing
      _ -> Nothing
    bubbles = fromMaybe (eventType `notElem` nonBubbling) (flag "bubbles")
    cancelable = fromMaybe (eventType `notElem` uncancelable) (flag "cancelable")
    eventFor propagation current =
      Event (withNativeObject (eventObject propagation current) (toForeign fields))
    eventObject propagation current =
      NativeObject
        { call = \name _ -> case name of
            "preventDefault" -> when cancelable (writeIORef propagation.prevented True)
            "stopPropagation" -> writeIORef propagation.stopped True
            "stopImmediatePropagation" -> writeIORef propagation.stopped True >> writeIORef propagation.stoppedNow True
            _ -> pass
        , invoke = \_ _ -> pure Nothing
        , set = \_ _ -> pass
        , get = fixed current
        , live = \case
            "defaultPrevented" -> Just . toJSBool <$> readIORef propagation.prevented
            name -> pure (fixed current name)
        , instanceOf = (`elem` eventClasses eventType)
        }
    fixed current = \case
      "type" -> Just (textValue eventType)
      "target" -> Just (handleOf target)
      "currentTarget" -> Just (handleOf current)
      "bubbles" -> Just (toJSBool bubbles)
      "cancelable" -> Just (toJSBool cancelable)
      "eventPhase" -> Just (toJSInt (if current == target then 2 else 3))
      _ -> Nothing

-- | Where an event being dispatched stands.
data Propagation = Propagation
  { prevented :: IORef Bool
  , stopped :: IORef Bool
  , stoppedNow :: IORef Bool
  }

-- | The nodes around a node, nearest first, up to the top of its tree.
ancestorsOf :: NativeNode -> IO [NativeNode]
ancestorsOf node =
  readIORef node.parentRef >>= \case
    Nothing -> pure []
    Just p -> (p :) <$> ancestorsOf p

-- | Whether the node is a form control a page dispatches no click to: one
-- with @disabled@, or one inside a @fieldset@ with @disabled@ and not in
-- that fieldset's first @legend@.
disabledControl :: NativeNode -> IO Bool
disabledControl node = case node.kind of
  ElementNode ns (ElemName tag)
    | isNothing (htmlIsNothing ns) && tag `elem` ["button", "input", "select", "textarea"] -> do
        own <- isJust <$> attribute "disabled" node
        if own then pure True else fieldsetDisabled node =<< ancestorsOf node
  _ -> pure False
  where
    -- The ancestors, nearest first: a disabled fieldset disables the
    -- control unless the control is in its first legend, which is then the
    -- ancestor just before it.
    fieldsetDisabled _ [] = pure False
    fieldsetDisabled child (a : rest) = do
      here <- case a.kind of
        ElementNode ns (ElemName "fieldset") | isNothing (htmlIsNothing ns) -> do
          off <- isJust <$> attribute "disabled" a
          if not off
            then pure False
            else do
              legend <- find isLegend <$> readIORef a.kids
              pure (legend /= Just child)
        _ -> pure False
      if here then pure True else fieldsetDisabled a rest
    isLegend n = case n.kind of
      ElementNode ns (ElemName "legend") -> isNothing (htmlIsNothing ns)
      _ -> False

-- | The types that do not bubble in a page.
nonBubbling :: [Text]
nonBubbling =
  [ "focus"
  , "blur"
  , "mouseenter"
  , "mouseleave"
  , "pointerenter"
  , "pointerleave"
  , "load"
  , "unload"
  , "error"
  , "abort"
  , "scroll"
  , "scrollend"
  , "resize"
  , "toggle"
  , "invalid"
  , "cancel"
  , "close"
  ]
    <> mediaEvents

-- | The types whose default a page cannot prevent.
uncancelable :: [Text]
uncancelable =
  filter (`notElem` ["invalid", "cancel"]) nonBubbling
    <> ["input", "change", "focusin", "focusout", "select"]

mediaEvents :: [Text]
mediaEvents =
  concatMap
    T.words
    [ "play pause playing ended timeupdate volumechange durationchange loadstart progress"
    , "loadedmetadata loadeddata canplay canplaythrough seeking seeked waiting ratechange emptied stalled suspend"
    ]

-- | The classes an event of a type is an instance of.
eventClasses :: Text -> [Text]
eventClasses eventType = "Event" : specific
  where
    ui = ["UIEvent"]
    mouse = "MouseEvent" : ui
    specific
      | eventType `elem` ["keydown", "keyup", "keypress"] = "KeyboardEvent" : ui
      | "pointer" `T.isPrefixOf` eventType || eventType `elem` ["gotpointercapture", "lostpointercapture"] = "PointerEvent" : mouse
      | eventType `elem` ["click", "dblclick", "auxclick", "contextmenu", "mousedown", "mouseup", "mousemove", "mouseover", "mouseout", "mouseenter", "mouseleave"] = mouse
      | eventType == "wheel" = "WheelEvent" : mouse
      | "drag" `T.isPrefixOf` eventType || eventType == "drop" = "DragEvent" : mouse
      | eventType `elem` ["focus", "blur", "focusin", "focusout"] = "FocusEvent" : ui
      | "touch" `T.isPrefixOf` eventType = "TouchEvent" : ui
      | eventType `elem` ["copy", "cut", "paste"] = ["ClipboardEvent"]
      | eventType `elem` ["input", "beforeinput"] = "InputEvent" : ui
      | otherwise = []

--------------------------------------------------------------------------------
-- The instance
--------------------------------------------------------------------------------

-- | How a property reads as text, for the inspection helpers. Not used for
-- equality — 'PropScalar' is.
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
  elementToEventTarget el = pure (fromNative (toNative el))

  -- The event type is not known until addEventListener; it is filled in there.
  mkEventListener f = liftIO $ do
    i <- nextIdent
    pure $ fromListener $ Listener {ident = i, eventType = "", fire = f}

  createTextNode txt _ = liftIO $ do
    node <- newNode TextNode
    writeIORef node.content txt
    pure (fromNative node)

  -- As in the DOM: an element's text content replaces its children with
  -- one text node (none at all for empty text), which is how a page empties
  -- a mount point; a text node's is its data.
  setTextContent txt node = liftIO $ do
    let n = toNative node
    case n.kind of
      TextNode -> writeIORef n.content txt
      DocumentNode -> pass
      ElementNode {} -> replaceText n txt

  createElement ns name _ = liftIO $ fromNative <$> newElement ns name

  -- The browser backends guard each of these on reference equality; reproduce
  -- the guards so a no-op patch stays a no-op here too.

  insertChildIx ix child parent = liftIO $ do
    let node = toNative child
        p = toNative parent
    occupant <- childAt ix p
    when (occupant /= Just node) $ insertNative node occupant p

  removeChild child _ = liftIO $ detach (toNative child)

  parentNode node = liftIO $ fmap fromNative <$> readIORef (toNative node).parentRef
  nextSibling node = liftIO $ fmap fromNative <$> nextSiblingNative (toNative node)
  firstChild node = liftIO $ fmap fromNative . head <$> readIORef (toNative node).kids
  sameNode a b = pure (toNative a == toNative b)
  nodeKind node = liftIO $ do
    let n = toNative node
    case n.kind of
      ElementNode ns name -> pure (ElementKind ns name)
      TextNode -> TextKind <$> readIORef n.content
      DocumentNode -> pure OtherKind
  nodeToElement node = pure $ case (toNative node).kind of
    ElementNode {} -> Just (fromNative (toNative node))
    _ -> Nothing

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
  windowToEventTarget w = pure (coerce w)
  documentToNode d = pure (coerce d)
  window = liftIO $ pure (fromNative ambientDocument)
  document _ = liftIO $ pure (fromNative ambientDocument)
  querySelector (QuerySelector selector) parent =
    liftIO
      $ fmap fromNative
      <$> querySelector selector (toNative parent)
  readyState _ = liftIO $ pure ReadyState.Complete
  readStorageItem kind key = liftIO $ M.lookup key . store kind <$> readIORef ambientStorage
  writeStorageItem kind key text =
    liftIO $ atomicModifyIORef'_ ambientStorage $ M.insertWith M.union kind (M.singleton key text)
  removeStorageItem kind key =
    liftIO $ atomicModifyIORef'_ ambientStorage $ M.adjust (M.delete key) kind
  storageItemKeys kind = liftIO $ M.keys . store kind <$> readIORef ambientStorage

instance MonadAttributes MemDOM where
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
