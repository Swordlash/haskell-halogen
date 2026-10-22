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
import Numeric (floatToDigits)
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
-- @newtype X = X (Foreign X)@ and @Foreign tag@ a newtype over @Any@ on native.
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
  renderIn context node

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

renderIn :: Context -> NativeNode -> IO Text
renderIn context node = case node.kind of
  TextNode -> do
    txt <- readIORef node.content
    refuseNul "a text" txt
    case context.element of
      Just raw | raw `elem` rawTextElements -> rawText raw txt
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
        open = "<" <> name <> foldMap renderAttribute (M.toAscList merged) <> ">"
    for_ (M.toList merged) $ \case
      ((_, attribute), Valued v) -> refuseNul ("the attribute " <> attribute) v
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
              | html && name `elem` ["pre", "textarea", "listing"] && "\n" `T.isPrefixOf` inner = "\n" <> inner
              | otherwise = inner
        pure $ open <> kept <> "</" <> name <> ">"

-- | The children in turn, with an empty comment between two text nodes,
-- except in a raw-text element, where the comment would be text.
renderChildren :: Context -> NativeNode -> IO Text
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

renderAttribute :: ((Maybe Text, Text), AttributeValue) -> Text
renderAttribute ((ns, name), v) =
  " " <> qualified <> case v of
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

escapeText :: Text -> Text
escapeText = T.concatMap $ \case
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  '\xa0' -> "&nbsp;"
  '\r' -> "&#13;"
  c -> T.singleton c

escapeAttribute :: Text -> Text
escapeAttribute = T.concatMap $ \case
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  '"' -> "&quot;"
  '\xa0' -> "&nbsp;"
  '\r' -> "&#13;"
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

  -- As in the DOM: an element's text content replaces its children with
  -- one text node (none at all for empty text), which is how a page empties
  -- a mount point; a text node's is its data.
  setTextContent txt node = liftIO $ do
    let n = toNative node
    case n.kind of
      TextNode -> writeIORef n.content txt
      DocumentNode -> pass
      ElementNode {} -> do
        traverse_ detach =<< readIORef n.kids
        unless (T.null txt) $ do
          text <- newNode TextNode
          writeIORef text.content txt
          insertNative text Nothing n

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
      <$> queryNative selector (toNative parent)
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
