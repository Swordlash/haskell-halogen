-- | What crosses the wire between a component and the page that shows it.
--
-- __To the page__, one JSON object per message, a batch of operations
-- applied in order:
--
-- > {"n": 1, "ops": [["clear"], ["create", 2, null, "div"], ...]}
--
-- @n@ counts the session's batches from 1. Each operation is an array whose
-- first element says what it is; a node is named by a number the session
-- gave it, and a listener likewise:
--
-- > ["clear"]                                  empty the container, forget every node
-- > ["create", node, namespace, name]          an element (namespace null for HTML)
-- > ["text", node, text]                       a text node
-- > ["set-text", node, text]                   a text node's text
-- > ["insert", parent, index, child]           put child at index among parent's children
-- > ["remove", parent, child]                  take child out of parent
-- > ["attr", node, namespace, name, value]     set an attribute
-- > ["unattr", node, namespace, name]          remove one
-- > ["prop", node, name, value]                set a property (a string, number or boolean)
-- > ["unprop", node, name]                     reset one
-- > ["listen", node, type, listener]           listen for events of a type
-- > ["unlisten", node, type, listener]         stop
-- > ["options", node, type, listener, options] what the listener's handler declared
-- > ["call", node, method, [arguments]]       call one of the node's methods
-- > ["set", node, name, value]                 assign one of its properties
-- > ["history", "push", address]               history.pushState(null, "", address)
-- > ["history", "replace", address]            history.replaceState(null, "", address)
-- > ["history", "go", n]                       history.go(n)
-- > ["storage", store, key, value]             write an item ("local" or "session"; null removes it)
--
-- Node 0 is the window (what the component's program knows as both the
-- window and the document) and node 1 the container the client attached
-- to (the component's @body@, which it is mounted into); the session never
-- creates either. @insert@'s index is where the child ends up, counted
-- among the parent's children once the child has been taken from wherever
-- it was.
--
-- @options@ follows a @listen@ whose handler declared anything
-- ('Halogen.VDom.DOM.HandlerOptions.HandlerOptions'), in the same batch,
-- and comes again whenever a render changes what it declared:
--
-- > {"preventDefault": true, "stopPropagation": false, "match": m}
--
-- where @m@ is @null@ (no match declared), @"any"@, @"plain-left-click"@,
-- or a list of keys, each @{"key": "/"}@ with @ctrlKey@, @shiftKey@,
-- @altKey@ and @metaKey@ when it says whether they are held. A listener
-- whose options declare nothing (no match, neither flag) is one that never
-- had any.
--
-- The client acts on them while the page dispatches the event, which the
-- program cannot: for a listener with a match, an event that does not match
-- is not sent at all, and one that does has its default prevented and its
-- propagation stopped as declared; for one without a match, every event it
-- hears. For a listener that declares nothing, the client prevents the
-- default of a @submit@ and of a click on an @a@ the browser would follow
-- in this tab, and of nothing else.
--
-- @call@ and @set@ are what the program did to the page through the @Web.*@
-- types (a field focused, a dialog opened, the title set), which the page
-- carries out from an allowlist, as data, and not as a way to call anything
-- by name. A call is one of
--
-- > focus          [] or [{"preventScroll": b}]
-- > blur, click, select, show, showModal   []
-- > close          [] or [returnValue], a string
-- > scrollIntoView [] or [{"block": s, "inline": s, "behavior": s}], each optional
--
-- on an element; a set is @title@ on node 0, the document's title. The
-- client ignores anything else, and a call on a node it does not have; one
-- that throws in the page (@showModal@ on a dialog already open) is logged
-- and does not stop the batch. The program records only these, so nothing
-- else crosses. A call comes after the operations that made its node, in
-- the same batch or a later one, and the client applies a batch in order,
-- synchronously: @focus@ on a node the batch created and attached reaches
-- it there.
--
-- A node taken out and not put back by the end of the batch is gone, on both
-- sides: once the last operation is applied, each node a @remove@ took out
-- that still has no parent is forgotten, with everything under it and the
-- listeners on them, and no operation names it again. A node taken out and
-- put back by an @insert@ in the same batch stays, as the node of a child
-- component does when its parent replaces the element around it, and a
-- batch never ends in the middle of a render. A batch says nothing about
-- what happens inside a node it leaves gone (its children taken out of it,
-- its listeners stopped): the client forgets all of it anyway. That is so
-- only after the batch's last @call@: everything before a @call@ is sent,
-- so that the client carries it out on the page as the program had it (a
-- link clicked to start a download, with the address the same action gave
-- it, then hidden; a form submitted with a field that is then taken away).
--
-- @history@ and @storage@ are what the program did to the page's address
-- and stores ("Halogen.Router"'s @navigate@ and @redirect@, a window's
-- @history@ through @Web.HTML.History@, "Web.Storage.Storage"): an address
-- is the one the program wrote (the router writes paths from the root, and
-- whole addresses before a fragment), or @null@ for the page's own; a key
-- is the whole key, the prefix included, and the client writes no key
-- without it (the page's other items are not the program's to write). The
-- session's mirror of the page
-- has changed already when the operation is recorded, so what the program
-- reads next is what it wrote. The page carries them out in order with the
-- rest of the batch; one the browser refuses (an address of another
-- origin, a store over its quota or denied the page) is logged there and
-- does not stop the batch, and the program, whose mirror has moved, does
-- not hear of it.
--
-- __To the program__, first, before anything else, when the client
-- connects: where the page is, and what its stores hold of this library's
-- items (those whose keys start with core's
-- 'Halogen.VDom.DOM.Monad.storagePrefix', @haskell-halogen:@; nothing
-- else crosses):
--
-- > {"hello": {"location": "https://example.com/blog/post/1?q=2#top",
-- >            "base": "https://example.com/blog/",
-- >            "storage": {"local": {"haskell-halogen:count": "Mw=="},
-- >                        "session": {}}}}
--
-- @location@ is @location.href@, and @base@ the page's @document.baseURI@
-- when it has a @\<base href>@ and @null@ when it has none. A store the
-- page is denied is sent empty. The session mirrors all of it: the
-- window's @location@ answers from it (@pathname@, @search@, @hash@, @href@,
-- @origin@), the document's @baseURI@ is @base@ (and empty with none), and
-- the storage methods of 'Halogen.VDom.DOM.Monad.MonadBrowserDOM' read it.
-- "Halogen.Remote.Server" waits for this message before it calls anything
-- for the connection; a session that is given something else first (or,
-- driven by hand, nothing) is at @\/@ with no base and empty stores.
-- Its size is that of the library's items in the page's two stores, at
-- most what a browser lets an origin keep (about 5 MB each), and in
-- practice a few bytes.
--
-- Then, whenever the page goes back or forward through its history (and
-- when the reader edits its fragment), its address, sent from the client's
-- own @popstate@ (and @hashchange@) listener, which the client adds to the
-- window before any of the program's, so that it crosses before the event
-- a listener of the program's hears:
--
-- > {"location": "https://example.com/blog/"}
--
-- And when another tab changes an item of this library's in local storage
-- (the window's @storage@ event), the change: an item's new text, @null@
-- for one removed, and a @key@ of @null@ for a store emptied:
--
-- > {"storage": {"kind": "local", "key": "haskell-halogen:count", "value": "NA=="}}
--
-- Each event a listener heard:
--
-- > {"listener": 7, "node": 4, "type": "input",
-- >  "event": {"type": "input", "target": {"value": "hi", ...},
-- >            "currentTarget": {"value": "hi", ...}, ...}}
--
-- @event@ holds what handlers read of an event: its @type@, @key@,
-- @code@, the modifier keys, @button@, the pointer's coordinates,
-- @detail@, @defaultPrevented@ (@true@ only when something in the page
-- had prevented the default before the client got the event; absent
-- otherwise), and for its @target@ and @currentTarget@ their @value@,
-- @checked@, @selectedIndex@, @tagName@ and @id@, and of a link (an @a@ or
-- an @area@) its @target@ when it names one and @download: true@ when it
-- has the attribute: what "Halogen.Router"'s @isPlainLeftClick@ reads.
--
-- = The probe
--
-- A test that drives the page from outside (@haskell-halogen-playwright@)
-- asks the page's @globalThis.__halogenProbe.activity()@ after every action
-- whether the program is busy ("Halogen.Activity"). Over a wire the program
-- is not in the page, so the client forwards the question, and the program
-- answers it. __To the program__, a question, with a number the client
-- chose:
--
-- > {"probe": 3}
--
-- __To the page__, its answer, a message that is not a batch (it has no
-- @ops@):
--
-- > {"probe": 3, "trees": 1, "running": 0, "waiting": 0}
--
-- @trees@ is 1 while the session's component is mounted and 0 before and
-- after; @running@ is 1 while its tree is running or has work queued, or
-- while the session has anything the page has not been sent (operations
-- recorded and not yet in a batch, a batch being sent, a render under
-- way), and 0 otherwise; @waiting@ is how many of its fibers wait in a
-- 'liftIO'. So "idle" over a wire means that the page shows what the
-- program rendered: rendered, and sent.
--
-- The answer is sent through the session's sender, in order with its
-- batches, and only once every operation recorded when the question
-- arrived has been sent: if anything was waiting to go then, the answer
-- waits for the batch that takes it (and for the render under way, since a
-- batch is taken only between renders); the reading is taken when the
-- answer is sent, not when the question arrived. That is the contract, and
-- it rests on three facts of order:
--
-- * The page's messages cross one socket, in order, and the program
--   handles them one at a time: an event the page sent before the question
--   has been handed to the component's loop (its handler run, or queued)
--   before the question is read, so a reading taken after the question
--   sees that work, as running, waiting or done.
--
-- * The tree's reading is taken before the session's own state: work that
--   ends between the two has recorded what it rendered before it ended, so
--   the session sees it as unsent.
--
-- * The client applies messages in the order they arrive and applies a
--   batch synchronously: when the page's promise resolves with an answer,
--   every batch sent before the answer is on the page.
--
-- So an idle answer means that every event the page sent before asking has
-- been handled and what it rendered is on the page, short of what is still
-- waiting in a 'liftIO' (counted in @waiting@) and what has not happened
-- yet (a timer, a pause in 'Halogen.Query.HalogenM.delay').
--
-- A batch can itself make the page send an event, after the question: a
-- @call@ of @focus@ or @click@ on an element the program listens to. The
-- answer that follows the batch does not account for that event, so the
-- client (@jsbits/remote.js@) does not give the page an answer when it has
-- sent an event since the question: it asks again, until an answer comes
-- that no event followed. From a batch with a @call@ until the page's next
-- frame it holds the answers it receives, since some events a call causes
-- are dispatched then (a dialog's @close@); and from a batch with a
-- @history@ @go@ until the page has gone there (its @popstate@), since the
-- page moves through its history a moment after it is asked.
--
-- = Messages a side does not know
--
-- Either side ignores a message it does not know (a JSON object with
-- neither @ops@ nor @probe@, for the client; with none of @listener@,
-- @probe@, @hello@, @location@ and @storage@, for the program), and the
-- client an operation it does not know, so that a kind added later does
-- not break an older client or program.
--
-- The page's messages other than events and questions keep the order
-- argument above: they cross the same socket, in order, and the program
-- handles them as they come, so a question asked after the page went back
-- is answered after the session knew where it went, and after the
-- @popstate@ it dispatched was handed to the component.
module Halogen.Remote.Protocol
  ( NodeId
  , ListenerId
  , Op (..)
  , HistoryMove (..)
  , Hello (..)
  , Batch (..)
  , EventMessage (..)
  , FromProgram (..)
  , FromPage (..)
  , encodeOptions
  , decodeOptions
  , encodeBatch
  , decodeEventMessage
  , encodeProbeAnswer
  , decodeFromProgram
  , encodeProbeQuestion
  , decodeFromPage
  , encodeFromPage
  )
where

import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Map.Strict qualified as Map
import Data.Scientific (floatingOrInteger, fromFloatDigits)
import Data.Text.Lazy qualified as Lazy
import Data.Vector qualified as Vector
import Halogen.Activity (Activity (..))
import Halogen.VDom.DOM.HandlerOptions (EventMatch (..), HandlerOptions (..), KeyMatch (..))
import Halogen.VDom.DOM.Monad (PropScalar (..), StorageKind (..))
import Protolude

-- | A node, as both sides name it.
type NodeId = Int

-- | A listener, as both sides name it.
type ListenerId = Int

-- | One change to the page, as the module's header spells it.
data Op
  = Clear
  | -- | An element: its node, its namespace ('Nothing' for HTML) and its name.
    Create NodeId (Maybe Text) Text
  | -- | A text node and its text.
    CreateText NodeId Text
  | -- | A text node's new text.
    SetText NodeId Text
  | -- | The parent, the index the child ends up at, and the child.
    Insert NodeId Int NodeId
  | -- | The parent and the child, which is forgotten with its subtree at
    -- the end of the batch unless an 'Insert' puts it back.
    Remove NodeId NodeId
  | -- | The element, the attribute's namespace and name, and its value.
    SetAttribute NodeId (Maybe Text) Text Text
  | RemoveAttribute NodeId (Maybe Text) Text
  | -- | The element, the property's name, and its value.
    SetProperty NodeId Text PropScalar
  | RemoveProperty NodeId Text
  | -- | The node, the event type and the listener.
    Listen NodeId Text ListenerId
  | Unlisten NodeId Text ListenerId
  | -- | The node, the event type and the listener, and what its handler
    -- declared now.
    ListenerOptions NodeId Text ListenerId HandlerOptions
  | -- | The node, the method and its arguments: one of the allowlist's
    -- (see the module's header).
    Call NodeId Text [Value]
  | -- | The node, the property and its value: the document's title.
    Assign NodeId Text Value
  | -- | A move in the page's session history.
    HistoryOp HistoryMove
  | -- | A store's item written ('Just' its text) or removed ('Nothing'): the
    -- whole key, as the program wrote it.
    StorageOp StorageKind Text (Maybe Text)
  deriving stock (Eq, Show)

-- | What the program did to the page's history: a new entry, the current
-- one replaced (with the address, or 'Nothing' for the one the page is at),
-- or a move through the entries, back for a negative number.
data HistoryMove
  = Push (Maybe Text)
  | Replace (Maybe Text)
  | Go Int
  deriving stock (Eq, Show)

-- | What the page tells the program first, when it connects: where it is,
-- and what its stores hold of this library's.
data Hello = Hello
  { location :: Text
  -- ^ The page's whole address, @location.href@.
  , base :: Maybe Text
  -- ^ The address of its @\<base href>@ (@document.baseURI@), if it has one.
  , local :: Map Text Text
  -- ^ The items of its local storage whose keys start with core's
  -- 'Halogen.VDom.DOM.Monad.storagePrefix', whole keys as stored.
  , session :: Map Text Text
  -- ^ The same of its session storage.
  }
  deriving stock (Eq, Show)

-- | The operations one message carries, and its place among the session's.
data Batch = Batch
  { sequence :: Int
  , ops :: [Op]
  }
  deriving stock (Eq, Show)

-- | An event the page heard, for one of the program's listeners.
data EventMessage = EventMessage
  { listener :: ListenerId
  , node :: NodeId
  , eventType :: Text
  , event :: Value
  -- ^ The event's fields, as the module's header lists them.
  }
  deriving stock (Eq, Show)

-- | A message to the page: a batch, or the answer to a question of the
-- probe's.
data FromProgram
  = ProgramBatch Batch
  | -- | The question's number, and the answer.
    ProbeAnswer Int Activity
  deriving stock (Eq, Show)

-- | A message to the program: an event, a question of the probe's, or what
-- the program keeps of the page changing.
data FromPage
  = PageEvent EventMessage
  | -- | The question's number, which the answer repeats.
    ProbeQuestion Int
  | -- | What the page says first.
    PageHello Hello
  | -- | The page's whole address, now that it has moved through its
    -- history (back, forward, a fragment).
    PageLocation Text
  | -- | Another tab's change to a store: the key and the item's new text
    -- ('Nothing' for one removed), or no key for a store emptied.
    PageStorage StorageKind (Maybe Text) (Maybe Text)
  deriving stock (Eq, Show)

encodeBatch :: Batch -> Text
encodeBatch = Lazy.toStrict . encodeToLazyText

decodeEventMessage :: Text -> Maybe EventMessage
decodeEventMessage = Aeson.decodeStrictText

encodeProbeAnswer :: Int -> Activity -> Text
encodeProbeAnswer question a =
  Lazy.toStrict . encodeToLazyText $
    Aeson.object ["probe" .= question, "trees" .= a.trees, "running" .= a.running, "waiting" .= a.waiting]

-- | What the program sent, as the client reads it: 'Nothing' for what is
-- neither a batch nor an answer (which the client ignores).
decodeFromProgram :: Text -> Maybe FromProgram
decodeFromProgram text =
  (ProgramBatch <$> Aeson.decodeStrictText text) <|> (Aeson.decodeStrictText text >>= parseMaybe answer)
  where
    answer = Aeson.withObject "ProbeAnswer" $ \o ->
      ProbeAnswer <$> o .: "probe" <*> (Activity <$> o .: "trees" <*> o .: "running" <*> o .: "waiting")

encodeProbeQuestion :: Int -> Text
encodeProbeQuestion question = Lazy.toStrict (encodeToLazyText (Aeson.object ["probe" .= question]))

-- | What the page sent: 'Nothing' for what is none of the messages the
-- module's header lists (which the program ignores).
decodeFromPage :: Text -> Maybe FromPage
decodeFromPage text =
  (PageEvent <$> decodeEventMessage text) <|> (Aeson.decodeStrictText text >>= parseMaybe other)
  where
    other = Aeson.withObject "FromPage" $ \o ->
      (ProbeQuestion <$> o .: "probe")
        <|> (PageHello <$> (o .: "hello" >>= hello))
        <|> (PageLocation <$> o .: "location")
        <|> (o .: "storage" >>= storage)
    hello = Aeson.withObject "Hello" $ \o -> do
      stores <- o .:? "storage"
      let store name = maybe (pure Map.empty) (\s -> fromMaybe Map.empty <$> s .:? name) stores
      Hello <$> o .: "location" <*> o .:? "base" <*> store "local" <*> store "session"
    storage = Aeson.withObject "StorageChange" $ \o ->
      PageStorage <$> (o .: "kind" >>= kind) <*> o .:? "key" <*> o .:? "value"

encodeFromPage :: FromPage -> Text
encodeFromPage =
  Lazy.toStrict . encodeToLazyText . \case
    PageEvent message -> toJSON message
    ProbeQuestion question -> Aeson.object ["probe" .= question]
    PageHello h ->
      Aeson.object
        [ "hello"
            .= Aeson.object
              [ "location" .= h.location
              , "base" .= h.base
              , "storage" .= Aeson.object ["local" .= h.local, "session" .= h.session]
              ]
        ]
    PageLocation href -> Aeson.object ["location" .= href]
    PageStorage k key value -> Aeson.object ["storage" .= Aeson.object ["kind" .= kindName k, "key" .= key, "value" .= value]]

-- | A store as the wire names it.
kindName :: StorageKind -> Text
kindName = \case
  LocalStorage -> "local"
  SessionStorage -> "session"

kind :: Text -> Parser StorageKind
kind = \case
  "local" -> pure LocalStorage
  "session" -> pure SessionStorage
  other -> fail ("not a store: " <> toS other)

instance ToJSON Op where
  toJSON = \case
    Clear -> tagged "clear" []
    Create n ns name -> tagged "create" [toJSON n, toJSON ns, toJSON name]
    CreateText n t -> tagged "text" [toJSON n, toJSON t]
    SetText n t -> tagged "set-text" [toJSON n, toJSON t]
    Insert p ix c -> tagged "insert" [toJSON p, toJSON ix, toJSON c]
    Remove p c -> tagged "remove" [toJSON p, toJSON c]
    SetAttribute n ns name v -> tagged "attr" [toJSON n, toJSON ns, toJSON name, toJSON v]
    RemoveAttribute n ns name -> tagged "unattr" [toJSON n, toJSON ns, toJSON name]
    SetProperty n name v -> tagged "prop" [toJSON n, toJSON name, scalar v]
    RemoveProperty n name -> tagged "unprop" [toJSON n, toJSON name]
    Listen n ty l -> tagged "listen" [toJSON n, toJSON ty, toJSON l]
    Unlisten n ty l -> tagged "unlisten" [toJSON n, toJSON ty, toJSON l]
    ListenerOptions n ty l o -> tagged "options" [toJSON n, toJSON ty, toJSON l, encodeOptions o]
    Call n method arguments -> tagged "call" [toJSON n, toJSON method, toJSON arguments]
    Assign n name value -> tagged "set" [toJSON n, toJSON name, value]
    HistoryOp (Push address) -> tagged "history" [String "push", toJSON address]
    HistoryOp (Replace address) -> tagged "history" [String "replace", toJSON address]
    HistoryOp (Go n) -> tagged "history" [String "go", toJSON n]
    StorageOp k key value -> tagged "storage" [String (kindName k), toJSON key, toJSON value]
    where
      tagged :: Text -> [Value] -> Value
      tagged tag rest = Array (Vector.fromList (String tag : rest))

-- | A property as JavaScript holds it: a number whatever its Haskell
-- spelling, which is also how 'PropScalar' compares them.
scalar :: PropScalar -> Value
scalar = \case
  ScalarInt i -> Number (fromInteger i)
  ScalarNum d -> Number (fromFloatDigits d)
  ScalarBool b -> Bool b
  ScalarText t -> String t

instance FromJSON Op where
  parseJSON = Aeson.withArray "Op" $ \array -> case Vector.toList array of
    String tag : rest -> op tag rest
    _ -> fail "an operation is an array whose first element is its name"
    where
      op :: Text -> [Value] -> Parser Op
      op tag rest = case (tag, rest) of
        ("clear", []) -> pure Clear
        ("create", [n, ns, name]) -> Create <$> parseJSON n <*> parseJSON ns <*> parseJSON name
        ("text", [n, t]) -> CreateText <$> parseJSON n <*> parseJSON t
        ("set-text", [n, t]) -> SetText <$> parseJSON n <*> parseJSON t
        ("insert", [p, ix, c]) -> Insert <$> parseJSON p <*> parseJSON ix <*> parseJSON c
        ("remove", [p, c]) -> Remove <$> parseJSON p <*> parseJSON c
        ("attr", [n, ns, name, v]) -> SetAttribute <$> parseJSON n <*> parseJSON ns <*> parseJSON name <*> parseJSON v
        ("unattr", [n, ns, name]) -> RemoveAttribute <$> parseJSON n <*> parseJSON ns <*> parseJSON name
        ("prop", [n, name, v]) -> SetProperty <$> parseJSON n <*> parseJSON name <*> unscalar v
        ("unprop", [n, name]) -> RemoveProperty <$> parseJSON n <*> parseJSON name
        ("listen", [n, ty, l]) -> Listen <$> parseJSON n <*> parseJSON ty <*> parseJSON l
        ("unlisten", [n, ty, l]) -> Unlisten <$> parseJSON n <*> parseJSON ty <*> parseJSON l
        ("options", [n, ty, l, o]) -> ListenerOptions <$> parseJSON n <*> parseJSON ty <*> parseJSON l <*> decodeOptions o
        ("call", [n, method, arguments]) -> Call <$> parseJSON n <*> parseJSON method <*> parseJSON arguments
        ("set", [n, name, value]) -> Assign <$> parseJSON n <*> parseJSON name <*> pure value
        ("history", [String "push", address]) -> HistoryOp . Push <$> parseJSON address
        ("history", [String "replace", address]) -> HistoryOp . Replace <$> parseJSON address
        ("history", [String "go", n]) -> HistoryOp . Go <$> parseJSON n
        ("storage", [String k, key, value]) -> StorageOp <$> kind k <*> parseJSON key <*> parseJSON value
        _ -> fail ("not an operation: " <> toS tag <> " with " <> show (length rest) <> " arguments")

      unscalar :: Value -> Parser PropScalar
      unscalar = \case
        Number x -> pure (either ScalarNum ScalarInt (floatingOrInteger x))
        Bool b -> pure (ScalarBool b)
        String t -> pure (ScalarText t)
        _ -> fail "a property's value is a string, a number or a boolean"

-- | A listener's options, as the module's header spells them.
encodeOptions :: HandlerOptions -> Value
encodeOptions options =
  Aeson.object
    [ "preventDefault" .= options.preventDefault
    , "stopPropagation" .= options.stopPropagation
    , "match" .= fmap match options.matching
    ]
  where
    match = \case
      AnyEvent -> String "any"
      PlainLeftClick -> String "plain-left-click"
      Keys ks -> Array (Vector.fromList (map keyOf ks))
    keyOf k =
      Aeson.object $
        ("key" .= k.key)
          : [ name .= held
            | (name, Just held) <- [("ctrlKey", k.ctrlKey), ("shiftKey", k.shiftKey), ("altKey", k.altKey), ("metaKey", k.metaKey)]
            ]

decodeOptions :: Value -> Parser HandlerOptions
decodeOptions = Aeson.withObject "HandlerOptions" $ \o ->
  HandlerOptions
    <$> o
    .: "preventDefault"
    <*> o .: "stopPropagation"
    <*> (traverse match =<< o .:? "match")
  where
    match = \case
      String "any" -> pure AnyEvent
      String "plain-left-click" -> pure PlainLeftClick
      Array ks -> Keys <$> traverse keyOf (Vector.toList ks)
      _ -> fail "a match is \"any\", \"plain-left-click\" or a list of keys"
    keyOf = Aeson.withObject "KeyMatch" $ \k ->
      KeyMatch <$> k .: "key" <*> k .:? "ctrlKey" <*> k .:? "shiftKey" <*> k .:? "altKey" <*> k .:? "metaKey"

instance ToJSON Batch where
  toJSON batch = Aeson.object ["n" .= batch.sequence, "ops" .= batch.ops]

instance FromJSON Batch where
  parseJSON = Aeson.withObject "Batch" $ \o -> Batch <$> o .: "n" <*> o .: "ops"

instance ToJSON EventMessage where
  toJSON m =
    Aeson.object
      [ "listener" .= m.listener
      , "node" .= m.node
      , "type" .= m.eventType
      , "event" .= m.event
      ]

instance FromJSON EventMessage where
  parseJSON = Aeson.withObject "EventMessage" $ \o ->
    EventMessage <$> o .: "listener" <*> o .: "node" <*> o .: "type" <*> o .: "event"
