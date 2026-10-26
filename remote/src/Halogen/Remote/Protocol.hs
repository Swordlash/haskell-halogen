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
--
-- Node 0 is the window (what the component's program knows as both the
-- window and the document) and node 1 the container the client attached
-- to (the component's @body@, which it is mounted into); the session never
-- creates either. @insert@'s index is where the child ends up, counted
-- among the parent's children once the child has been taken from wherever
-- it was.
--
-- A node taken out and not put back by the end of the batch is gone, on both
-- sides: once the last operation is applied, each node a @remove@ took out
-- that still has no parent is forgotten, with everything under it and the
-- listeners on them, and no operation names it again. A node taken out and
-- put back by an @insert@ in the same batch stays, as the node of a child
-- component does when its parent replaces the element around it, and a
-- batch never ends in the middle of a render. A batch says nothing about
-- what happens inside a node it leaves gone (its children taken out of it,
-- its listeners stopped): the client forgets all of it anyway.
--
-- __To the program__, one JSON object per event a listener heard:
--
-- > {"listener": 7, "node": 4, "type": "input",
-- >  "event": {"type": "input", "target": {"value": "hi", ...},
-- >            "currentTarget": {"value": "hi", ...}, ...}}
--
-- @event@ holds what handlers read of an event: its @type@, @key@,
-- @code@, the modifier keys, @button@, the pointer's coordinates,
-- @detail@, and for its @target@ and @currentTarget@ their @value@,
-- @checked@, @selectedIndex@ and @tagName@.
module Halogen.Remote.Protocol
  ( NodeId
  , ListenerId
  , Op (..)
  , Batch (..)
  , EventMessage (..)
  , encodeBatch
  , decodeEventMessage
  )
where

import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Parser)
import Data.Scientific (floatingOrInteger, fromFloatDigits)
import Data.Text.Lazy qualified as Lazy
import Data.Vector qualified as Vector
import Halogen.VDom.DOM.Monad (PropScalar (..))
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

encodeBatch :: Batch -> Text
encodeBatch = Lazy.toStrict . encodeToLazyText

decodeEventMessage :: Text -> Maybe EventMessage
decodeEventMessage = Aeson.decodeStrictText

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
        _ -> fail ("not an operation: " <> toS tag <> " with " <> show (length rest) <> " arguments")

      unscalar :: Value -> Parser PropScalar
      unscalar = \case
        Number x -> pure (either ScalarNum ScalarInt (floatingOrInteger x))
        Bool b -> pure (ScalarBool b)
        String t -> pure (ScalarText t)
        _ -> fail "a property's value is a string, a number or a boolean"

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
