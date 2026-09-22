-- | The DOM interface the VDom machinery is written against.
--
-- The node and element types are associated rather than fixed to
-- "Web.DOM.Internal.Types", so a backend can say what its own tree is made
-- of. That is only possible because each backend has its own monad — while
-- the sole instance was at 'IO' there was nothing for the association to
-- range over.
--
-- The browser-shaped operations live in 'MonadBrowserDOM'. The reconciler
-- uses none of them, and they have no meaning for an in-memory tree or a
-- canvas scene graph, so making them a separate class keeps a backend from
-- having to stub them.
module Halogen.VDom.DOM.Monad.Class
  ( PropValue (..)
  , MonadDOM (..)
  , MonadAttributes (..)
  , MonadBrowserDOM (..)
  , StorageKind (..)
  , storagePrefix
  , mouseHandler
  )
where

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Identity (IdentityT (..))
import HPrelude
import Halogen.VDom.Types
import Web.DOM.Internal.Types (Document, Element, HTMLDocument, Node, Window)
import Web.DOM.ParentNode (QuerySelector)
import Web.Event.Event (Event, EventType)
import Web.Event.Internal.Types (EventTarget)
import Web.HTML.Common
import Web.HTML.HTMLDocument.ReadyState as ReadyState
import Web.UIEvent.MouseEvent

data PropValue val where
  IntProp :: (Integral a) => a -> PropValue a
  NumProp :: Double -> PropValue Double
  BoolProp :: Bool -> PropValue Bool
  TxtProp :: Text -> PropValue Text
  ViaTxtProp :: (a -> Text) -> a -> PropValue a

-- | 'PrimMonad' is a superclass because "Halogen.VDom.DOM.Prop" needs one
-- mutable cell per event handler: a listener is registered with the DOM once
-- and its target is then swapped on every patch, so the cell is what keeps
-- the listener's identity stable across a patch.
--
-- Spelling that as 'MonadIO' would rule out a backend with no 'IO'
-- underneath; 'PrimMonad' asks for exactly what is needed and is satisfied by
-- both 'IO' and @ST s@.
class (PrimMonad m) => MonadDOM m where
  -- | What this backend's tree is made of.
  --
  -- The browser backends set these to the "Web.DOM.Internal.Types" newtypes;
  -- an in-memory backend can use ordinary Haskell data, and a canvas backend
  -- its own display objects.
  type DomNode m

  type DomElement m
  type DomDocument m
  type DomEventListener m
  type DomEventTarget m

  mkEventListener :: (Event -> m ()) -> m (DomEventListener m)

  -- | Every element is a node. A free 'coerce' for the browser backends, but
  -- not something the class can assume of a backend in general.
  --
  -- In the monad rather than pure only so that @m@ is determined: 'Element'
  -- and 'Node' are non-injective, so a pure @DomElement m -> DomNode m@ could never
  -- be resolved at a call site.
  elementToNode :: DomElement m -> m (DomNode m)
  default elementToNode :: (LiftsDOM t n m) => DomElement m -> m (DomNode m)
  elementToNode = lift . elementToNode

  -- | Likewise for event targets. Also monadic, and for the same reason.
  elementToEventTarget :: DomElement m -> m (DomEventTarget m)
  default elementToEventTarget :: (LiftsDOM t n m) => DomElement m -> m (DomEventTarget m)
  elementToEventTarget = lift . elementToEventTarget

  createTextNode :: Text -> DomDocument m -> m (DomNode m)
  default createTextNode :: (LiftsDOM t n m) => Text -> DomDocument m -> m (DomNode m)
  createTextNode value doc = lift (createTextNode value doc)

  setTextContent :: Text -> DomNode m -> m ()
  default setTextContent :: (LiftsDOM t n m) => Text -> DomNode m -> m ()
  setTextContent value node = lift (setTextContent value node)

  createElement :: Maybe Namespace -> ElemName -> DomDocument m -> m (DomElement m)
  default createElement :: (LiftsDOM t n m) => Maybe Namespace -> ElemName -> DomDocument m -> m (DomElement m)
  createElement ns name doc = lift (createElement ns name doc)

  -- The parent here is a node, not a distinct ParentNode: that distinction is
  -- a browser coercion, and backends that lack it should not have to invent
  -- one.
  --
  -- These three are the whole of what the reconciler needs to mutate a tree.
  insertChildIx :: Int -> DomNode m -> DomNode m -> m ()
  default insertChildIx :: (LiftsDOM t n m) => Int -> DomNode m -> DomNode m -> m ()
  insertChildIx ix child parent = lift (insertChildIx ix child parent)

  removeChild :: DomNode m -> DomNode m -> m ()
  default removeChild :: (LiftsDOM t n m) => DomNode m -> DomNode m -> m ()
  removeChild child parent = lift (removeChild child parent)

  parentNode :: DomNode m -> m (Maybe (DomNode m))
  default parentNode :: (LiftsDOM t n m) => DomNode m -> m (Maybe (DomNode m))
  parentNode = lift . parentNode

  addEventListener :: EventType -> DomEventListener m -> DomEventTarget m -> m ()
  default addEventListener :: (LiftsDOM t n m) => EventType -> DomEventListener m -> DomEventTarget m -> m ()
  addEventListener ty listener target = lift (addEventListener ty listener target)

  removeEventListener :: EventType -> DomEventListener m -> DomEventTarget m -> m ()
  default removeEventListener :: (LiftsDOM t n m) => EventType -> DomEventListener m -> DomEventTarget m -> m ()
  removeEventListener ty listener target = lift (removeEventListener ty listener target)

-- | What a transformer over a 'MonadDOM' needs for the lifted defaults.
--
-- The tree is the one underneath: a transformer adds an effect to how a
-- backend is driven, never a new kind of node.
type LiftsDOM t n m =
  ( MonadTrans t
  , MonadDOM n
  , m ~ t n
  , DomNode m ~ DomNode n
  , DomElement m ~ DomElement n
  , DomDocument m ~ DomDocument n
  , DomEventListener m ~ DomEventListener n
  , DomEventTarget m ~ DomEventTarget n
  )

-- | Elements configured by named attributes and properties.
--
-- Split out for the same reason as 'MonadBrowserDOM': the reconciler never
-- touches these, "Halogen.VDom.DOM.Prop" does. A backend whose elements are
-- not configured this way should not have to stub six methods to be a tree.
-- A Pixi @Graphics@ is the case in point — it is configured by replaying a
-- drawing command sequence, not by setting named string attributes.
class (MonadDOM m) => MonadAttributes m where
  setAttribute :: Maybe Namespace -> AttrName -> Text -> DomElement m -> m ()
  default setAttribute :: (LiftsAttributes t n m) => Maybe Namespace -> AttrName -> Text -> DomElement m -> m ()
  setAttribute ns name value el = lift (setAttribute ns name value el)

  setProperty :: PropName a -> PropValue a -> DomElement m -> m ()
  default setProperty :: (LiftsAttributes t n m) => PropName a -> PropValue a -> DomElement m -> m ()
  setProperty name value el = lift (setProperty name value el)

  propertyEquals :: PropName a -> PropValue a -> DomElement m -> m Bool
  default propertyEquals :: (LiftsAttributes t n m) => PropName a -> PropValue a -> DomElement m -> m Bool
  propertyEquals name value el = lift (propertyEquals name value el)

  removeProperty :: PropName a -> DomElement m -> m ()
  default removeProperty :: (LiftsAttributes t n m) => PropName a -> DomElement m -> m ()
  removeProperty name el = lift (removeProperty name el)

  removeAttribute :: Maybe Namespace -> AttrName -> DomElement m -> m ()
  default removeAttribute :: (LiftsAttributes t n m) => Maybe Namespace -> AttrName -> DomElement m -> m ()
  removeAttribute ns name el = lift (removeAttribute ns name el)

  hasAttribute :: Maybe Namespace -> AttrName -> DomElement m -> m Bool
  default hasAttribute :: (LiftsAttributes t n m) => Maybe Namespace -> AttrName -> DomElement m -> m Bool
  hasAttribute ns name el = lift (hasAttribute ns name el)

-- | 'LiftsDOM', for a backend that also has attributes.
type LiftsAttributes t n m = (LiftsDOM t n m, MonadAttributes n)

-- | The parts of the DOM that only a browser has.
--
-- Split out of 'MonadDOM' because the reconciler uses none of them: it is
-- 'Halogen.VDom.Driver' that needs a document to build against and a ready
-- state to wait for. An in-memory or canvas backend can implement 'MonadDOM'
-- and stop there.
-- The equalities are superclasses rather than constraints repeated at each
-- use site: a browser backend is by definition one whose tree is made of the
-- "Web.DOM.Internal.Types" newtypes, and saying so once here keeps every
-- caller of 'awaitBody' and friends from having to restate it.
class
  ( MonadAttributes m
  , DomNode m ~ Node
  , DomElement m ~ Element
  , DomDocument m ~ Document
  , DomEventTarget m ~ EventTarget
  ) =>
  MonadBrowserDOM m
  where
  -- Splicing a rendered root into a document that already exists. The
  -- reconciler never does this - it owns its whole subtree - so these live
  -- here rather than in 'MonadDOM', where a canvas backend that mounts onto a
  -- stage would have to invent them.
  insertBefore :: DomNode m -> DomNode m -> DomNode m -> m ()
  default insertBefore :: (LiftsBrowserDOM t n m) => DomNode m -> DomNode m -> DomNode m -> m ()
  insertBefore node sib parent = lift (insertBefore node sib parent)

  appendChild :: DomNode m -> DomNode m -> m ()
  default appendChild :: (LiftsBrowserDOM t n m) => DomNode m -> DomNode m -> m ()
  appendChild node parent = lift (appendChild node parent)

  replaceChild :: DomNode m -> DomNode m -> DomNode m -> m ()
  default replaceChild :: (LiftsBrowserDOM t n m) => DomNode m -> DomNode m -> DomNode m -> m ()
  replaceChild node old parent = lift (replaceChild node old parent)

  nextSibling :: DomNode m -> m (Maybe (DomNode m))
  default nextSibling :: (LiftsBrowserDOM t n m) => DomNode m -> m (Maybe (DomNode m))
  nextSibling = lift . nextSibling

  window :: m Window
  default window :: (LiftsBrowserDOM t n m) => m Window
  window = lift window

  windowToEventTarget :: Window -> m (DomEventTarget m)
  default windowToEventTarget :: (LiftsBrowserDOM t n m) => Window -> m (DomEventTarget m)
  windowToEventTarget = lift . windowToEventTarget

  document :: Window -> m HTMLDocument
  default document :: (LiftsBrowserDOM t n m) => Window -> m HTMLDocument
  document = lift . document

  documentToNode :: HTMLDocument -> m (DomNode m)
  default documentToNode :: (LiftsBrowserDOM t n m) => HTMLDocument -> m (DomNode m)
  documentToNode = lift . documentToNode

  querySelector :: QuerySelector -> DomNode m -> m (Maybe (DomElement m))
  default querySelector :: (LiftsBrowserDOM t n m) => QuerySelector -> DomNode m -> m (Maybe (DomElement m))
  querySelector sel node = lift (querySelector sel node)

  readyState :: HTMLDocument -> m ReadyState
  default readyState :: (LiftsBrowserDOM t n m) => HTMLDocument -> m ReadyState
  readyState = lift . readyState

  -- What a store keeps under one key, as text, and nothing where it keeps
  -- nothing. A backend is asked for nothing more than that:
  -- "Web.Storage.Storage" is what says the text is base64 and what the bytes
  -- under it mean, and it says it once for every backend rather than once per
  -- backend.
  --
  -- One key at a time, rather than the store as a whole, because the store is
  -- shared: a tab that read every key, changed one and wrote them all back
  -- would undo whatever another tab had written in between.
  readStorageItem :: StorageKind -> Text -> m (Maybe Text)
  default readStorageItem :: (LiftsBrowserDOM t n m) => StorageKind -> Text -> m (Maybe Text)
  readStorageItem kind key = lift (readStorageItem kind key)

  writeStorageItem :: StorageKind -> Text -> Text -> m ()
  default writeStorageItem :: (LiftsBrowserDOM t n m) => StorageKind -> Text -> Text -> m ()
  writeStorageItem kind key text = lift (writeStorageItem kind key text)

  removeStorageItem :: StorageKind -> Text -> m ()
  default removeStorageItem :: (LiftsBrowserDOM t n m) => StorageKind -> Text -> m ()
  removeStorageItem kind key = lift (removeStorageItem kind key)

  -- Every key the store holds, this program's and anything else's. Which of
  -- them are this program's is 'Web.Storage.Storage.keys', which knows the
  -- prefix they are kept under.
  storageItemKeys :: StorageKind -> m [Text]
  default storageItemKeys :: (LiftsBrowserDOM t n m) => StorageKind -> m [Text]
  storageItemKeys = lift . storageItemKeys

-- | Which of the browser's two stores is meant. They differ only in how long
-- what is written to them lasts: a session store is emptied when the tab is
-- closed, a local one is not.
data StorageKind
  = LocalStorage
  | SessionStorage
  deriving stock (Eq, Ord, Show)

-- | What a browser keeps this program's entries under: a prefix on every key
-- it writes.
--
-- A store belongs to an origin rather than to a page, so what is in it was not
-- necessarily put there by this program. The prefix is how the two are told
-- apart: see "Web.Storage.Storage".
storagePrefix :: Text
storagePrefix = "haskell-halogen:"

-- | 'LiftsAttributes', for a backend that is a browser.
type LiftsBrowserDOM t n m = (LiftsAttributes t n m, MonadBrowserDOM n)

--------------------------------------------------------------------------------

-- | A transformer over a DOM is a DOM.
--
-- Only 'mkEventListener' has to be written out: the DOM calls a listener
-- back, so the handler has to be /run/ rather than lifted, and running it
-- means discharging whatever the transformer added. That is why 'ReaderT'
-- and 'IdentityT' are here and 'StateT' is not — a state update made inside
-- a callback has nowhere to go.
instance (MonadDOM m) => MonadDOM (ReaderT r m) where
  type DomNode (ReaderT r m) = DomNode m
  type DomElement (ReaderT r m) = DomElement m
  type DomDocument (ReaderT r m) = DomDocument m
  type DomEventListener (ReaderT r m) = DomEventListener m
  type DomEventTarget (ReaderT r m) = DomEventTarget m

  mkEventListener f = ReaderT $ \r -> mkEventListener (\ev -> runReaderT (f ev) r)

instance (MonadAttributes m) => MonadAttributes (ReaderT r m)

instance (MonadBrowserDOM m) => MonadBrowserDOM (ReaderT r m)

instance (MonadDOM m) => MonadDOM (IdentityT m) where
  type DomNode (IdentityT m) = DomNode m
  type DomElement (IdentityT m) = DomElement m
  type DomDocument (IdentityT m) = DomDocument m
  type DomEventListener (IdentityT m) = DomEventListener m
  type DomEventTarget (IdentityT m) = DomEventTarget m

  mkEventListener f = IdentityT $ mkEventListener (runIdentityT . f)

instance (MonadAttributes m) => MonadAttributes (IdentityT m)

instance (MonadBrowserDOM m) => MonadBrowserDOM (IdentityT m)

mouseHandler :: (MouseEvent -> a) -> Event -> a
mouseHandler = coerce
