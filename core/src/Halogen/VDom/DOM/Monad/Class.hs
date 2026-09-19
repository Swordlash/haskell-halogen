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
  , mouseHandler
  )
where

import Control.Monad.Primitive (PrimMonad)
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

  -- | Likewise for event targets. Also monadic, and for the same reason.
  elementToEventTarget :: DomElement m -> m (DomEventTarget m)

  createTextNode :: Text -> DomDocument m -> m (DomNode m)
  setTextContent :: Text -> DomNode m -> m ()
  createElement :: Maybe Namespace -> ElemName -> DomDocument m -> m (DomElement m)

  -- The parent of each of these is a node, not a distinct ParentNode: that
  -- distinction is a browser coercion, and backends that lack it should not
  -- have to invent one.
  insertBefore :: DomNode m -> DomNode m -> DomNode m -> m ()
  appendChild :: DomNode m -> DomNode m -> m ()
  replaceChild :: DomNode m -> DomNode m -> DomNode m -> m ()
  insertChildIx :: Int -> DomNode m -> DomNode m -> m ()
  removeChild :: DomNode m -> DomNode m -> m ()
  parentNode :: DomNode m -> m (Maybe (DomNode m))
  nextSibling :: DomNode m -> m (Maybe (DomNode m))

  addEventListener :: EventType -> DomEventListener m -> DomEventTarget m -> m ()
  removeEventListener :: EventType -> DomEventListener m -> DomEventTarget m -> m ()

-- | Elements configured by named attributes and properties.
--
-- Split out for the same reason as 'MonadBrowserDOM': the reconciler never
-- touches these, "Halogen.VDom.DOM.Prop" does. A backend whose elements are
-- not configured this way should not have to stub six methods to be a tree.
-- A Pixi @Graphics@ is the case in point — it is configured by replaying a
-- drawing command sequence, not by setting named string attributes.
class (MonadDOM m) => MonadAttributes m where
  setAttribute :: Maybe Namespace -> AttrName -> Text -> DomElement m -> m ()
  setProperty :: PropName a -> PropValue a -> DomElement m -> m ()
  propertyEquals :: PropName a -> PropValue a -> DomElement m -> m Bool
  removeProperty :: PropName a -> DomElement m -> m ()
  removeAttribute :: Maybe Namespace -> AttrName -> DomElement m -> m ()
  hasAttribute :: Maybe Namespace -> AttrName -> DomElement m -> m Bool

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
  window :: m Window
  windowToEventTarget :: Window -> m (DomEventTarget m)
  document :: Window -> m HTMLDocument
  documentToNode :: HTMLDocument -> m (DomNode m)
  querySelector :: QuerySelector -> DomNode m -> m (Maybe (DomElement m))
  readyState :: HTMLDocument -> m ReadyState

mouseHandler :: (MouseEvent -> a) -> Event -> a
mouseHandler = coerce
