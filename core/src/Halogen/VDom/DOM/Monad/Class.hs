-- | The DOM interface the VDom machinery is written against.
--
-- This module holds only the class and the arch-independent helpers. The
-- @instance MonadDOM IO@ lives in one of "Halogen.VDom.DOM.Monad.JS",
-- "Halogen.VDom.DOM.Monad.WASM" or "Halogen.VDom.DOM.Monad.Native", exactly one
-- of which is compiled into the package (see the @if arch@ blocks in the cabal
-- file). Import "Halogen.VDom.DOM.Monad" to get the class together with
-- whichever backend this build selected.
module Halogen.VDom.DOM.Monad.Class
  ( PropValue (..)
  , MonadDOM (..)
  , mouseHandler
  , elementToNode
  , toEventTarget
  )
where

import HPrelude
import Halogen.VDom.Types
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Internal.Types
import Web.DOM.ParentNode
import Web.Event.Event
import Web.HTML.Common
import Web.HTML.HTMLDocument.ReadyState as ReadyState
import Web.UIEvent.MouseEvent

data PropValue val where
  IntProp :: (Integral a) => a -> PropValue a
  NumProp :: Double -> PropValue Double
  BoolProp :: Bool -> PropValue Bool
  TxtProp :: Text -> PropValue Text
  ViaTxtProp :: (a -> Text) -> a -> PropValue a

class (Monad m) => MonadDOM m where
  -- | Mutable cells, abstracted so the class can be instantiated at monads
  -- with no 'IO' underneath.
  --
  -- "Halogen.VDom.DOM.Prop" needs one cell per event handler: a listener is
  -- registered with the DOM once and its target is then swapped on every
  -- patch, so the cell is what keeps the listener's identity stable. That is
  -- a real requirement, not an implementation detail, which is why it belongs
  -- in the class rather than in an @IO@-shaped constraint on the caller.
  type Ref m :: Type -> Type

  newRef :: a -> m (Ref m a)
  readRef :: Ref m a -> m a
  writeRef :: Ref m a -> a -> m ()
  modifyRef' :: Ref m a -> (a -> a) -> m ()

  mkEventListener :: (Event -> m ()) -> m EventListener

  createTextNode :: Text -> Document -> m Node
  setTextContent :: Text -> Node -> m ()
  createElement :: Maybe Namespace -> ElemName -> Document -> m Element
  insertBefore :: Node -> Node -> ParentNode -> m ()
  appendChild :: Node -> ParentNode -> m ()
  replaceChild :: Node -> Node -> ParentNode -> m ()
  insertChildIx :: Int -> Node -> ParentNode -> m ()
  removeChild :: Node -> ParentNode -> m ()
  parentNode :: Node -> m (Maybe ParentNode)
  nextSibling :: Node -> m (Maybe Node)
  setAttribute :: Maybe Namespace -> AttrName -> Text -> Element -> m ()
  setProperty :: PropName a -> PropValue a -> Element -> m ()
  propertyEquals :: PropName a -> PropValue a -> Element -> m Bool
  removeProperty :: PropName a -> Element -> m ()
  removeAttribute :: Maybe Namespace -> AttrName -> Element -> m ()
  hasAttribute :: Maybe Namespace -> AttrName -> Element -> m Bool

  addEventListener :: EventType -> EventListener -> EventTarget -> m ()
  removeEventListener :: EventType -> EventListener -> EventTarget -> m ()

  window :: m Window
  document :: Window -> m HTMLDocument

  querySelector :: QuerySelector -> ParentNode -> m (Maybe Element)
  readyState :: HTMLDocument -> m ReadyState
  log :: Text -> m ()

mouseHandler :: (MouseEvent -> a) -> Event -> a
mouseHandler = coerce

elementToNode :: Element -> Node
elementToNode = coerce

toEventTarget :: a -> EventTarget
toEventTarget = unsafeCoerce
