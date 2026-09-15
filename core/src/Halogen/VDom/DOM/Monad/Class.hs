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
