module Halogen.VDom.DOM.Monad.Common where

import HPrelude
import Halogen.VDom.Types
import Web.DOM.Element
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
  unsafeGetProperty :: PropName a -> Element -> m a
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
