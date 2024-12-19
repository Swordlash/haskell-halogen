{-# LANGUAGE CPP #-}

module Halogen.VDom.DOM.Monad where

import Halogen.VDom.Types
import Protolude
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element
import Web.DOM.Internal.Types
import Web.Event.Event
import Web.HTML.Common
import Web.UIEvent.MouseEvent

data PropValue val where
  IntProp :: (Integral a) => !a -> PropValue a
  NumProp :: !Double -> PropValue Double
  BoolProp :: !Bool -> PropValue Bool
  TxtProp :: !Text -> PropValue Text

class (Monad m) => MonadDOM m where
  mkEventListener :: (Event -> m a) -> m EventListener

  createTextNode :: Text -> Document -> m Node
  setTextContent :: Text -> Node -> m ()
  createElement :: Maybe Namespace -> ElemName -> Document -> m Element
  insertBefore :: Node -> Node -> Node -> m ()
  appendChild :: Node -> Node -> m ()
  replaceChild :: Node -> Node -> Node -> m ()
  insertChildIx :: Int -> Node -> Node -> m ()
  removeChild :: Node -> Node -> m ()
  parentNode :: Node -> m (Maybe Node)
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

mouseHandler :: (MouseEvent -> a) -> Event -> a
mouseHandler = unsafeCoerce

elementToNode :: Element -> Node
elementToNode = unsafeCoerce

toEventTarget :: a -> EventTarget
toEventTarget = unsafeCoerce
