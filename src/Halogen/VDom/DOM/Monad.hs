{-# LANGUAGE CPP #-}
module Halogen.VDom.DOM.Monad where

import Halogen.VDom.Types
import Protolude
import Unsafe.Coerce (unsafeCoerce)
import Web.UIEvent.MouseEvent
import Web.Event.Event
import Web.HTML.Common
import Web.DOM.Element
import Data.Foreign

newtype EventListener = EventListener (Foreign EventListener)
newtype Node = Node (Foreign Node)
newtype Document = Element (Foreign Document)

class Monad m => MonadDOM m where
  mkEventListener :: (Event -> m a) -> m EventListener

  createTextNode :: Text -> Document -> m Node
  setTextContent :: Text -> Node -> m ()
  createElement :: Maybe Namespace -> ElemName -> Document -> m Element
  insertChildIx :: Int -> Node -> Node -> m ()
  removeChild :: Node -> Node -> m ()
  parentNode :: Node -> m Node 
  setAttribute :: Maybe Namespace -> AttrName -> Text -> Element -> m ()
  removeAttribute :: Maybe Namespace -> AttrName -> Element -> m ()
  hasAttribute :: Maybe Namespace -> AttrName -> Element -> m Bool

  addEventListener :: EventType -> EventListener -> EventTarget -> m ()
  removeEventListener :: EventType -> EventListener -> EventTarget -> m ()

mouseHandler :: (MouseEvent -> a) -> Event -> a
mouseHandler = unsafeCoerce

elementToNode :: Element -> Node
elementToNode = unsafeCoerce
