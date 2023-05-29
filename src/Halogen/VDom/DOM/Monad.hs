module Halogen.VDom.DOM.Monad where

import Halogen.VDom.Types
import Protolude
import Web.DOM.Types

class Monad m => MonadDOM m where
  data EventListener m
  data Node m
  data Element m
  data Document m

  data Event m
  data EventTarget m
  data MouseEvent m

  mkEventListener :: (Event m -> m a) -> m (EventListener m)

  unsafeRefEq :: a -> b -> m Bool
  createTextNode :: Text -> Document m -> m (Node m)
  setTextContent :: Text -> Node m -> m ()
  createElement :: Maybe Namespace -> ElemName -> Document m -> m (Element m)
  insertChildIx :: Int -> Node m -> Node m -> m ()
  removeChild :: Node m -> Node m -> m ()
  parentNode :: Node m -> m (Node m)
  setAttribute :: Maybe Namespace -> AttrName -> Text -> Element m -> m ()
  removeAttribute :: Maybe Namespace -> AttrName -> Element m -> m ()
  hasAttribute :: Maybe Namespace -> AttrName -> Element m -> m Bool

  addEventListener :: EventType -> EventListener m -> EventTarget m -> m ()
  removeEventListener :: EventType -> EventListener m -> EventTarget m -> m ()

  mouseHandler :: (MouseEvent m -> a) -> Event m -> a
  elementToNode :: Element m -> Node m
