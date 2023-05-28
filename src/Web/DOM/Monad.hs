module Web.DOM.Monad where

import Protolude
import Web.DOM.Types

class Monad m => MonadDOM m where
  data EventListener m
  data Node m
  data Element m
  data Document m

  data Event m
  data MouseEvent m

  mkEventListener     :: (Event m -> m a) -> m (EventListener m)

  unsafeRefEq         :: a -> b -> m Bool
  createTextNode      :: Text -> Document m -> m (Node m)
  setTextContent      :: Text -> Node m -> m ()
  createElement       :: Maybe Namespace -> ElemName -> Document m -> m (Element m)
  insertChildIx       :: Int -> Node m -> Node m -> m ()
  removeChild         :: Node m -> Node m -> m ()
  parentNode          :: Node m -> m (Node m)
  setAttribute        :: Maybe Namespace -> Text -> Text -> Element m -> m ()
  removeAttribute     :: Maybe Namespace -> Text -> Element m -> m ()
  hasAttribute        :: Maybe Namespace -> Text -> Element m -> m Bool
  addEventListener    :: Text -> EventListener m -> Element m -> m ()
  removeEventListener :: Text -> EventListener m -> Element m -> m ()

  mouseHandler :: (MouseEvent m -> a) -> Event m -> a
  elementToNode :: Element m -> Node m