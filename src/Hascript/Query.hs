module Hascript.Query where

import Protolude
import Data.Functor.Yoneda
import Data.Row
import Hascript.Data.Slot
import Web.DOM.Monad

data ChildQuery (ps :: Row Type) (a :: Type) where
  ChildQuery
    :: (forall slot m. Applicative m => (slot g o -> m (Maybe b)) -> SlotStorage ps slot -> m (f b))
    -> (g b)
    -> (f b -> a)
    -> ChildQuery ps a

deriving instance Functor (ChildQuery ps)

newtype RefLabel = RefLabel Text

data Input msg m
  = Action msg
  | RefUpdate RefLabel (Maybe (Element m))

data QueryF query msg input a
  = Initialize a
  | Finalize a
  | Receive input a
  | Message msg a
  | Query (Yoneda query a) (() -> a)
  deriving Functor

instance Bifunctor (QueryF query msg) where
  bimap f g = \case
    Initialize a -> Initialize (g a)
    Finalize a -> Finalize (g a)
    Receive i a -> Receive (f i) (g a)
    Message msg a -> Message msg (g a)
    Query y g' -> Query (fmap g y) (g . g')