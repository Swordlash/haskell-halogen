module Halogen.Query.ChildQuery where

import Data.Row
import Halogen.Data.Slot
import Protolude

data ChildQuery (ps :: Row Type) (a :: Type) where
  ChildQuery
    :: (forall slot m. (Applicative m) => (slot g o -> m (Maybe b)) -> SlotStorage ps slot -> m (f b))
    -> (g b)
    -> (f b -> a)
    -> ChildQuery ps a

deriving instance Functor (ChildQuery ps)
