module Halogen.Query.HalogenQ where

import Data.Functor.Coyoneda
import Protolude

data HalogenQ query msg input a
  = Initialize a
  | Finalize a
  | Receive input a
  | Message msg a
  | Query (Coyoneda query a) (() -> a)
  deriving (Functor)

instance Bifunctor (HalogenQ query msg) where
  bimap f g = \case
    Initialize a -> Initialize (g a)
    Finalize a -> Finalize (g a)
    Receive i a -> Receive (f i) (g a)
    Message msg a -> Message msg (g a)
    Query y g' -> Query (fmap g y) (g . g')
