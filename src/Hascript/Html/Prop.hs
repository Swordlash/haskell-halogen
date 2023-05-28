module Hascript.Html.Prop where

import Protolude
import Web.DOM.Types
import Web.DOM.Monad

data PropValue
  = IntProp !Integer
  | NumProp !Double
  | BoolProp !Bool
  | TxtProp !Text

data Prop msg
  = Attribute !(Maybe Namespace) !AttrName !Text
  | Property !PropName !PropValue
  | forall m. Handler !EventType !(Event m -> Maybe msg)

deriving instance Functor Prop

class IsPropValue a where
  propValue :: a -> PropValue

instance IsPropValue Text where
  propValue = TxtProp

instance IsPropValue Int where
  propValue = IntProp . fromIntegral