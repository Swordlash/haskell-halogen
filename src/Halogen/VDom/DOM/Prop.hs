module Halogen.VDom.DOM.Prop (Prop(..), ElemRef(..), PropValue(..)) where

import Halogen.VDom.Types
import Protolude
import Web.HTML.Common
import Web.Event.Event
import Web.DOM.Element

data PropValue val where
  IntProp :: Integral a => !a -> PropValue a
  NumProp :: !Double -> PropValue Double
  BoolProp :: !Bool -> PropValue Bool
  TxtProp :: !Text -> PropValue Text

data Prop msg
  = Attribute !(Maybe Namespace) !AttrName !Text
  | forall val. Property !(PropName val) !(PropValue val)
  | Handler !EventType !(Event -> Maybe msg)
  | Ref (ElemRef Element -> Maybe msg)

deriving instance Functor Prop

data ElemRef a
  = Created a
  | Removed a
  deriving Functor
