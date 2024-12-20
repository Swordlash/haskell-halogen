module Halogen.HTML.Core (module Halogen.HTML.Core, module Web.HTML.Common, Namespace (..), ElemName (..)) where

import Halogen.Query.Input
import Halogen.VDom.DOM.Prop
import Halogen.VDom.Types as VDom
import Protolude hiding (Handler)
import Web.DOM.Element
import Web.Event.Event
import Web.HTML.Common

newtype HTML w i = HTML {unHTML :: VDom [Prop (Input i)] w}

instance Bifunctor HTML where
  bimap f g (HTML vdom) = HTML $ bimap (fmap (fmap (fmap g))) f vdom

instance Functor (HTML w) where
  fmap = second

renderWidget :: (i -> j) -> (w -> HTML x j) -> HTML w i -> HTML x j
renderWidget f g (HTML vdom) =
  HTML (VDom.renderWidget (map (map (map f))) ((.unHTML) . g) vdom)

widget :: p -> HTML p q
widget = HTML . VDom.Widget

-- | Constructs a text node `HTML` value.
text :: Text -> HTML w i
text = HTML . VDom.Text

-- | A smart constructor for HTML elements.
element :: forall w i. Maybe VDom.Namespace -> VDom.ElemName -> [Prop i] -> [HTML w i] -> HTML w i
element ns en props htmls = HTML $ VDom.Elem ns en (map (fmap Action) props) (fmap (.unHTML) htmls)

-- | Create a HTML property.
prop :: forall value i. (IsProp value) => PropName value -> value -> Prop i
prop name = Property name . toPropValue

-- | Create a HTML attribute.
attr :: Maybe VDom.Namespace -> AttrName -> Text -> Prop i
attr = Attribute

-- | Create an event handler.
handler :: EventType -> (Event -> Maybe i) -> Prop i
handler = Handler

ref :: forall i. (Maybe Element -> Maybe i) -> Prop i
ref f =
  Ref $
    f . \case
      Created x -> Just x
      Removed _ -> Nothing

class IsProp a where
  toPropValue :: a -> PropValue a

instance {-# OVERLAPPABLE #-} (Integral a) => IsProp a where toPropValue = IntProp

instance {-# OVERLAPPING #-} IsProp Text where toPropValue = TxtProp

instance {-# OVERLAPPING #-} IsProp Double where toPropValue = NumProp

instance {-# OVERLAPPING #-} IsProp Bool where toPropValue = BoolProp
