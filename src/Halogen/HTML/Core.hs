module Halogen.HTML.Core (module Halogen.HTML.Core, module Web.HTML.Common, Namespace (..), ElemName (..)) where

import DOM.HTML.Indexed
import Data.MediaType
import HPrelude
import Halogen.Query.Input
import Halogen.VDom.DOM.Prop
import Halogen.VDom.Types as VDom
import Web.DOM.Element
import Web.Event.Event
import Web.HTML.Common

newtype HTML w i = HTML {unHTML :: VDom [Prop (Input i)] w}

unHTML :: HTML w i -> VDom [Prop (Input i)] w
unHTML = coerce

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

-- | A smart constructor for HTML elements with keyed children.
keyed :: forall w i. Maybe VDom.Namespace -> VDom.ElemName -> [Prop i] -> [(Text, HTML w i)] -> HTML w i
keyed ns name props children = HTML $ VDom.Keyed ns name (map (fmap Action) props) (map (second unHTML) children)

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
  Ref
    $ f
    . \case
      Created x -> Just x
      Removed _ -> Nothing

class IsProp a where
  toPropValue :: a -> PropValue a

instance {-# OVERLAPPABLE #-} (Integral a) => IsProp a where toPropValue = IntProp

instance {-# OVERLAPPING #-} IsProp Text where toPropValue = TxtProp

instance {-# OVERLAPPING #-} IsProp Double where toPropValue = NumProp

instance {-# OVERLAPPING #-} IsProp Bool where toPropValue = BoolProp

instance IsProp MediaType where
  toPropValue = ViaTxtProp coerce

instance IsProp ButtonType where
  toPropValue = ViaTxtProp renderButtonType

instance IsProp CrossOriginValue where
  toPropValue = ViaTxtProp renderCrossOriginValue

instance IsProp DirValue where
  toPropValue = ViaTxtProp renderDirValue

instance IsProp FormMethod where
  toPropValue = ViaTxtProp renderFormMethod

instance IsProp InputType where
  toPropValue = ViaTxtProp renderInputType

instance IsProp KindValue where
  toPropValue = ViaTxtProp renderKindValue

instance IsProp MenuitemType where
  toPropValue = ViaTxtProp renderMenuitemType

instance IsProp MenuType where
  toPropValue = ViaTxtProp renderMenuType

instance IsProp AutocompleteType where
  toPropValue = ViaTxtProp renderAutocompleteType

instance IsProp OrderedListType where
  toPropValue = ViaTxtProp renderOrderedListType

instance IsProp PreloadValue where
  toPropValue = ViaTxtProp renderPreloadValue

instance IsProp ScopeValue where
  toPropValue = ViaTxtProp renderScopeValue

instance IsProp StepValue where
  toPropValue = ViaTxtProp renderStepValue

instance IsProp WrapValue where
  toPropValue = ViaTxtProp renderWrapValue

instance IsProp InputAcceptType where
  toPropValue = ViaTxtProp renderInputAcceptType
