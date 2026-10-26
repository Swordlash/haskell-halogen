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

-- | Gives the element a piece of HTML is more properties, before its own.
--
-- Of two properties with the same name (two attributes @title@, two
-- handlers of @click@), the reconciler keeps the last
-- ("Halogen.VDom.DOM.Prop"), so the element's own win over these: this adds
-- what the element does not say, and never changes what it does. That is
-- what makes it safe to apply to HTML someone else built, which is what it
-- is for: the tutorial marks each element of the reader's code with where it
-- was built by wrapping the element's whole application in it, whatever its
-- properties are or however they are written.
--
-- A text node and a widget (a slot) have no properties and are left as they
-- are. A grafted piece ('VDom.Grafted') is grafted first, so that the
-- properties go on the element under it.
withProps :: forall w i. [Prop i] -> HTML w i -> HTML w i
withProps extra (HTML vdom) = HTML (go vdom)
  where
    added = map (fmap Action) extra
    go = \case
      VDom.Elem ns name props children -> VDom.Elem ns name (added <> props) children
      VDom.Keyed ns name props children -> VDom.Keyed ns name (added <> props) children
      VDom.Grafted graft -> go (VDom.runGraft graft)
      other -> other

-- | Create a HTML property.
prop :: forall value i. (IsProp value) => PropName value -> value -> Prop i
prop name = Property name . toPropValue

-- | Create a HTML attribute.
attr :: Maybe VDom.Namespace -> AttrName -> Text -> Prop i
attr = Attribute

-- | Create an event handler, which leaves the event as it is.
handler :: EventType -> (Event -> Maybe i) -> Prop i
handler et = Handler et handlerOptions

-- | Change what a handler does to its event in the listener itself
-- ('HandlerOptions'); any other property is left as it is.
withHandlerOptions :: (HandlerOptions -> HandlerOptions) -> Prop i -> Prop i
withHandlerOptions change = \case
  Handler et options f -> Handler et (change options) f
  other -> other

-- | A property that hears of its element being created and removed.
--
-- The removed element is passed on, not just the fact of a removal: when a
-- keyed element is replaced by one under a new key, the new one is created
-- before the old one is removed, and whoever keeps the element needs to tell
-- the removal of the old one from that of the one it holds.
ref :: forall i. (ElemRef Element -> Maybe i) -> Prop i
ref = Ref

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
