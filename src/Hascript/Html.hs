module Hascript.Html where

import Protolude
import Hascript.Html.Prop
import Web.DOM.Types

data Html msg w
  = Text !Text
  | Element !(Maybe Namespace) !ElemName [Prop msg] [Html msg w]
  | Component w
  | Grafted (Graft msg w)
  deriving Functor

instance Bifunctor Html where
  bimap f g = \case
    Text s -> Text s
    Element ns'm en props children -> Element ns'm en (map (fmap f) props) (map (bimap f g) children)
    Component w -> Component $ g w
    Grafted graft -> Grafted $ bimap f g graft
  
data Graft msg w = forall msg' w'. Graft (msg' -> msg) (w' -> w) (Html msg' w')

deriving instance Functor (Graft msg)

instance Bifunctor Graft where
  bimap f g (Graft fm wm v) = Graft (f . fm) (g . wm) v


runGraft :: Graft msg w -> Html msg w
runGraft (Graft fm fw v) = bimap fm fw v

renderComponent :: (msg -> msg') -> (w -> Html msg' w') -> Html msg w -> Html msg' w'
renderComponent fm injComponent = \case
  Text txt -> Text txt
  Element ns'm en props children -> 
    Element ns'm en (map (fmap fm) props) (map (renderComponent fm injComponent) children)
  Component w -> injComponent w
  Grafted graft -> renderComponent fm injComponent (runGraft graft)