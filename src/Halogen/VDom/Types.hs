module Halogen.VDom.Types where

import Protolude

newtype ElemName = ElemName Text
  deriving (Eq, Ord, Show, IsString)

newtype Namespace = Namespace Text
  deriving (Eq, Ord, Show, IsString)

data VDOM a w
  = Text !Text
  | Element !(Maybe Namespace) !ElemName a [VDOM a w]
  | Component w
  | Grafted (Graft a w)
  deriving (Functor)

instance Bifunctor VDOM where
  bimap f g = \case
    Text s -> Text s
    Element ns'm en props children -> Element ns'm en (f props) (map (bimap f g) children)
    Component w -> Component $ g w
    Grafted graft -> Grafted $ bimap f g graft

data Graft a w = forall a' w'. Graft (a' -> a) (w' -> w) (VDOM a' w')

deriving instance Functor (Graft a)

instance Bifunctor Graft where
  bimap f g (Graft fm wm v) = Graft (f . fm) (g . wm) v

runGraft :: Graft a w -> VDOM a w
runGraft (Graft fm fw v) = bimap fm fw v

renderComponent :: (a -> a') -> (w -> VDOM a' w') -> VDOM a w -> VDOM a' w'
renderComponent fm injComponent = \case
  Text txt -> Text txt
  Element ns'm en props children ->
    Element ns'm en (fm props) (map (renderComponent fm injComponent) children)
  Component w -> injComponent w
  Grafted graft -> renderComponent fm injComponent (runGraft graft)
