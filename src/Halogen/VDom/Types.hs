module Halogen.VDom.Types
  ( VDom (..)
  , renderWidget
  , Graft (..)
  , runGraft
  , ElemName (..)
  , Namespace (..)
  , unNamespace
  )
where

import HPrelude

newtype ElemName = ElemName Text
  deriving (Eq, Ord, Show, IsString)

newtype Namespace = Namespace Text
  deriving (Eq, Ord, Show, IsString)

unNamespace :: Namespace -> Text
unNamespace (Namespace ns) = ns

data VDom a w
  = Text !Text
  | Elem !(Maybe Namespace) !ElemName a [VDom a w]
  | Widget w
  | Grafted (Graft a w)
  deriving (Functor)

instance Bifunctor VDom where
  bimap f g = \case
    Text s -> Text s
    Elem ns'm en props children -> Elem ns'm en (f props) (map (bimap f g) children)
    Widget w -> Widget $ g w
    Grafted graft -> Grafted $ bimap f g graft

data Graft a w = forall a' w'. Graft (a' -> a) (w' -> w) (VDom a' w')

deriving instance Functor (Graft a)

instance Bifunctor Graft where
  bimap f g (Graft fm wm v) = Graft (f . fm) (g . wm) v

runGraft :: Graft a w -> VDom a w
runGraft (Graft fm fw v) = bimap fm fw v

renderWidget :: (a -> a') -> (w -> VDom a' w') -> VDom a w -> VDom a' w'
renderWidget fm injWidget = \case
  Text txt -> Text txt
  Elem ns'm en props children ->
    Elem ns'm en (fm props) (map (renderWidget fm injWidget) children)
  Widget w -> injWidget w
  Grafted graft -> renderWidget fm injWidget (runGraft graft)
