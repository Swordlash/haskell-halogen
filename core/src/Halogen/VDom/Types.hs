module Halogen.VDom.Types
  ( VDom (..)
  , renderWidget
  , Graft (..)
  , runGraft
  , ElemName (..)
  , Namespace (..)
  , unNamespace
  , elementInstanceOf
  )
where

import HPrelude

newtype ElemName = ElemName Text
  deriving (Eq, Ord, Show, IsString)

newtype Namespace = Namespace Text
  deriving (Eq, Ord, Show, IsString)

unNamespace :: Namespace -> Text
unNamespace (Namespace ns) = ns

-- | Whether an element of a namespace and a name is an instance of a DOM
-- class, by the class's JavaScript name, as a browser answers
-- @instanceof@: every element is an @EventTarget@, a @Node@ and an
-- @Element@, an HTML one an @HTMLElement@ and the class of its tag
-- (@HTMLDialogElement@ for a @dialog@), an SVG one an @SVGElement@. What a
-- backend that is not a browser answers for its elements' checked casts
-- ('Halogen.JSBits.Value.NativeObject'), so that
-- 'Halogen.Query.getHTMLElementRef' and @HTMLDialogElement.fromElement@
-- find the element there too. A tag missing from the table here is only an
-- @HTMLElement@.
elementInstanceOf :: Maybe Namespace -> ElemName -> Text -> Bool
elementInstanceOf namespace (ElemName tag) klass
  | klass `elem` ["EventTarget", "Node", "Element"] = True
  | otherwise = case namespace of
      Nothing -> html
      Just (Namespace "http://www.w3.org/1999/xhtml") -> html
      Just (Namespace "http://www.w3.org/2000/svg") -> klass == "SVGElement" || (tag == "svg" && klass == "SVGSVGElement")
      Just _ -> False
  where
    html = klass == "HTMLElement" || (tag, klass) `elem` htmlClasses
    htmlClasses =
      [ ("a", "HTMLAnchorElement")
      , ("audio", "HTMLAudioElement")
      , ("body", "HTMLBodyElement")
      , ("button", "HTMLButtonElement")
      , ("canvas", "HTMLCanvasElement")
      , ("details", "HTMLDetailsElement")
      , ("dialog", "HTMLDialogElement")
      , ("div", "HTMLDivElement")
      , ("form", "HTMLFormElement")
      , ("iframe", "HTMLIFrameElement")
      , ("img", "HTMLImageElement")
      , ("input", "HTMLInputElement")
      , ("label", "HTMLLabelElement")
      , ("li", "HTMLLIElement")
      , ("ol", "HTMLOListElement")
      , ("option", "HTMLOptionElement")
      , ("p", "HTMLParagraphElement")
      , ("select", "HTMLSelectElement")
      , ("span", "HTMLSpanElement")
      , ("table", "HTMLTableElement")
      , ("textarea", "HTMLTextAreaElement")
      , ("ul", "HTMLUListElement")
      , ("video", "HTMLVideoElement")
      ]

data VDom a w
  = Text Text
  | Elem (Maybe Namespace) ElemName a [VDom a w]
  | Keyed (Maybe Namespace) ElemName a [(Text, VDom a w)]
  | Widget w
  | Grafted (Graft a w)
  deriving (Functor)

instance Bifunctor VDom where
  bimap f g = \case
    Text s -> Text s
    Elem ns'm en props children -> Elem ns'm en (f props) (map (bimap f g) children)
    Keyed ns'm en props children -> Keyed ns'm en (f props) (map (second (bimap f g)) children)
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
  Keyed ns'm en props children ->
    Keyed ns'm en (fm props) (map (second (renderWidget fm injWidget)) children)
  Widget w -> injWidget w
  Grafted graft -> renderWidget fm injWidget (runGraft graft)
