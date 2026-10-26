module Test.HTML (spec) where

import Data.Bifunctor (bimap)
import Data.Text (Text)
import Halogen.HTML qualified as HH
import Halogen.HTML.Core (AttrName (..), HTML (..), withProps)
import Halogen.HTML.Core qualified as Core
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Query.Input (Input (..))
import Halogen.VDom.DOM.Prop (Prop (..))
import Halogen.VDom.Types (ElemName (..), Graft (..), VDom (..))
import Prelude
import Test.Hspec (Spec, describe, it, shouldBe)
import Web.HTML.Common (PropName (..))

-- | What a test compares of a piece of HTML: each element's name and its
-- properties in order, a property by its kind and name (an attribute with
-- its value too, and a handler with the message it makes of no event), and
-- what is under it.
data Shape
  = ElementShape Text [Text] [Shape]
  | KeyedShape Text [Text] [(Text, Shape)]
  | TextShape Text
  | WidgetShape
  deriving stock (Eq, Show)

shape :: VDom [Prop (Input Int)] w -> Shape
shape = \case
  Elem _ (ElemName name) props children -> ElementShape name (map property props) (map shape children)
  Keyed _ (ElemName name) props children -> KeyedShape name (map property props) (map (fmap shape) children)
  Text t -> TextShape t
  Widget _ -> WidgetShape
  Grafted (Graft f g vdom) -> shape (bimap f g vdom)
  where
    property = \case
      Attribute _ (AttrName name) value -> "attr " <> name <> "=" <> value
      Property (PropName name) _ -> "prop " <> name
      Handler _ _ _ -> "handler"
      Ref _ -> "ref"

mark :: Prop i
mark = Core.attr Nothing (AttrName "data-mark") "1:1"

spec :: Spec
spec = describe "withProps" $ do
  it "puts the properties before an element's own, which it keeps, and leaves its children alone" $
    shape (withProps [mark] (HH.div [HP.id "x", HE.onClick (const (1 :: Int))] [HH.p_ [HH.text "a"]])).unHTML
      `shouldBe` ElementShape "div" ["attr data-mark=1:1", "prop id", "handler"] [ElementShape "p" [] [TextShape "a"]]
  it "does the same to an element with keyed children" $
    shape (withProps [mark] (HH.withKeys HH.ul [HP.id "x"] [("k", HH.li_ [HH.text "a"])] :: HTML () Int)).unHTML
      `shouldBe` KeyedShape "ul" ["attr data-mark=1:1", "prop id"] [("k", ElementShape "li" [] [TextShape "a"])]
  it "leaves a text node and a widget as they are" $ do
    shape (withProps [mark] (HH.text "a" :: HTML () Int)).unHTML `shouldBe` TextShape "a"
    shape (withProps [mark] (Core.widget () :: HTML () Int)).unHTML `shouldBe` WidgetShape
  it "reaches the element under a bimap, and under a graft" $ do
    let element = HH.button [HP.title "t"] [HH.text "b"] :: HTML () Int
    shape (withProps [mark] (bimap id (+ 1) element)).unHTML
      `shouldBe` ElementShape "button" ["attr data-mark=1:1", "prop title"] [TextShape "b"]
    shape (withProps [mark] (HTML (Grafted (Graft id id element.unHTML)))).unHTML
      `shouldBe` ElementShape "button" ["attr data-mark=1:1", "prop title"] [TextShape "b"]
