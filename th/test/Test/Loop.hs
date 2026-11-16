{-# LANGUAGE OverloadedRecordDot #-}

-- | @{% for %}@ over any 'Foldable', in every place a template loops:
-- among children, among attributes, in an attribute's text and among a
-- style's rules.
module Test.Loop (spec) where

import Clay (Css, (-:), (?))
import Clay qualified as C
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Halogen.HTML qualified as HH
import Halogen.HTML.Core (ClassName (..))
import Halogen.HTML.Properties qualified as HP
import Halogen.TH (clay, hh, hhs)
import Test.Hspec
import Test.Render (render)

type Html = HH.HTML () ()

newtype User = User {name :: Text}

same :: Html -> Html -> Expectation
same quoted written = render quoted `shouldBe` render written

sameAll :: [Html] -> [Html] -> Expectation
sameAll quoted written = map render quoted `shouldBe` map render written

sameCss :: Css -> Css -> Expectation
sameCss quoted written = C.renderWith C.compact [] quoted `shouldBe` (C.renderWith C.compact [] written :: TL.Text)

shown :: Int -> Text
shown = T.pack . show

spec :: Spec
spec = do
  describe "children" $ do
    it "a Maybe: once for a Just" $ do
      let selected = Just (User "Ann")
      [hhs|{% for user in selected %}<p>{{ user.name }}</p>{% end %}|] `sameAll` [HH.p_ [HH.text "Ann"]]
    it "a Maybe: not at all for a Nothing" $ do
      let selected = Nothing :: Maybe User
      [hh|<div>{% for user in selected %}<p>{{ user.name }}</p>{% end %}</div>|] `same` HH.div_ []
    it "a NonEmpty" $ do
      let xs = 1 :| [2, 3] :: NonEmpty Int
      [hh|<ol>{% for x in xs %}<li>{{ shown x }}</li>{% end %}</ol>|]
        `same` HH.ol_ [HH.li_ [HH.text (shown x)] | x <- [1, 2, 3]]
    it "a Map: its values, in the order of the keys" $ do
      let ages = Map.fromList [("bo", 7), ("al", 9)] :: Map Text Int
      [hhs|{% for age in ages %}<i>{{ shown age }}</i>{% end %}|] `sameAll` [HH.i_ [HH.text "9"], HH.i_ [HH.text "7"]]
    it "Map.toList of a Map: its pairs" $ do
      let ages = Map.fromList [("bo", 7), ("al", 9)] :: Map Text Int
      [hhs|{% for (who, age) in Map.toList ages %}<i>{{ who }} {{ shown age }}</i>{% end %}|]
        `sameAll` [HH.i_ [HH.text "al", HH.text " ", HH.text "9"], HH.i_ [HH.text "bo", HH.text " ", HH.text "7"]]
    it "a Seq, and a Set with a type signature" $ do
      let xs = Seq.fromList [3, 1] :: Seq Int
          ys = Set.fromList [3, 1, 3] :: Set Int
      [hhs|{% for x in xs %}<b>{{ shown x }}</b>{% end %}{% for y in (ys :: Set Int) %}<u>{{ shown y }}</u>{% end %}|]
        `sameAll` [HH.b_ [HH.text "3"], HH.b_ [HH.text "1"], HH.u_ [HH.text "1"], HH.u_ [HH.text "3"]]
    it "a pattern that can fail skips what it does not match, over a Maybe too" $ do
      let found = Just (Left "missing") :: Maybe (Either Text Int)
          other = Just (Right 4) :: Maybe (Either Text Int)
      [hhs|{% for Left why in found %}<p>{{ why }}</p>{% end %}{% for Left why in other %}<p>{{ why }}</p>{% end %}|]
        `sameAll` [HH.p_ [HH.text "missing"]]
    it "nested loops over different Foldables" $ do
      let groups = Map.fromList [(1, 'a' :| "b"), (2, 'c' :| "")] :: Map Int (NonEmpty Char)
          chosen = Just 2 :: Maybe Int
      [hh|<table>{% for g in groups %}<tr>{% for c in g %}<td>{{ T.singleton c }}</td>{% end %}</tr>{% end %}{% for n in chosen %}{% for (k, _) in Map.toList groups %}{% if k == n %}<tr><th>{{ shown k }}</th></tr>{% end %}{% end %}{% end %}</table>|]
        `same` HH.table_
          [ HH.tr_ [HH.td_ [HH.text "a"], HH.td_ [HH.text "b"]]
          , HH.tr_ [HH.td_ [HH.text "c"]]
          , HH.tr_ [HH.th_ [HH.text "2"]]
          ]
    it "a list, as before" $ do
      let names = ["a", "b"] :: [Text]
      [hhs|{% for x in names %}<b>{{ x }}</b>{% end %}|] `sameAll` [HH.b_ [HH.text "a"], HH.b_ [HH.text "b"]]

  describe "attributes" $ do
    it "a Maybe among the attributes" $ do
      let tip = Just "t" :: Maybe Text
      [hh|<p {% for t in tip %}title={{ t }} {% end %}></p>|] `same` HH.p [HP.title "t"] []
    it "a Maybe in an attribute's text" $ do
      let extra = Just "open" :: Maybe Text
          none = Nothing :: Maybe Text
      [hh|<div class="panel{% for e in extra %} {{ e }}{% end %}{% for e in none %} {{ e }}{% end %}"></div>|]
        `same` HH.div [HP.class_ (ClassName "panel open")] []

  describe "clay" $ do
    it "a Maybe among the rules, and Map.toList for pairs" $ do
      let accent = Just "red" :: Maybe Text
          sizes = Map.fromList [("s", 12), ("m", 16)] :: Map Text Int
      [clay|
        {% for a in accent %} .accent { color: {{ a }} } {% end %}
        {% for (n, px) in Map.toList sizes %}
          .text-{{ n }} { font-size: {{ C.px (fromIntegral px) }}; }
        {% end %}
      |]
        `sameCss` (do ".accent" ? ("color" -: "red"); ".text-m" ? C.fontSize (C.px 16); ".text-s" ? C.fontSize (C.px 12))
    it "a Maybe inside a rule" $ do
      let gap = Nothing :: Maybe Int
          pad = Just 3 :: Maybe Int
      [clay| .a { {% for g in gap %} gap: {{ C.px (fromIntegral g) }}; {% end %} {% for p in pad %} padding: {{ C.px (fromIntegral p) }}; {% end %} } |]
        `sameCss` (".a" ? ("padding" -: "3px"))
