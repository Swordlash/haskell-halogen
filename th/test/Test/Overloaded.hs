{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Loops whose source is written as a list, in a module with
-- @OverloadedLists@ and @OverloadedStrings@: there a list literal, a range
-- and a string literal are of any type of a class, and only the loop says
-- it is a list. A loop draws from such a source as it is (not through
-- 'Data.Foldable.toList', which would leave the type open), so these build
-- as they did before loops took any 'Foldable'.
module Test.Overloaded (spec) where

import Clay (Css, (-:), (?))
import Clay qualified as C
import Data.Set (Set)
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

same :: Html -> Html -> Expectation
same quoted written = render quoted `shouldBe` render written

sameAll :: [Html] -> [Html] -> Expectation
sameAll quoted written = map render quoted `shouldBe` map render written

shown :: Int -> Text
shown = T.pack . show

spec :: Spec
spec = do
  it "a list literal" $
    [hhs|{% for x in [1, 2, 3] %}<b>{{ shown x }}</b>{% end %}|]
      `sameAll` [HH.b_ [HH.text "1"], HH.b_ [HH.text "2"], HH.b_ [HH.text "3"]]
  it "a range" $ do
    let n = 3 :: Int
    [hhs|{% for x in [1 .. n] %}<b>{{ shown x }}</b>{% end %}|]
      `sameAll` [HH.b_ [HH.text "1"], HH.b_ [HH.text "2"], HH.b_ [HH.text "3"]]
  it "a comprehension" $
    [hhs|{% for x in [y * 2 | y <- [1, 2]] %}<b>{{ shown x }}</b>{% end %}|]
      `sameAll` [HH.b_ [HH.text "2"], HH.b_ [HH.text "4"]]
  it "a list literal in parentheses" $
    [hhs|{% for x in ([4, 5]) %}<b>{{ shown x }}</b>{% end %}|]
      `sameAll` [HH.b_ [HH.text "4"], HH.b_ [HH.text "5"]]
  it "a string literal" $
    [hhs|{% for c in "ab" %}<i>{{ T.singleton c }}</i>{% end %}|]
      `sameAll` [HH.i_ [HH.text "a"], HH.i_ [HH.text "b"]]
  it "a list literal of string literals in an attribute's text" $
    [hh|<p class="{% for c in ["a", "b"] %}{{ c }}{% end %}"></p>|]
      `same` HH.p [HP.class_ (ClassName "ab")] []
  it "an if whose branches are list literals" $ do
    let open = True
        a = 7 :: Int
    [hhs|{% for x in if open then [a] else [] %}<b>{{ shown x }}</b>{% end %}|] `sameAll` [HH.b_ [HH.text "7"]]
  it "a list literal with a signature is of the signature's type" $
    [hhs|{% for x in ([3, 1, 3] :: Set Int) %}<b>{{ shown x }}</b>{% end %}|]
      `sameAll` [HH.b_ [HH.text "1"], HH.b_ [HH.text "3"]]
  it "a list literal among a style's rules" $ do
    let sheet = [clay| {% for o in ["0.5", "1"] %} .a { opacity: {{ o :: Text }} } {% end %} |] :: Css
    C.renderWith C.compact [] sheet
      `shouldBe` (C.renderWith C.compact [] (do ".a" ? ("opacity" -: "0.5"); ".a" ? ("opacity" -: "1")) :: TL.Text)
