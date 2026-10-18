module Test.Check (spec) where

import Halogen.TH.Check
import Language.Haskell.TH (Extension (..))
import Test.Hspec

spec :: Spec
spec = do
  it "finds nothing wrong with a quote that is fine" $ do
    check [] Hh (3, 10) "<p class=\"a\">{{ x }}</p>" `shouldReturn` Nothing
    check [] Clay (3, 10) ".a { color: {{ ink }}; }" `shouldReturn` Nothing
  it "places a parse error in a hole at its line and column of the file" $
    check [] Hh (10, 14) "\n  <div>\n    <span class=\"n\">{{ show n) }}</span>\n  </div>\n"
      `shouldReturn` Just (Complaint 12 (Just 30) "parse error on input `)'")
  it "places one on the quote's first line after the quote's column" $
    check [] Clay (6, 16) ".a { color: {{ red) }}; } "
      `shouldReturn` Just (Complaint 6 (Just 34) "parse error on input `)'")
  it "places one in the pattern of a for" $
    check [] Hhs (7, 16) "{% for (x in xs %}<b>{{ x }}</b>{% end %}"
      `shouldReturn` Just (Complaint 7 (Just 25) "parse error (possibly incorrect indentation or mismatched brackets)")
  it "places what the template gets wrong on its line, without a column" $
    check [] Hh (4, 12) "<div>\n  <p>{{ x</p>\n</div>"
      `shouldReturn` Just (Complaint 5 Nothing "{{ is never closed by }}")
  it "parses the Haskell with the extensions it is given" $ do
    -- Without Arrows, proc is a name and the arrow after it a parse error,
    -- at its column; with Arrows the parse goes through, to what a
    -- template cannot hold, which has no column.
    fmap (fmap column) (check [] Hhs (1, 1) "{{ proc x -> f -< x }}") `shouldReturn` Just (Just 11)
    fmap (fmap column) (check [Arrows] Hhs (1, 1) "{{ proc x -> f -< x }}") `shouldReturn` Just Nothing
