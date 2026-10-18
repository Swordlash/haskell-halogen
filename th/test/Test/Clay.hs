-- | The @clay@ quoter, each case against the same style written with Clay
-- by hand: both render to the same stylesheet.
module Test.Clay (spec) where

import Clay (Css, (&), (-:), (<?), (?))
import Clay qualified as C
import Clay.Stylesheet (Feature (..), MediaType (..), key)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Halogen.TH (clay)
import Test.Hspec

css :: Css -> TL.Text
css = C.renderWith C.compact []

same :: Css -> Css -> Expectation
same quoted written = css quoted `shouldBe` css written

spec :: Spec
spec = do
  describe "rules" $ do
    it "a selector with declarations" $
      [clay| .menu { position: relative; min-height: 100%; } |]
        `same` (".menu" ? do "position" -: "relative"; "min-height" -: "100%")
    it "selectors as written: descendants, children, lists" $
      [clay| .a .b > c, #d::before { color: red } |]
        `same` ((".a .b > c" <> "#d::before") ? ("color" -: "red"))
    it "nested rules, refinements and children" $
      [clay|
        .card {
          color: red;
          .title { font-weight: bold; }
          &:hover { color: blue; }
          &.open, &[aria-expanded="true"] { display: block; }
          > .body { padding: 4px; }
          & .deep { margin: 0; }
        }
      |]
        `same` ( ".card" ? do
                   "color" -: "red"
                   ".title" ? ("font-weight" -: "bold")
                   ":hover" & ("color" -: "blue")
                   ".open" & ("display" -: "block")
                   C.attr "aria-expanded=\"true\"" & ("display" -: "block")
                   ".body" <? ("padding" -: "4px")
                   ".deep" ? ("margin" -: "0")
               )
    it "comments, strings, functions and custom properties" $
      [clay| .a { /* not this */ --gap: 4px; content: "a; b"; background: url(x.png) no-repeat; width: calc(100% - 2px) !important; } |]
        `same` (".a" ? do "--gap" -: "4px"; "content" -: "\"a; b\""; "background" -: "url(x.png) no-repeat"; "width" -: "calc(100% - 2px) !important")
    it "declarations at the top: a mixin" $
      [clay| border: 1px solid; border-radius: 4px; |] `same` (do "border" -: "1px solid"; "border-radius" -: "4px")

  describe "at-rules" $ do
    it "@media with a type and features" $
      [clay| @media screen and (max-width: 600px) and (hover) { .a { color: red } } |]
        `same` C.query (MediaType "screen") [Feature "max-width" (Just "600px"), Feature "hover" Nothing] (".a" ? ("color" -: "red"))
    it "@media with features only, not and only" $ do
      [clay| @media (max-width: 600px) { color: red } |]
        `same` C.query (MediaType "all") [Feature "max-width" (Just "600px")] ("color" -: "red")
      [clay| @media not print { color: red } |] `same` C.queryNot (MediaType "print") [] ("color" -: "red")
      [clay| @media only screen { color: red } |] `same` C.queryOnly (MediaType "screen") [] ("color" -: "red")
    it "@media nested in a rule" $
      [clay| .a { color: red; @media (max-width: 600px) { color: blue } } |]
        `same` (".a" ? do "color" -: "red"; C.query (MediaType "all") [Feature "max-width" (Just "600px")] ("color" -: "blue"))
    it "@keyframes" $
      [clay| @keyframes spin { from { opacity: 0 } 50% { opacity: 0.5 } to { opacity: 1 } } |]
        `same` C.keyframes "spin" [(0, "opacity" -: "0"), (50, "opacity" -: "0.5"), (100, "opacity" -: "1")]
    it "@keyframes with statements and Haskell among the stops" $ do
      let mid = True
          late = [(90, "opacity" -: "0.9")] :: [(C.Number, Css)]
      [clay| @keyframes fade { from { opacity: 0 } {% if mid %} 50% { opacity: 0.5 } {% end %} {{ late }} {% for o in ["0.99"] %} 99% { opacity: {{ o :: Text }} } {% end %} to { opacity: 1 } } |]
        `same` C.keyframes "fade" [(0, "opacity" -: "0"), (50, "opacity" -: "0.5"), (90, "opacity" -: "0.9"), (99, "opacity" -: "0.99"), (100, "opacity" -: "1")]
    it "@media with a function in a feature's value" $
      [clay| @media (min-width: calc(100% - 1px)) and (hover) { color: red } |]
        `same` C.query (MediaType "all") [Feature "min-width" (Just "calc(100% - 1px)"), Feature "hover" Nothing] ("color" -: "red")
    it "@font-face and @import" $
      [clay| @import "fonts.css"; @font-face { font-family: X; src: url(x.woff2); } |]
        `same` (do C.importUrl "fonts.css"; C.fontFace (do "font-family" -: "X"; "src" -: "url(x.woff2)"))

  describe "Haskell" $ do
    it "a value: anything Clay can render" $ do
      let gold = C.rgb 200 160 40
          pad = C.px 12
      [clay| .a { color: {{ gold }}; padding: {{ pad }}; z-index: {{ 3 :: Integer }}; } |]
        `same` (".a" ? do C.color gold; key "padding" pad; C.zIndex 3)
    it "a part of a value" $ do
      let line = C.rgb 1 2 3
          w = "2px" :: Text
      [clay| .a { border: {{ w }} solid {{ line }}; } |] `same` (".a" ? ("border" -: ("2px solid " <> C.plain (C.unValue (C.value line)))))
    it "a selector" $ do
      let sel = ".dynamic" :: C.Selector
      [clay| {{ sel }} { color: red } |] `same` (sel ? ("color" -: "red"))
    it "a rule: a mixin among rules" $ do
      let boxed = do "border" -: "1px solid"; "border-radius" -: "4px"
      [clay| .a { {{ boxed }} color: red; } |] `same` (".a" ? do boxed; "color" -: "red")
    it "statements" $ do
      let compact = True
          sizes = [("s", 12), ("m", 16)] :: [(Text, Int)]
      [clay|
        {% for (n, px) in sizes %}
          .text-{{ n }} { font-size: {{ C.px (fromIntegral px) }}; }
        {% end %}
        {% if compact %} .a { gap: 2px } {% else %} .a { gap: 8px } {% end %}
      |]
        `same` (do ".text-s" ? C.fontSize (C.px 12); ".text-m" ? C.fontSize (C.px 16); ".a" ? ("gap" -: "2px"))
