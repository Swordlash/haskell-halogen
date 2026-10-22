{-# LANGUAGE OverloadedRecordDot #-}

-- | The @hh@ and @hhs@ quoters, each case against the same markup written
-- with "Halogen.HTML" by hand.
module Test.Html (spec) where

import Clay qualified as C
import DOM.HTML.Indexed qualified as I
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Halogen.HTML qualified as HH
import Halogen.HTML.Core (AttrName (..), ClassName (..), ElemName (..))
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Query.Input (RefLabel (..))
import Halogen.Svg.Attributes qualified as SA
import Halogen.Svg.Elements qualified as SE
import Halogen.Svg.Indexed qualified as SI
import Halogen.TH (hh, hhs, svg, svgs)
import Test.Hspec
import Test.Render (render)

data Action = Inc | Dec | Typed Text
  deriving (Eq, Show)

type Html = HH.HTML () Action

data Person = Person {name :: Text, age :: Int}

same :: Html -> Html -> Expectation
same quoted written = render quoted `shouldBe` render written

sameAll :: [Html] -> [Html] -> Expectation
sameAll quoted written = map render quoted `shouldBe` map render written

spec :: Spec
spec = do
  describe "SVG" $ do
    it "an <svg> in hh is SVG: its elements in SVG's namespace, numbers and paths typed, the class an attribute" $ do
      let w = 30 :: Double
          cmds = [SA.m SA.Abs 0 0, SA.l SA.Abs 10 10]
      [hh|<div><svg viewBox={{ (0, 0, w, 20) }} class="map"><path d={{ cmds }} stroke-width="2" class="road"/><circle cx={{ w / 2 }} cy="5" r="3"/></svg></div>|]
        `same` HH.div_ [SE.svg [SA.viewBox 0 0 w 20, SA.class_ (ClassName "map")] [SE.path [SA.d cmds, SA.strokeWidth 2, SA.class_ (ClassName "road")], SE.circle [SA.cx (w / 2), SA.cy 5, SA.r 3]]]
    it "keeps the case of SVG names, takes an attribute by its function's name, and an unknown one as written" $
      [svg|<g><foo-bar stroke-linecap="round" strokeLineJoin="bevel" data-x="1"></foo-bar></g>|]
        `same` SE.g [] [SE.element (ElemName "foo-bar") [HP.attr (AttrName "stroke-linecap") "round", HP.attr (AttrName "stroke-linejoin") "bevel", HP.attr (AttrName "data-x") "1"] []]
    it "an element without a function: its attributes as text, the style and handlers typed" $ do
      let colour = "red" :: Text
      [svg|<linearGradient id="sky" x1="0" y2="1"><stop offset="0" style={{ "stop-color:" <> colour }} onClick={{ const Inc }}/></linearGradient>|]
        `same` SE.element
          (ElemName "linearGradient")
          [HP.attr (AttrName "id") "sky", HP.attr (AttrName "x1") "0", HP.attr (AttrName "y2") "1"]
          [SE.element (ElemName "stop") ([HP.attr (AttrName "offset") "0", HP.styleText "stop-color:red", HE.onClick (const Inc)] :: [HP.IProp (SI.GlobalAttributes Empty) Action]) []]
    it "an <SVG> in HTML is the svg element; fill on an animation is the state it leaves; a function's name stands for its attribute" $
      [hh|<div><SVG viewBox="0 0 1 1"><animate attributeName="x" fill={{ SA.Freeze }}/><text fontStretch="condensed" fontVariant="small-caps">t</text></SVG></div>|]
        `same` HH.div_
          [ SE.svg
              [HP.attr (AttrName "viewBox") "0 0 1 1"]
              [ SE.animate [HP.attr (AttrName "attributeName") "x", SA.fillAnim SA.Freeze]
              , SE.text [HP.attr (AttrName "font-stretch") "condensed", SA.fontVariant "small-caps"] [HH.text "t"]
              ]
          ]
    it "a circle holding an animation, fill on every animation, xlink:href on use and image, a mask's content units" $
      [svg|<g><circle r="1"><animateTransform attributeName="transform" fill={{ SA.Freeze }}/><set attributeName="r" to="2" fill={{ SA.Remove }}/></circle><use xlink:href={{ "#a" :: Text }}/><image xlink:href={{ "b.png" :: Text }}/><mask maskContentUnits={{ SA.ObjectBoundingBox }}></mask></g>|]
        `same` SE.g
          []
          [ SE.circleNode
              [SA.r 1]
              [ SE.animateTransform [HP.attr (AttrName "attributeName") "transform", SA.fillAnim SA.Freeze]
              , SE.set [HP.attr (AttrName "attributeName") "r", HP.attr (AttrName "to") "2", SA.fillAnim SA.Remove]
              ]
          , SE.use [SA.xlinkHref "#a"]
          , SE.image [SA.xlinkHref "b.png"]
          , SE.mask [SA.maskContentUnits SA.ObjectBoundingBox] []
          ]
    it "onLoad on the svg, pathLength on shapes, xlinkHref by the function's name" $
      [svg|<svg onLoad={{ const Inc }}><path pathLength={{ 10 }} d="M0 0"/><circle pathLength="5" r="1"/><use xlinkHref={{ "#a" :: Text }}/></svg>|]
        `same` SE.svg
          [HE.onLoad (const Inc)]
          [ SE.path [SA.pathLength 10, HP.attr (AttrName "d") "M0 0"]
          , SE.circle [SA.pathLength 5, SA.r 1]
          , SE.use [SA.xlinkHref "#a"]
          ]
    it "a marker's viewBox, mpath's id, transform on a polygon, href on an animation, lengths with units" $ do
      let ts = [SA.Translate 1 2]
      [svg|<g><marker viewBox={{ (0, 0, 10, 10) }}></marker><animateMotion dur="1s"><mpath id="m" xlink:href="#p"/></animateMotion><polygon points="0,0 1,0 1,1" transform={{ ts }}/><animate href="#c" attributeName="r"/><rect width="100%" height="10em" x="1"/></g>|]
        `same` SE.g
          []
          [ SE.marker [SA.viewBox 0 0 10 10] []
          , SE.animateMotion [HP.attr (AttrName "dur") "1s"] [SE.mpath [SA.id "m", SA.xlinkHref "#p"]]
          , SE.polygon [HP.attr (AttrName "points") "0,0 1,0 1,1", SA.transform ts]
          , SE.animate [SA.href "#c", HP.attr (AttrName "attributeName") "r"]
          , SE.rect [HP.attr (AttrName "width") "100%", HP.attr (AttrName "height") "10em", SA.x 1]
          ]
    it "a rect holding an animation, fill on a polygon" $ do
      let colour = C.red
      [svg|<g><rect width="2" height="2"><animate attributeName="x" to="3"/></rect><polygon points="0,0 1,1" fill={{ colour }}/></g>|]
        `same` SE.g
          []
          [ SE.element (ElemName "rect") ([SA.width 2, SA.height 2] :: [HP.IProp SI.SVGrect Action]) [SE.animate [HP.attr (AttrName "attributeName") "x", HP.attr (AttrName "to") "3"]]
          , SE.polygon [HP.attr (AttrName "points") "0,0 1,1", SA.fill colour]
          ]
    it "a viewBox written as text, and a foreignObject holding HTML" $
      [hh|<svg viewBox="0 0 10 10"><foreignObject><div class="x">hi<br></div></foreignObject></svg>|]
        `same` SE.svg [HP.attr (AttrName "viewBox") "0 0 10 10"] [SE.foreignObject [] [HH.div [HP.class_ (ClassName "x")] [HH.text "hi", HH.br_]]]
    it "svgs: several nodes, with statements and handlers" $
      sameAll
        [svgs|{% for x in [1, 2 :: Double] %}<rect x={{ x }} y="0" width="1" height="1" onClick={{ const Inc }}/>{% end %}<text x="0" y="0">t</text>|]
        ([SE.rect [SA.x x, SA.y 0, SA.width 1, SA.height 1, HE.onClick (const Inc)] | x <- [1, 2]] <> [SE.text [SA.x 0, SA.y 0] [HH.text "t"]])
  describe "elements" $ do
    it "an element with a class and text" $
      [hh|<div class="panel">Hi</div>|] `same` HH.div [HP.class_ (ClassName "panel")] [HH.text "Hi"]
    it "a Css among the children is a style element" $ do
      let sheet = ".a" C.? ("color" C.-: "red")
      [hh|<div>{{ sheet }}<p>x</p></div>|] `same` HH.div [] [HH.element (ElemName "style") [] [HH.text (TL.toStrict (C.renderWith C.compact [] sheet))], HH.p [] [HH.text "x"]]
    it "nested elements, indented: indentation is not text" $
      [hh|
        <ul class="list">
          <li>one</li>
          <li>two</li>
        </ul>
      |]
        `same` HH.ul [HP.class_ (ClassName "list")] [HH.li_ [HH.text "one"], HH.li_ [HH.text "two"]]
    it "a space on one line between elements stays" $
      [hh|<p><b>a</b> <i>b</i></p>|] `same` HH.p_ [HH.b_ [HH.text "a"], HH.text " ", HH.i_ [HH.text "b"]]
    it "a text over several lines is one line" $
      [hh|<p>
            one
            two
          </p>|]
        `same` HH.p_ [HH.text "one two"]
    it "a text over several lines keeps the spaces of its first and last line where they meet a tag" $
      [hh|<p><b>a</b> and
            more <i>b</i></p>|]
        `same` HH.p_ [HH.b_ [HH.text "a"], HH.text " and more ", HH.i_ [HH.text "b"]]
    it "a text over several lines keeps a non-breaking space at a line's edge" $
      [hh|<p>
            &nbsp;a&nbsp;
          </p>|]
        `same` HH.p_ [HH.text "\xa0\&a\xa0"]
    it "a hole before /> in text or in a quoted value keeps the /> as text" $ do
      let label = "a" :: Text
      [hh|<p title="{{ label }}/>">{{ label }}/></p>|] `same` HH.p [HP.title "a/>"] [HH.text "a", HH.text "/>"]
    it "void elements, with or without a slash, and with an end tag" $
      [hh|<p>a<br>b<br/>c<br></br>d<input type="text"></p>|]
        `same` HH.p_ [HH.text "a", HH.br_, HH.text "b", HH.br_, HH.text "c", HH.br_, HH.text "d", HH.input [HP.attr (AttrName "type") "text"]]
    it "an element Halogen has no function for" $
      [hh|<my-widget size="3">x</my-widget>|] `same` HH.element (ElemName "my-widget") [HP.attr (AttrName "size") "3"] [HH.text "x"]
    it "entities and comments" $
      [hh|<p>a &amp; b<!-- not this -->{# nor this #}</p>|] `same` HH.p_ [HH.text "a & b"]
    it "hhs: several roots" $
      [hhs|<b>a</b><i>b</i>|] `sameAll` [HH.b_ [HH.text "a"], HH.i_ [HH.text "b"]]

  describe "attributes" $ do
    it "text properties, with Haskell in them" $ do
      let ident = "main" :: Text
          extra = "open" :: Text
      [hh|<section id={{ ident }} class="panel {{ extra }}" title="t"></section>|]
        `same` HH.section [HP.id ident, HP.class_ (ClassName ("panel " <> extra)), HP.title "t"] []
    it "boolean attributes: alone, or with a Bool" $ do
      let off = False
      [hh|<form novalidate><input disabled><input disabled={{ off }}><option selected="selected"></option></form>|]
        `same` HH.form [HP.noValidate True] [HH.input [HP.disabled True], HH.input [HP.disabled off], HH.option [HP.selected True] []]
    it "numbers: as written, or in Haskell" $ do
      let n = 3 :: Int
      [hh|<table><tr><td colspan="2" rowspan={{ n }}><input min="0" max="1.5"></td></tr></table>|]
        `same` HH.table_ [HH.tr_ [HH.td [HP.colSpan 2, HP.rowSpan n] [HH.input [HP.min 0, HP.max 1.5]]]]
    it "properties of their own type: Haskell is the property, text the attribute" $ do
      let txt = "typed" :: Text
      [hh|<input type={{ I.InputText }} value={{ txt }} step="any">|]
        `same` HH.input [HP.type_ I.InputText, HP.value txt, HP.attr (AttrName "step") "any"]
    it "attributes Halogen has no function for" $
      [hh|<div data-x="1" aria-label="l" role="button"></div>|]
        `same` HH.div [HP.attr (AttrName "data-x") "1", HP.attr (AttrName "aria-label") "l", HP.attr (AttrName "role") "button"] []
    it "event handlers, in any case" $
      [hh|<button onClick={{ const Inc }} onvalueinput={{ Typed }}>+</button>|]
        `same` HH.button [HE.onClick (const Inc), HE.onValueInput Typed] [HH.text "+"]
    it "style: text, Clay, or a Text" $ do
      let inline = "color: blue" :: Text
      [hh|<div style="color: red"><span style={{ C.color C.red }}></span><b style={{ inline }}></b></div>|]
        `same` HH.div [HP.styleText "color: red"] [HH.span [HP.style (C.color C.red)] [], HH.b [HP.styleText inline] []]
    it "ref takes a RefLabel" $
      [hh|<input ref={{ RefLabel "draft" }}>|] `same` HH.input [HP.ref (RefLabel "draft")]
    it "a property or a list of them among the attributes" $
      [hh|<div {{ HP.id "a" }} {{ [HP.title "t", HP.hidden True] }}></div>|]
        `same` HH.div [HP.id "a", HP.title "t", HP.hidden True] []
    it "an if among the attributes" $ do
      let open = True
      [hh|<details {% if open %}open{% else %}class="shut"{% end %}></details>|]
        `same` HH.details [HP.attr (AttrName "open") ""] []

  describe "children in Haskell" $ do
    it "a text, a node, a list, a maybe" $ do
      let who = "you" :: Text
          strong = HH.strong_ [HH.text "!"] :: Html
          items = [HH.li_ [HH.text "x"], HH.li_ [HH.text "y"]] :: [Html]
          none = Nothing :: Maybe Html
      [hh|<p>Hi {{ who }}{{ strong }}<ul>{{ items }}</ul>{{ none }}</p>|]
        `same` HH.p_ [HH.text "Hi ", HH.text who, strong, HH.ul_ items]
    it "holes on lines of their own have nothing between them" $ do
      let a = "a" :: Text
          b = HH.b_ [HH.text "b"] :: Html
      [hh|<p>
            {{ a }}
            {{ b }}

            {{ a }}
          </p>|]
        `same` HH.p_ [HH.text a, b, HH.text a]
    it "a space between holes on one line stays" $ do
      let a = "a" :: Text
      [hh|<p>
            {{ a }} {{ a }}
          </p>|]
        `same` HH.p_ [HH.text a, HH.text " ", HH.text a]
    it "a text and a hole on separate lines have no space between them, unless it is a hole" $ do
      let who = "you" :: Text
      [hh|<p>
            Hello,
            {{ who }}
            and
            {{ " " :: Text }}{{ who }}
          </p>|]
        `same` HH.p_ [HH.text "Hello,", HH.text who, HH.text "and", HH.text " ", HH.text who]
    it "a text on one line keeps its spaces next to a hole" $ do
      let who = "you" :: Text
      [hh|<p>
            Hello, {{ who }} and
            more
          </p>|]
        `same` HH.p_ [HH.text "Hello, ", HH.text who, HH.text " and more"]
    it "the expression is any Haskell: fields, lambdas, case" $ do
      let p = Person {name = "Ann", age = 41}
      [hh|<p>{{ p.name }} is {{ T.pack (show p.age) }}, {{ (\case { Person {age = a} | a > 40 -> "over"; _ -> "under" :: Text }) p }} forty</p>|]
        `same` HH.p_ [HH.text "Ann", HH.text " is ", HH.text "41", HH.text ", ", HH.text "over", HH.text " forty"]

  describe "statements" $ do
    it "if, elif, else" $ do
      let n = 5 :: Int
      [hhs|{% if n > 9 %}<b>many</b>{% elif n > 3 %}<i>some</i>{% else %}<u>few</u>{% end %}|]
        `sameAll` [HH.i_ [HH.text "some"]]
    it "if without else" $
      [hhs|<a></a>{% if False %}<b></b>{% end %}<c></c>|] `sameAll` [HH.a_ [], HH.element (ElemName "c") [] []]
    it "for over a pattern" $ do
      let names = ["a", "b"] :: [Text]
      [hh|<ol>{% for (i, x) in zip [1 :: Int ..] names %}<li>{{ T.pack (show i) }}. {{ x }}</li>{% end %}</ol>|]
        `same` HH.ol_ [HH.li_ [HH.text "1", HH.text ". ", HH.text "a"], HH.li_ [HH.text "2", HH.text ". ", HH.text "b"]]
    it "let, over the rest" $ do
      let n = 2 :: Int
      [hh|<p>{% let m = n * 2 %}{{ T.pack (show m) }} and {{ T.pack (show (m + 1)) }}</p>|]
        `same` HH.p_ [HH.text "4", HH.text " and ", HH.text "5"]
    it "statements on lines of their own make no text" $ do
      let names = ["a", "b"] :: [Text]
      [hh|<ul>
            {% for x in names %}
              {% let shown = x <> "!" %}
              {% if x /= "" %}
                <li>{{ shown }}</li>
              {% end %}
            {% end %}
          </ul>|]
        `same` HH.ul_ [HH.li_ [HH.text (x <> "!")] | x <- names]
    it "a text inside a statement on lines of its own is trimmed" $ do
      let n = 2 :: Int
      [hh|<p>
            {% for _ in [1 .. n] %}
              again
            {% end %}
          </p>|]
        `same` HH.p_ [HH.text "again", HH.text "again"]
    it "nested statements" $ do
      let rows = [[1, 2], [3]] :: [[Int]]
      [hh|<table>{% for r in rows %}<tr>{% for c in r %}{% if even c %}<td>{{ T.pack (show c) }}</td>{% end %}{% end %}</tr>{% end %}</table>|]
        `same` HH.table_ [HH.tr_ [HH.td_ [HH.text "2"]], HH.tr_ []]
