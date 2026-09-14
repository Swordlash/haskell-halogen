module Test.SvgAttributes (spec) where

import Data.Text (Text)
import Halogen.HTML.Core (AttrName (..))
import Halogen.HTML.Properties (IProp (..))
import Halogen.Svg.Attributes qualified as SA
import Halogen.Svg.Indexed qualified as SI
import Halogen.VDom.DOM.Prop (Prop (..))
import Prelude
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual)

attribute :: IProp r i -> (Text, Text)
attribute (IProp (Attribute Nothing (AttrName name) value)) = (name, value)
attribute _ = error "Test.SvgAttributes: expected an unnamespaced attribute"

-- These annotations are compile-time regressions: each setter must agree with
-- the corresponding field type in Halogen.Svg.Indexed.
circleAttributes :: [IProp SI.SVGcircle ()]
circleAttributes = [SA.r 1, SA.strokeWidth 2, SA.strokeDashOffset 3, SA.strokeOpacity 0.5]

ellipseAttributes :: [IProp SI.SVGellipse ()]
ellipseAttributes = [SA.rx 4, SA.ry 5]

markerAttributes :: [IProp SI.SVGmarker ()]
markerAttributes = [SA.refX 6, SA.refY 7]

lineAttributes :: [IProp SI.SVGline ()]
lineAttributes = [SA.x1 1, SA.y1 2, SA.x2 3, SA.y2 4]

polylineAttributes :: [IProp SI.SVGpolyline ()]
polylineAttributes = [SA.pathLength 8]

svgAttributes :: [IProp SI.SVGsvg ()]
svgAttributes = [SA.width 9, SA.height 10, SA.fontSizeAdjust 0.75]

positionAttributes :: [IProp SI.SVGforeignObject ()]
positionAttributes = [SA.x 11, SA.y 12]

spec :: Spec
spec = describe "SVG attributes" $ do
  it "serializes every upstream path command" $
    assertEqual
      "complete upstream path command serialization"
      ( "d"
      , "M1.0, 2.0 l3.0, 4.0 H5.0 v6.0 C7.0,8.0 9.0,10.0 11.0,12.0 s13.0, 14.0, 15.0, 16.0 Q17.0, 18.0, 19.0, 20.0 t21.0, 22.0 A23.0, 24.0, 25.0 0 1 26.0 27.0 z"
      )
      ( attribute
          ( SA.d
              [ SA.m SA.Abs 1 2
              , SA.l SA.Rel 3 4
              , SA.h SA.Abs 5
              , SA.v SA.Rel 6
              , SA.c SA.Abs 7 8 9 10 11 12
              , SA.s SA.Rel 13 14 15 16
              , SA.q SA.Abs 17 18 19 20
              , SA.t SA.Rel 21 22
              , SA.a SA.Abs 23 24 25 SA.Arc0 SA.Sweep1 26 27
              , SA.z
              ]
              :: IProp SI.SVGpath ()
          )
      )
  it "converts path commands to rendered text" $
    assertEqual
      "path commands convert to their rendered text"
      ["H1.0", "v2.0"]
      (SA.toArrayString [SA.h SA.Abs 1, SA.v SA.Rel 2])
  it "renders the transform attribute" $
    assertEqual
      "transform attribute"
      ("transform", "matrix(1.0 0.0 0.0 1.0 2.0 3.0) translate(4.0 5.0) scale(6.0 7.0) rotate(8.0 9.0 10.0) skewX(11.0) skewY(12.0)")
      ( attribute
          ( SA.transform
              [ SA.Matrix 1 0 0 1 2 3
              , SA.Translate 4 5
              , SA.Scale 6 7
              , SA.Rotate 8 9 10
              , SA.SkewX 11
              , SA.SkewY 12
              ]
              :: IProp SI.SVGg ()
          )
      )
  it "renders the patternTransform attribute" $
    assertEqual
      "pattern transform attribute"
      ("patternTransform", "translate(1.0 2.0)")
      (attribute (SA.patternTransform [SA.Translate 1 2] :: IProp SI.SVGpattern ()))
  it "agrees with numeric fields in the indexed rows" $
    assertEqual
      "numeric indexed attributes"
      [ ("r", "1.0")
      , ("stroke-width", "2.0")
      , ("stroke-dashoffset", "3.0")
      , ("stroke-opacity", "0.5")
      , ("rx", "4.0")
      , ("ry", "5.0")
      , ("refX", "6.0")
      , ("refY", "7.0")
      , ("x1", "1.0")
      , ("y1", "2.0")
      , ("x2", "3.0")
      , ("y2", "4.0")
      , ("pathLength", "8.0")
      , ("width", "9.0")
      , ("height", "10.0")
      , ("font-size-adjust", "0.75")
      , ("x", "11.0")
      , ("y", "12.0")
      ]
      ( map attribute circleAttributes
          <> map attribute ellipseAttributes
          <> map attribute markerAttributes
          <> map attribute lineAttributes
          <> map attribute polylineAttributes
          <> map attribute svgAttributes
          <> map attribute positionAttributes
      )
