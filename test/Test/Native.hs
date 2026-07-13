{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Native (non-JavaScript) test specs — these run under a plain GHC and
-- in ordinary CI.
--
-- Currently this covers the four @Halogen.Svg.Attributes@ gaps fixed to
-- bring the module to parity with @purescript-halogen-svg-elems@:
--
--   1. Dimension/radius setters (@x@, @y@, @width@, @height@, @r@, @rx@,
--      @ry@, @strokeWidth@) were constrained @HasType \"…\" Text@ while
--      "Halogen.Svg.Indexed" declares those very row fields as @Double@,
--      so they could never resolve against their own elements. 'svgRoot'
--      is the regression guard: it builds a typed @svg/circle/rect@ tree
--      and only type-checks once the constraints say @Double@.
--   2. The @transform@ setter previously emitted the attribute name
--      @\"Transformation\"@ (and leaned on Clay's un-exported
--      @Transformation@). 'attrNameValue' pins both the correct attribute
--      name and the rendered value.
--   3. Path-command smart constructors @m@/@l@/@q@/@z@ (with
--      'CommandPositionReference') were missing entirely.
--   4. 'Color', 'FontSize' and 'Transformation' are re-exported from the
--      module — the import list below fails to compile if they regress.
module Test.Native (nativeSpec) where

import Data.Row (type (.==))
import Data.Text (Text)
import Halogen.HTML.Core (AttrName (..))
import Halogen.HTML.Core qualified as HC
import Halogen.HTML.Properties (IProp (..))
import Halogen.Svg.Attributes
  ( Color
  , CommandPositionReference (..)
  , FontSize
  , PathCommand (..)
  , Transform (..)
  , Transformation
  , printTransform
  )
import Halogen.Svg.Attributes qualified as SA
import Halogen.Svg.Elements qualified as SE
import Halogen.VDom.DOM.Prop (Prop (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude

-- | Pull the (name, value) out of an attribute-backed 'IProp' so the
-- rendered attribute name and text can be asserted directly.
attrNameValue :: IProp r i -> Maybe (Text, Text)
attrNameValue (IProp (Attribute _ (AttrName n) v)) = Just (n, v)
attrNameValue _ = Nothing

-- | Gap 1 guard. A typed @svg/circle/rect@ tree built only through the
-- indexed rows from "Halogen.Svg.Indexed". @width@/@height@ resolve
-- against @SVGsvg@ and @SVGrect@, @r@ against @SVGcircle@, @x@/@y@
-- against @SVGrect@ — all of which the indexed rows declare as @Double@.
-- The mere fact that this type-checks is the regression: before the fix
-- the setters demanded @Text@ and this tree would not build.
svgRoot :: forall w i. HC.HTML w i
svgRoot =
  SE.svg
    [ SA.viewBox 0.0 0.0 200.0 100.0
    , SA.width 200.0
    , SA.height 100.0
    ]
    [ SE.circle
        [ SA.cx 50.0
        , SA.cy 50.0
        , SA.r 20.0
        ]
    , SE.rect
        [ SA.x 10.0
        , SA.y 10.0
        , SA.width 30.0
        , SA.height 40.0
        ]
    ]

-- | Gap 4 guard: the three Clay-sourced types stay re-exported. These
-- signatures only need the names to be in scope through
-- 'Halogen.Svg.Attributes'.
_reExportedColor :: Maybe Color
_reExportedColor = Nothing

_reExportedFontSize :: Maybe FontSize
_reExportedFontSize = Nothing

_reExportedTransformation :: Maybe Transformation
_reExportedTransformation = Nothing

nativeSpec :: Spec
nativeSpec = do
  describe "Halogen.Svg.Attributes path commands (gap 3)" $ do
    it "m/l render absolute (uppercase) and relative (lowercase) forms" $ do
      SA.m Abs 1.0 2.0 `shouldBe` PathCommand "M 1.0 2.0"
      SA.m Rel 1.0 2.0 `shouldBe` PathCommand "m 1.0 2.0"
      SA.l Abs 5.0 6.0 `shouldBe` PathCommand "L 5.0 6.0"
      SA.l Rel 5.0 6.0 `shouldBe` PathCommand "l 5.0 6.0"

    it "q renders a quadratic Bézier with control + end points" $
      SA.q Abs 1.0 2.0 3.0 4.0 `shouldBe` PathCommand "Q 1.0 2.0 3.0 4.0"

    it "z is the close-path command (no relative form)" $
      SA.z `shouldBe` PathCommand "Z"

  describe "Halogen.Svg.Attributes transform (gap 2)" $ do
    it "printTransform renders all six SVG transform functions" $ do
      printTransform (Translate 1.0 2.0) `shouldBe` "translate(1.0 2.0)"
      printTransform (Scale 2.0 3.0) `shouldBe` "scale(2.0 3.0)"
      printTransform (Rotate 90.0 50.0 50.0) `shouldBe` "rotate(90.0 50.0 50.0)"
      printTransform (SkewX 10.0) `shouldBe` "skewX(10.0)"
      printTransform (SkewY 20.0) `shouldBe` "skewY(20.0)"
      printTransform (Matrix 1.0 0.0 0.0 1.0 5.0 6.0)
        `shouldBe` "matrix(1.0 0.0 0.0 1.0 5.0 6.0)"

    it "transform emits the 'transform' attribute name, space-joined" $ do
      let p :: IProp ("transform" .== Text) ()
          p = SA.transform [Translate 1.0 2.0, Rotate 90.0 0.0 0.0]
      attrNameValue p
        `shouldBe` Just ("transform", "translate(1.0 2.0) rotate(90.0 0.0 0.0)")

  describe "Halogen.Svg.Attributes dimension setters (gap 1)" $
    it "builds a typed svg/circle/rect tree (type-check is the assertion)" $ do
      let v :: HC.HTML () ()
          v = svgRoot
      v `seq` (True `shouldBe` True)
