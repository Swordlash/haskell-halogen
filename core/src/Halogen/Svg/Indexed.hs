-- | Note: an element may contain more attributes than what
-- | we currently allow in its corresponding `SVGelemName`.
module Halogen.Svg.Indexed
  ( CoreAttributes
  , GlobalEventAttributes
  , GlobalAttributes
  , StrokeAttributes
  , StokeEndAttributes
  , StrokeJoinAttributes
  , FillAttributes
  , MarkerAttributes
  , FontAttributes
  , CanBeMaskedAttributes
  , AllPresentationAttributes
  , SVGsvg
  , SVGg
  , SVGforeignObject
  , SVGmarker
  , SVGcircle
  , SVGellipse
  , SVGline
  , SVGpolyline
  , SVGpolygon
  , SVGpath
  , SVGpattern
  , SVGrect
  , SVGtext
  , SVGmask
  , AnimationAttributes
  , SVGanimate
  , SVGanimateMotion
  , SVGimage
  , SVGmpath
  , SVGtitle
  , SVGuse
  )
where

import Data.Row
import Protolude
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)

{-
The table below show which groups of attributes apply to which elements. This
table is compiled by looking up each attribute on MDN
(e.g..+ https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke)
and looking at the list labeled "You can use this attribute with the following
SVG elements:". Groups are formed from attributes that have the same element
applicability.

element            stroke | strokeEnd | strokeJoin | fill  | font  | marker
circle                X   |     -     |     -      |   X   |   -   |    X
ellipse               X   |     -     |     -      |   X   |   -   |    X
line                  X   |     X     |     -      |   -   |   -   |    X
path                  X   |     X     |     X      |   X   |   -   |    X
rect                  X   |     -     |     X      |   X   |   -   |    X
text                  X   |     X     |     X      |   X   |   X   |    -
svg                   C   |     C     |     C      |   C   |   C   |    C
g                     C   |     C     |     C      |   C   |   C   |    C
marker                C   |     C     |     C      |   C   |   C   |    C
foreignObject         C   |     C     |     C      |   C   |   C   |    C
use                   -   |     -     |     -      |   -   |   -   |    -

X indicates that the collection of attributes applies to that element
- indicates that the collection of attributes does not apply to that element
C indicates that the collection of attributes does not apply to that element
  but may apply to a child element and hence can still be set
-}

-- These core attributes are applicable to every element
type CoreAttributes r = ("id" .== Text .+ "class" .== Text .+ "style" .== Text .+ "tabIndex" .== Int .+ "lang" .== Text .+ r)

-- Subset of events that work on Firefox 60/Chromium 66
type GlobalEventAttributes r =
  ( "onClick" .== MouseEvent
      .+ "onDoubleClick" .== MouseEvent
      .+ "onContextMenu" .== MouseEvent
      .+ "onKeyDown" .== KeyboardEvent
      .+ "onKeyPress" .== KeyboardEvent
      .+ "onKeyUp" .== KeyboardEvent
      .+ "onMouseDown" .== MouseEvent
      .+ "onMouseEnter" .== MouseEvent
      .+ "onMouseLeave" .== MouseEvent
      .+ "onMouseMove" .== MouseEvent
      .+ "onMouseOut" .== MouseEvent
      .+ "onMouseOver" .== MouseEvent
      .+ "onMouseUp" .== MouseEvent
      .+ "onWheel" .== WheelEvent
      .+ r
  )

type GlobalAttributes r = CoreAttributes (GlobalEventAttributes r)

-- Presentation attributes.+ grouped by applicability (see table above) ---------
type StrokeAttributes r =
  ( "stroke" .== Text
      .+ "strokeDashArray" .== Text
      .+ "strokeDashOffset" .== Double
      .+ "strokeOpacity" .== Double
      .+ "strokeWidth" .== Double
      .+ r
  )

type StokeEndAttributes r =
  ( "strokeLineCap" .== Text
      .+ r
  )

type StrokeJoinAttributes r =
  ( "strokeLineJoin" .== Text
      .+ "strokeMiterLimit" .== Text
      .+ r
  )

type FillAttributes r =
  ( "fill" .== Text
      .+ "fillOpacity" .== Double
      .+ r
  )

type MarkerAttributes r =
  ( "markerStart" .== Text
      .+ "markerMid" .== Text
      .+ "markerEnd" .== Text
      .+ r
  )

type FontAttributes r =
  ( "fontFamily" .== Text
      .+ "fontSize" .== Text
      .+ "fontSizeAdjust" .== Double
      .+ "fontStretch" .== Text
      .+ "fontStyle" .== Text
      .+ "fontVariant" .== Text
      .+ "fontWeight" .== Text
      .+ r
  )

type CanBeMaskedAttributes r =
  ( "mask" .== Text
      .+ r
  )

type AllPresentationAttributes r =
  StrokeAttributes
    ( StrokeJoinAttributes
        ( StokeEndAttributes
            ( FillAttributes
                ( FontAttributes
                    ( MarkerAttributes
                        (CanBeMaskedAttributes r)
                    )
                )
            )
        )
    )

-- Specific SVG elements -------------------------------------------------------
type SVGsvg =
  GlobalAttributes
    ( AllPresentationAttributes
        ( "width" .== Double
            .+ "height" .== Double
            .+ "viewBox" .== Text
            .+ "preserveAspectRatio" .== Text
        )
    )

type SVGg =
  GlobalAttributes
    ( AllPresentationAttributes
        ("transform" .== Text)
    )

type SVGforeignObject =
  GlobalAttributes
    ( AllPresentationAttributes
        ( "x" .== Double
            .+ "y" .== Double
            .+ "height" .== Double
            .+ "width" .== Double
        )
    )

type SVGmarker =
  GlobalAttributes
    ( AllPresentationAttributes
        ( "markerWidth" .== Double
            .+ "markerHeight" .== Double
            .+ "strokeWidth" .== Double
            .+ "refX" .== Double
            .+ "refY" .== Double
            .+ "orient" .== Text
            .+ "markerUnits" .== Text
        )
    )

type SVGmask =
  GlobalAttributes
    ( AllPresentationAttributes
        ( "transform" .== Text
            .+ "x" .== Double
            .+ "y" .== Double
            .+ "width" .== Double
            .+ "height" .== Double
            .+ "maskUnits" .== Text
            .+ "maskContentsUnits" .== Text
        )
    )

type SVGcircle =
  GlobalAttributes
    ( CanBeMaskedAttributes
        ( StrokeAttributes
            ( FillAttributes
                ( MarkerAttributes
                    ( "cx" .== Double
                        .+ "cy" .== Double
                        .+ "r" .== Double
                        .+ "transform" .== Text
                    )
                )
            )
        )
    )

type SVGellipse =
  GlobalAttributes
    ( CanBeMaskedAttributes
        ( StrokeAttributes
            ( FillAttributes
                ( MarkerAttributes
                    ( "cx" .== Double
                        .+ "cy" .== Double
                        .+ "rx" .== Double
                        .+ "ry" .== Double
                        .+ "transform" .== Text
                    )
                )
            )
        )
    )

type SVGline =
  GlobalAttributes
    ( CanBeMaskedAttributes
        ( StrokeAttributes
            ( StokeEndAttributes
                ( MarkerAttributes
                    ( "x1" .== Double
                        .+ "y1" .== Double
                        .+ "x2" .== Double
                        .+ "y2" .== Double
                        .+ "transform" .== Text
                    )
                )
            )
        )
    )

type SVGpolyline =
  GlobalAttributes
    ( CanBeMaskedAttributes
        ( StrokeAttributes
            ( StokeEndAttributes
                ( MarkerAttributes
                    ( "points" .== Text
                        .+ "pathLength" .== Double
                    )
                )
            )
        )
    )

type SVGpolygon =
  GlobalAttributes
    ( CanBeMaskedAttributes
        ( StrokeAttributes
            ( StokeEndAttributes
                ( MarkerAttributes
                    ( "points" .== Text
                        .+ "pathLength" .== Double
                    )
                )
            )
        )
    )

type SVGpath =
  GlobalAttributes
    ( CanBeMaskedAttributes
        ( StrokeAttributes
            ( StokeEndAttributes
                ( StrokeJoinAttributes
                    ( FillAttributes
                        ( MarkerAttributes
                            ( "d" .== Text
                                .+ "transform" .== Text
                            )
                        )
                    )
                )
            )
        )
    )

type SVGpattern =
  GlobalAttributes
    ( "height" .== Double
        .+ "href" .== Text
        .+ "patternContentUnits" .== Text
        .+ "patternTransform" .== Text
        .+ "patternUnits" .== Text
        .+ "preserveAspectRatio" .== Text
        .+ "viewBox" .== Text
        .+ "width" .== Double
        .+ "x" .== Double
        .+ "xlinkHref" .== Text
        .+ "y" .== Double
    )

type SVGrect =
  GlobalAttributes
    ( CanBeMaskedAttributes
        ( StrokeAttributes
            ( StrokeJoinAttributes
                ( FillAttributes
                    ( MarkerAttributes
                        ( "x" .== Double
                            .+ "y" .== Double
                            .+ "rx" .== Double
                            .+ "ry" .== Double
                            .+ "width" .== Double
                            .+ "height" .== Double
                            .+ "transform" .== Text
                        )
                    )
                )
            )
        )
    )

type SVGtext =
  GlobalAttributes
    ( CanBeMaskedAttributes
        ( StrokeAttributes
            ( StokeEndAttributes
                ( StrokeJoinAttributes
                    ( FillAttributes
                        ( FontAttributes
                            ( "x" .== Double
                                .+ "y" .== Double
                                .+ "textAnchor" .== Text
                                .+ "dominantBaseline" .== Text
                                .+ "transform" .== Text
                            )
                        )
                    )
                )
            )
        )
    )

type SVGuse =
  GlobalAttributes
    ( CanBeMaskedAttributes
        ( StrokeAttributes
            ( StokeEndAttributes
                ( StrokeJoinAttributes
                    ( FillAttributes
                        ( FontAttributes
                            ( "x" .== Double
                                .+ "y" .== Double
                                .+ "width" .== Double
                                .+ "height" .== Double
                                .+ "transform" .== Text
                                .+ "href" .== Text
                            )
                        )
                    )
                )
            )
        )
    )

--------------------------------------------------------------------------------

type AnimationAttributes r =
  GlobalAttributes
    ( "from" .== Text
        .+ "to" .== Text
        .+ "begin" .== Text
        .+ "dur" .== Text
        .+ "repeatCount" .== Text
        .+ "fill" .== Text
        .+ r
    )
-- ^ Unlike `fill` in `GlobalAttributes`, `fill` in `AnimationAttributes` is
-- intended to record a `FillState` via `fillAnim`.

type SVGanimate = AnimationAttributes ("attributeName" .== Text)

type SVGanimateMotion = AnimationAttributes ("path" .== Text)

type SVGimage =
  GlobalAttributes
    ( "x" .== Double
        .+ "y" .== Double
        .+ "width" .== Double
        .+ "height" .== Double
        .+ "href" .== Text
        .+ "preserveAspectRatio" .== Text
    )

-- TODO should this have GlobalAttributes?
type SVGmpath = ("xlinkHref" .== Text)

--------------------------------------------------------------------------------

type SVGtitle = GlobalAttributes Empty
