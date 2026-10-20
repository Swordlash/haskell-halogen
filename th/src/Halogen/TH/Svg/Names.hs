{-# LANGUAGE TemplateHaskellQuotes #-}

-- | The names an SVG template compiles to: the element and attribute
-- functions of "Halogen.Svg", by the name written in SVG. SVG names keep
-- their case (@viewBox@, @foreignObject@); an attribute may also be
-- written as its function is named (@strokeWidth@ for @stroke-width@). A
-- name missing here becomes a plain element (in SVG's namespace) or
-- attribute, as written.
module Halogen.TH.Svg.Names
  ( element
  , property
  , attributeName
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Halogen.Svg.Attributes qualified as SA
import Halogen.Svg.Elements qualified as SE
import Halogen.TH.Html.Names (ElementKind (..), PropKind (..))

element :: Text -> Maybe ElementKind
element = (`Map.lookup` elements)

elements :: Map Text ElementKind
elements =
  Map.fromList $
    [(n, WithChildren x) | (n, x) <- nodes] <> [(n, NoChildren x) | (n, x) <- leaves]
  where
    nodes =
      [ ("svg", 'SE.svg), ("g", 'SE.g), ("text", 'SE.text), ("defs", 'SE.defs), ("mask", 'SE.mask), ("marker", 'SE.marker)
      , ("pattern", 'SE.pattern_), ("foreignObject", 'SE.foreignObject), ("title", 'SE.title), ("animateMotion", 'SE.animateMotion)
      ]
    leaves =
      [ ("circle", 'SE.circle), ("ellipse", 'SE.ellipse), ("image", 'SE.image), ("line", 'SE.line), ("path", 'SE.path)
      , ("polygon", 'SE.polygon), ("polyline", 'SE.polyline), ("rect", 'SE.rect), ("use", 'SE.use), ("animate", 'SE.animate)
      , ("mpath", 'SE.mpath)
      ]

-- | By the attribute's name as SVG writes it, or as its function is named.
-- | An attribute of an element: @fill@ on an animation is the state it
-- leaves ('SA.fillAnim', @freeze@ or @remove@), not a colour.
property :: Text -> Text -> Maybe PropKind
property el name
  | name == "fill" && el `elem` ["animate", "animateMotion", "animateTransform", "set"] = Just (TypedProp 'SA.fillAnim)
  | otherwise = Map.lookup name properties

properties :: Map Text PropKind
properties =
  Map.fromList $
    [ ("class", SvgClassProp)
    , ("style", StyleProp)
    , ("ref", RefProp)
    , ("viewBox", ViewBoxProp)
    , ("id", TextProp 'SA.id)
    , ("href", TextProp 'SA.href)
    , ("xlink:href", TextProp 'SA.xlinkHref)
    , ("font-family", TextProp 'SA.fontFamily)
    , ("fontFamily", TextProp 'SA.fontFamily)
    , ("font-variant", TextProp 'SA.fontVariant)
    , ("fontVariant", TextProp 'SA.fontVariant)
    ]
      <> [(n, DoubleProp x) | (n, x) <- numbers]
      <> [(n, TypedProp x) | (n, x) <- typed]
  where
    numbers =
      [ ("x", 'SA.x), ("y", 'SA.y), ("x1", 'SA.x1), ("y1", 'SA.y1), ("x2", 'SA.x2), ("y2", 'SA.y2)
      , ("cx", 'SA.cx), ("cy", 'SA.cy), ("r", 'SA.r), ("rx", 'SA.rx), ("ry", 'SA.ry)
      , ("width", 'SA.width), ("height", 'SA.height), ("refX", 'SA.refX), ("refY", 'SA.refY)
      , ("pathLength", 'SA.pathLength), ("markerWidth", 'SA.markerWidth), ("markerHeight", 'SA.markerHeight)
      ]
        <> both
          [ ("stroke-width", "strokeWidth", 'SA.strokeWidth), ("stroke-opacity", "strokeOpacity", 'SA.strokeOpacity)
          , ("fill-opacity", "fillOpacity", 'SA.fillOpacity), ("stroke-dashoffset", "strokeDashOffset", 'SA.strokeDashOffset)
          , ("font-size-adjust", "fontSizeAdjust", 'SA.fontSizeAdjust)
          ]
    typed =
      [ ("fill", 'SA.fill), ("stroke", 'SA.stroke), ("d", 'SA.d), ("points", 'SA.points), ("transform", 'SA.transform)
      , ("patternTransform", 'SA.patternTransform), ("orient", 'SA.orient), ("dur", 'SA.dur), ("path", 'SA.path)
      , ("markerUnits", 'SA.markerUnits), ("maskUnits", 'SA.maskUnits), ("maskContentUnits", 'SA.maskContentUnits)
      ]
        <> both
          [ ("stroke-linecap", "strokeLineCap", 'SA.strokeLineCap), ("stroke-linejoin", "strokeLineJoin", 'SA.strokeLineJoin)
          , ("stroke-miterlimit", "strokeMiterLimit", 'SA.strokeMiterLimit), ("stroke-dasharray", "strokeDashArray", 'SA.strokeDashArray)
          , ("text-anchor", "textAnchor", 'SA.textAnchor), ("dominant-baseline", "dominantBaseline", 'SA.dominantBaseline)
          , ("font-size", "fontSize", 'SA.fontSize), ("font-weight", "fontWeight", 'SA.fontWeight), ("font-style", "fontStyle", 'SA.fontStyle)
          , ("font-stretch", "fontStretch", 'SA.fontStretch)
          , ("marker-start", "markerStart", 'SA.markerStart), ("marker-mid", "markerMid", 'SA.markerMid), ("marker-end", "markerEnd", 'SA.markerEnd)
          ]
    both xs = concat [[(svgName, x), (fnName, x)] | (svgName, fnName, x) <- xs]

-- | The attribute an attribute written as its function is named stands
-- for (@strokeWidth@: @stroke-width@); any other name as it is.
attributeName :: Text -> Text
attributeName n = Map.findWithDefault n n kebab
  where
    kebab =
      Map.fromList
        [ ("strokeWidth", "stroke-width"), ("strokeOpacity", "stroke-opacity"), ("fillOpacity", "fill-opacity")
        , ("strokeDashOffset", "stroke-dashoffset"), ("fontSizeAdjust", "font-size-adjust"), ("strokeLineCap", "stroke-linecap")
        , ("strokeLineJoin", "stroke-linejoin"), ("strokeMiterLimit", "stroke-miterlimit"), ("strokeDashArray", "stroke-dasharray")
        , ("textAnchor", "text-anchor"), ("dominantBaseline", "dominant-baseline"), ("fontSize", "font-size")
        , ("fontWeight", "font-weight"), ("fontStyle", "font-style"), ("fontFamily", "font-family"), ("markerStart", "marker-start")
        , ("markerMid", "marker-mid"), ("markerEnd", "marker-end"), ("fontStretch", "font-stretch"), ("fontVariant", "font-variant")
        ]
