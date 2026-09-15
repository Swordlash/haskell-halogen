-- | Similar to `Halogen.HTML as HH` but contains SVG elements.
-- | We recommend importing this module via the `SE` module alias:
-- | ```
-- | import Halogen.Svg.Elements as SE
-- | ```
module Halogen.Svg.Elements
  ( element
  , svg
  , g
  , circle
  , image
  , ellipse
  , rect
  , path
  , pattern_
  , line
  , polyline
  , polygon
  , text
  , foreignObject
  , defs
  , mask
  , marker
  , animate
  , animateMotion
  , circleNode
  , mpath
  , title
  , use
  )
where

-- Like Halogen.HTML.Elements

import Halogen.HTML.Core (ElemName (..), HTML, Namespace (..))
import Halogen.HTML.Elements (Leaf, Node, elementNS)
import Halogen.HTML.Properties (IProp)
import Halogen.Svg.Indexed qualified as I
import Protolude hiding (mask)

-- | This `element` rather than `Halogen.HTML.Elements.element` must be used
-- | because all SVG elements created via JavaScript must have the svg namespace:
-- | `"http://www.w3.org/2000/svg"`
element :: forall r w i. ElemName -> [IProp r i] -> [HTML w i] -> HTML w i
element = elementNS (Namespace "http://www.w3.org/2000/svg")

svg :: forall p i. Node I.SVGsvg p i
svg = element $ ElemName "svg"

g :: forall p i. Node I.SVGg p i
g = element $ ElemName "g"

circle :: forall p i. Leaf I.SVGcircle p i
circle props = element (ElemName "circle") props []

image :: forall p i. Leaf I.SVGimage p i
image props = element (ElemName "image") props []

ellipse :: forall p i. Leaf I.SVGellipse p i
ellipse props = element (ElemName "ellipse") props []

rect :: forall p i. Leaf I.SVGrect p i
rect props = element (ElemName "rect") props []

path :: forall p i. Leaf I.SVGpath p i
path props = element (ElemName "path") props []

pattern_ :: forall p i. Node I.SVGpattern p i
pattern_ = element (ElemName "pattern")

line :: forall p i. Leaf I.SVGline p i
line props = element (ElemName "line") props []

polyline :: forall p i. Leaf I.SVGpolyline p i
polyline props = element (ElemName "polyline") props []

polygon :: forall p i. Leaf I.SVGpolygon p i
polygon props = element (ElemName "polygon") props []

text :: forall p i. Node I.SVGtext p i
text = element (ElemName "text")

use :: forall p i. Leaf I.SVGuse p i
use props = element (ElemName "use") props []

foreignObject :: forall p i. Node I.SVGforeignObject p i
foreignObject = element (ElemName "foreignObject")

defs :: forall p i. Node I.SVGg p i
defs = element $ ElemName "defs"

mask :: forall p i. Node I.SVGg p i
mask = element $ ElemName "mask"

marker :: forall p i. Node I.SVGmarker p i
marker = element $ ElemName "marker"

--------------------------------------------------------------------------------

animate :: forall p i. Leaf I.SVGanimate p i
animate props = element (ElemName "animate") props []

-- TODO there's also animateTransform, etc
animateMotion :: forall p i. Node I.SVGanimateMotion p i
animateMotion = element (ElemName "animateMotion")

circleNode :: forall p i. Node I.SVGcircle p i
circleNode = element (ElemName "circle")

-- TODO Node or Leaf? mpath :: forall p i. Node I.SVGmpath p i
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Element/mpath
mpath :: forall p i. Leaf I.SVGmpath p i
mpath props = element (ElemName "mpath") props []

--------------------------------------------------------------------------------

title :: forall p i. Node I.SVGtitle p i
title = element (ElemName "title")
