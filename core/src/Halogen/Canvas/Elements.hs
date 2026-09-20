-- | Canvas elements, in the shape "Halogen.HTML.Elements" uses.
--
-- Geometry is positional and styling is a prop, which is the one place this
-- deviates from the HTML convention: there, an element with no children takes
-- only props and gets no @_@ variant. A canvas primitive with no geometry is
-- meaningless, so geometry stays mandatory and the @_@ suffix means "no
-- styling props" rather than "no children".
module Halogen.Canvas.Elements
  ( group
  , group_
  , keyedGroup
  , withKeys
  , line
  , line_
  , rectangle
  , rectangle_
  , circle
  , circle_
  , ellipse
  , arc
  , quadraticBezier
  , bezier
  , path
  , path_
  , text
  , text_
  , sprite
  , sprite_
  )
where

import HPrelude hiding (group)
import Halogen.Canvas.Core
import Halogen.Canvas.Types
import Halogen.Svg.Attributes (PathCommand)
import Halogen.VDom.Types (ElemName (..))

graphicsName, textName, spriteName, containerName :: ElemName
graphicsName = ElemName "graphics"
textName = ElemName "text"
spriteName = ElemName "sprite"
containerName = ElemName "container"

group :: CanvasElem event i
group = element containerName

group_ :: [CanvasNode event i] -> CanvasNode event i
group_ = group []

keyedGroup :: [CanvasProp event i] -> [(Text, CanvasNode event i)] -> CanvasNode event i
keyedGroup = keyed containerName

drawing :: Shape -> [CanvasProp event i] -> CanvasNode event i
drawing shape props = element graphicsName (Draw shape : props) []

line :: Point -> Point -> StrokeStyle -> CanvasLeaf event i
line start end stroke = drawing (Line start end stroke)

line_ :: Point -> Point -> StrokeStyle -> CanvasNode event i
line_ start end stroke = line start end stroke []

rectangle :: Point -> Point -> Maybe FillStyle -> Maybe StrokeStyle -> CanvasLeaf event i
rectangle position size fill stroke = drawing (Rectangle position size fill stroke)

rectangle_ :: Point -> Point -> Maybe FillStyle -> Maybe StrokeStyle -> CanvasNode event i
rectangle_ position size fill stroke = rectangle position size fill stroke []

circle :: Point -> Double -> Maybe FillStyle -> Maybe StrokeStyle -> CanvasLeaf event i
circle position radius fill stroke = drawing (Circle position radius fill stroke)

circle_ :: Point -> Double -> Maybe FillStyle -> Maybe StrokeStyle -> CanvasNode event i
circle_ position radius fill stroke = circle position radius fill stroke []

ellipse :: Point -> Point -> Maybe FillStyle -> Maybe StrokeStyle -> CanvasLeaf event i
ellipse position radii fill stroke = drawing (Ellipse position radii fill stroke)

arc :: Point -> Double -> Double -> Double -> Bool -> StrokeStyle -> CanvasLeaf event i
arc position radius startAngle endAngle anticlockwise stroke =
  drawing (Arc position radius startAngle endAngle anticlockwise stroke)

quadraticBezier :: Point -> Point -> Point -> StrokeStyle -> CanvasLeaf event i
quadraticBezier start control end stroke = drawing (QuadraticBezier start control end stroke)

bezier :: Point -> Point -> Point -> Point -> StrokeStyle -> CanvasLeaf event i
bezier start control1 control2 end stroke = drawing (Bezier start control1 control2 end stroke)

-- | An arbitrary outline, in SVG path commands.
--
-- Unlike the other primitives this one can be filled as well as stroked, so
-- both styles are given the way 'rectangle' takes them.
path :: [PathCommand] -> Maybe FillStyle -> Maybe StrokeStyle -> CanvasLeaf event i
path commands fill stroke = drawing (Path commands fill stroke)

path_ :: [PathCommand] -> Maybe FillStyle -> Maybe StrokeStyle -> CanvasNode event i
path_ commands fill stroke = path commands fill stroke []

-- | A label, positioned at its top left.
--
-- The position is a default: it is here because a label has no geometry of
-- its own to carry one, and an explicit 'Halogen.Canvas.Properties.transform'
-- or 'Halogen.Canvas.Properties.at' in the props replaces it outright rather
-- than combining with it.
text :: Point -> Text -> TextStyle -> CanvasLeaf event i
text position value style props =
  element textName (Label value style : placing position props) []

text_ :: Point -> Text -> TextStyle -> CanvasNode event i
text_ position value style = text position value style []

-- | A textured rectangle, centred on its position.
--
-- The position is a default, exactly as it is for 'text'.
sprite :: Point -> Point -> Texture -> CanvasLeaf event i
sprite position size texture props =
  element spriteName (Src texture size : placing position props) []

sprite_ :: Point -> Point -> Texture -> CanvasNode event i
sprite_ position size texture = sprite position size texture []

-- | Supply a position, unless the caller already did.
--
-- Emitting both would leave two props under one key. The reconciler resolves
-- that - the last one wins - but a prop the caller cannot see is a poor thing
-- to make them reason about, so the duplicate is never built.
placing :: Point -> [CanvasProp event i] -> [CanvasProp event i]
placing position props
  | any placed props = props
  | otherwise = Place defaultTransform {position} : props
  where
    placed = \case
      Place _ -> True
      _ -> False
