module Halogen.Svg.Attributes (module Halogen.Svg.Attributes) where

import Clay hiding (Baseline, attr, map, max)
import Data.Coerce
import Data.Row
import Data.Text qualified as T
import GHC.Show qualified
import Halogen.HTML.Core qualified as H
import Halogen.HTML.Properties (IProp, attr, attrNS)
import Protolude

--------------------------------------------------------------------------------

data Align
  = Min
  | Mid
  | Max
  deriving (Eq, Show)

--------------------------------------------------------------------------------

data Baseline
  = Auto
  | UseScript
  | NoChange
  | ResetSize
  | Ideographic
  | Alphabetic
  | Hanging
  | Mathematical
  | Central
  | BaselineMiddle
  | TextAfterEdge
  | TextBeforeEdge
  deriving (Eq, Show)

printBaseline :: Baseline -> Text
printBaseline = \case
  Auto -> "auto"
  UseScript -> "use-script"
  NoChange -> "no-change"
  ResetSize -> "reset-size"
  Ideographic -> "ideographic"
  Alphabetic -> "alphabetic"
  Hanging -> "hanging"
  Mathematical -> "mathematical"
  Central -> "central"
  BaselineMiddle -> "middle"
  TextAfterEdge -> "text-after-edge"
  TextBeforeEdge -> "text-before-edge"

--------------------------------------------------------------------------------

newtype PathCommand = PathCommand Text
  deriving newtype (Eq)

instance Show PathCommand where
  show (PathCommand txt) = toS txt

--------------------------------------------------------------------------------

data Duration = Duration
  { hours :: Maybe Double
  , minutes :: Maybe Double
  , seconds :: Maybe Double
  , milliseconds :: Maybe Double
  }

defaultDuration :: Duration
defaultDuration =
  Duration
    { hours = Nothing
    , minutes = Nothing
    , seconds = Nothing
    , milliseconds = Nothing
    }

--------------------------------------------------------------------------------

data FillState
  = Freeze
  | Remove
  deriving (Eq, Show)

printFillState :: FillState -> Text
printFillState = \case
  Freeze -> "freeze"
  Remove -> "remove"

--------------------------------------------------------------------------------

data FontStretch
  = StretchNormal
  | StretchUltraCondensed
  | StretchExtraCondensed
  | StretchCondensed
  | StretchSemiCondensed
  | StretchSemiExpanded
  | StretchExpanded
  | StretchExtraExpanded
  | StretchUltraExpanded
  | StretchPercent Number
  deriving (Eq, Show)

printFontStretch :: FontStretch -> Text
printFontStretch = \case
  StretchNormal -> "normal"
  StretchUltraCondensed -> "ultra-condensed"
  StretchExtraCondensed -> "extra-condensed"
  StretchCondensed -> "condensed"
  StretchSemiCondensed -> "semi-condensed"
  StretchSemiExpanded -> "semi-expanded"
  StretchExpanded -> "expanded"
  StretchExtraExpanded -> "extra-expanded"
  StretchUltraExpanded -> "ultra-expanded"
  StretchPercent n -> show n <> "%"

--------------------------------------------------------------------------------

data MarkerUnit
  = UserSpaceOnUse
  | StrokeWidth
  deriving (Eq, Show)

printMarkerUnit :: MarkerUnit -> Text
printMarkerUnit = \case
  UserSpaceOnUse -> "userSpaceOnUse"
  StrokeWidth -> "strokeWidth"

--------------------------------------------------------------------------------

data MaskUnit
  = UserSpaceOnUse_
  | ObjectBoundingBox
  deriving (Eq, Show)

-- This instance of Show is currently identical to printMaskUnit. That is
-- likely to change so don't rely on it

printMaskUnit :: MaskUnit -> Text
printMaskUnit = \case
  UserSpaceOnUse_ -> "userSpaceOnUse"
  ObjectBoundingBox -> "objectBoundingBox"

--------------------------------------------------------------------------------

data Orient
  = AutoOrient
  | AutoStartReverse
  deriving (Eq, Ord)

printOrient :: Orient -> Text
printOrient = \case
  AutoOrient -> "auto"
  AutoStartReverse -> "auto-start-reverse"

--------------------------------------------------------------------------------

data TextAnchor
  = AnchorStart
  | AnchorMiddle
  | AnchorEnd
  deriving (Eq, Show)

printTextAnchor :: TextAnchor -> Text
printTextAnchor = \case
  AnchorStart -> "start"
  AnchorMiddle -> "middle"
  AnchorEnd -> "end"

--------------------------------------------------------------------------------

data MeetOrSlice
  = Meet
  | Slice
  deriving (Eq, Show)

printMeetOrSlice :: MeetOrSlice -> Text
printMeetOrSlice = \case
  Meet -> "meet"
  Slice -> "slice"

--------------------------------------------------------------------------------

data StrokeLineCap
  = LineCapButt
  | LineCapSquare
  | LineCapRound
  deriving (Eq, Show)

printStrokeLineCap :: StrokeLineCap -> Text
printStrokeLineCap = \case
  LineCapButt -> "butt"
  LineCapSquare -> "square"
  LineCapRound -> "round"

--------------------------------------------------------------------------------

data StrokeLineJoin
  = LineJoinArcs
  | LineJoinBevel
  | LineJoinMiter
  | LineJoinMiterClip
  | LineJoinRound
  deriving (Eq, Show)

printStrokeLineJoin :: StrokeLineJoin -> Text
printStrokeLineJoin = \case
  LineJoinArcs -> "arcs"
  LineJoinBevel -> "bevel"
  LineJoinMiter -> "miter"
  LineJoinMiterClip -> "miter-clip"
  LineJoinRound -> "round"

--------------------------------------------------------------------------------

renderValue :: forall a. (Val a) => a -> Text
renderValue = plain . coerce . value

attributeName :: forall r i. (HasType "attributeName" Text r) => Text -> IProp r i
attributeName = attr (H.AttrName "attributeName")

-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/begin
begin :: forall r i. (HasType "begin" Text r) => Text -> IProp r i
begin = attr (H.AttrName "begin")

class_ :: forall r i. (HasType "class" Text r) => H.ClassName -> IProp r i
class_ = attr (H.AttrName "class") . coerce

classes :: forall r i. (HasType "class" Text r) => [H.ClassName] -> IProp r i
classes = attr (H.AttrName "class") . T.unwords . coerce

cx :: forall r i. (HasType "cx" Double r) => Double -> IProp r i
cx = attr (H.AttrName "cx") . show

cy :: forall r i. (HasType "cy" Double r) => Double -> IProp r i
cy = attr (H.AttrName "cy") . show

d :: forall r i. (HasType "d" Text r) => [PathCommand] -> IProp r i
d = attr (H.AttrName "d") . T.unwords . coerce

dominantBaseline :: forall r i. (HasType "dominantBaseline" Text r) => Baseline -> IProp r i
dominantBaseline = attr (H.AttrName "dominant-baseline") . printBaseline

dur :: forall r i. (HasType "dur" Text r) => Duration -> IProp r i
dur = attr (H.AttrName "dur") . printDuration
  where
    printDuration :: Duration -> Text
    printDuration (Duration {hours, minutes, seconds, milliseconds}) =
      f "h" hours <> f "m" minutes <> f "s" seconds <> f "i" milliseconds

    f unit_ = maybe "" (\val -> show val <> unit_)

fill :: forall r i. (HasType "fill" Text r) => Color -> IProp r i
fill = attr (H.AttrName "fill") . renderValue

-- Note: same as 'fill' but that function is already specialised to Color
fillAnim :: forall r i. (HasType "fill" Text r) => FillState -> IProp r i
fillAnim = attr (H.AttrName "fill") . printFillState

fillOpacity :: forall r i. (HasType "fillOpacity" Double r) => Double -> IProp r i
fillOpacity = attr (H.AttrName "fill-opacity") . show

fontFamily :: forall r i. (HasType "fontFamily" Text r) => Text -> IProp r i
fontFamily = attr (H.AttrName "font-family")

fontSize :: forall r i. (HasType "fontSize" Text r) => FontSize -> IProp r i
fontSize = attr (H.AttrName "font-size") . renderValue

fontSizeAdjust :: forall r i. (HasType "fontSizeAdjust" Text r) => Double -> IProp r i
fontSizeAdjust = attr (H.AttrName "font-size-adjust") . show

fontStretch :: forall r i. (HasType "fontStretch" Text r) => FontStretch -> IProp r i
fontStretch = attr (H.AttrName "font-stretch") . printFontStretch

fontStyle :: forall r i. (HasType "fontStyle" Text r) => FontStyle -> IProp r i
fontStyle = attr (H.AttrName "font-style") . renderValue

fontVariant :: forall r i. (HasType "fontVariant" Text r) => Text -> IProp r i
fontVariant = attr (H.AttrName "font-variant")

fontWeight :: forall r i. (HasType "fontWeight" Text r) => FontWeight -> IProp r i
fontWeight = attr (H.AttrName "font-weight") . renderValue

-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/from
from :: forall r i. (HasType "from" Text r) => Text -> IProp r i
from = attr (H.AttrName "from")

-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/to
to :: forall r i. (HasType "to" Text r) => Text -> IProp r i
to = attr (H.AttrName "to")

id :: forall r i. (HasType "id" Text r) => Text -> IProp r i
id = attr (H.AttrName "id")

markerStart :: forall r i. (HasType "markerStart" Text r) => Text -> IProp r i
markerStart = attr (H.AttrName "marker-start")

markerMid :: forall r i. (HasType "markerMid" Text r) => Text -> IProp r i
markerMid = attr (H.AttrName "marker-mid")

markerEnd :: forall r i. (HasType "markerEnd" Text r) => Text -> IProp r i
markerEnd = attr (H.AttrName "marker-end")

markerUnits :: forall r i. (HasType "markerUnits" Text r) => MarkerUnit -> IProp r i
markerUnits = attr (H.AttrName "markerUnits") . printMarkerUnit

markerWidth :: forall r i. (HasType "markerWidth" Double r) => Double -> IProp r i
markerWidth = attr (H.AttrName "markerWidth") . show

markerHeight :: forall r i. (HasType "markerHeight" Double r) => Double -> IProp r i
markerHeight = attr (H.AttrName "markerHeight") . show

mask :: forall r i. (HasType "mask" Text r) => Text -> IProp r i
mask = attr (H.AttrName "mask")

maskUnits :: forall r i. (HasType "maskUnits" Text r) => MaskUnit -> IProp r i
maskUnits = attr (H.AttrName "maskUnits") . printMaskUnit

maskContentUnits :: forall r i. (HasType "maskContentUnits" Text r) => MaskUnit -> IProp r i
maskContentUnits = attr (H.AttrName "maskContentUnits") . printMaskUnit

orient :: forall r i. (HasType "orient" Text r) => Orient -> IProp r i
orient = attr (H.AttrName "orient") . printOrient

path :: forall r i. (HasType "path" Text r) => [PathCommand] -> IProp r i
path = attr (H.AttrName "path") . T.intercalate " " . coerce

-- | An array of x-y value pairs (e.g. `[(x, y)]`).
points :: forall r i. (HasType "points" Text r) => [(Double, Double)] -> IProp r i
points = attr (H.AttrName "points") . T.intercalate " " . map (\(x_, y_) -> show x_ <> "," <> show y_)

pathLength :: forall r i. (HasType "pathLength" Text r) => Double -> IProp r i
pathLength = attr (H.AttrName "pathLength") . show

patternContentUnits :: forall r i. (HasType "patternContentUnits" Text r) => Text -> IProp r i
patternContentUnits = attr (H.AttrName "patternContentUnits")

patternTransformation :: forall r i. (HasType "patternTransformation" Text r) => [Transformation] -> IProp r i
patternTransformation = attr (H.AttrName "patternTransformation") . T.unwords . map renderValue

patternUnits :: forall r i. (HasType "patternUnits" Text r) => Text -> IProp r i
patternUnits = attr (H.AttrName "patternUnits")

preserveAspectRatio
  :: forall r i
   . (HasType "preserveAspectRatio" Text r)
  => Maybe (Align, Align)
  -> MeetOrSlice
  -> IProp r i
preserveAspectRatio align slice =
  attr
    (H.AttrName "preserveAspectRatio")
    (T.intercalate " " $ [align_str, printMeetOrSlice slice])
  where
    align_str = case align of
      Nothing -> "none"
      Just (x_, y_) -> T.intercalate "" $ ["x", show x_, "Y", show y_]

r :: forall r i. (HasType "r" Text r) => Double -> IProp r i
r = attr (H.AttrName "r") . show

refX :: forall r i. (HasType "refX" Text r) => Double -> IProp r i
refX = attr (H.AttrName "refX") . show

refY :: forall r i. (HasType "refY" Text r) => Double -> IProp r i
refY = attr (H.AttrName "refY") . show

repeatCount :: forall r i. (HasType "repeatCount" Text r) => Text -> IProp r i
repeatCount = attr (H.AttrName "repeatCount")

rx :: forall r i. (HasType "rx" Text r) => Double -> IProp r i
rx = attr (H.AttrName "rx") . show

ry :: forall r i. (HasType "ry" Text r) => Double -> IProp r i
ry = attr (H.AttrName "ry") . show

stroke :: forall r i. (HasType "stroke" Text r) => Color -> IProp r i
stroke = attr (H.AttrName "stroke") . renderValue

strokeDashArray :: forall r i. (HasType "strokeDashArray" Text r) => Text -> IProp r i
strokeDashArray = attr (H.AttrName "stroke-dasharray")

strokeDashOffset :: forall r i. (HasType "strokeDashOffset" Text r) => Double -> IProp r i
strokeDashOffset = attr (H.AttrName "stroke-dashoffset") . show

strokeLineCap :: forall r i. (HasType "strokeLineCap" Text r) => StrokeLineCap -> IProp r i
strokeLineCap = attr (H.AttrName "stroke-linecap") . printStrokeLineCap

strokeLineJoin :: forall r i. (HasType "strokeLineJoin" Text r) => StrokeLineJoin -> IProp r i
strokeLineJoin = attr (H.AttrName "stroke-linejoin") . printStrokeLineJoin

-- | The `Double` arg must be greater than or equal to 1. Thus, this function
-- | will use `1.0` if given any value less than `1.0`.
strokeMiterLimit :: forall r i. (HasType "strokeMiterLimit" Text r) => Double -> IProp r i
strokeMiterLimit = attr (H.AttrName "stroke-miterlimit") . show . max 1.0

strokeOpacity :: forall r i. (HasType "strokeOpacity" Text r) => Double -> IProp r i
strokeOpacity = attr (H.AttrName "stroke-opacity") . show

strokeWidth :: forall r i. (HasType "strokeWidth" Text r) => Double -> IProp r i
strokeWidth = attr (H.AttrName "stroke-width") . show

textAnchor :: forall r i. (HasType "textAnchor" Text r) => TextAnchor -> IProp r i
textAnchor = attr (H.AttrName "text-anchor") . printTextAnchor

transformation :: forall r i. (HasType "Transformation" Text r) => [Transformation] -> IProp r i
transformation = attr (H.AttrName "Transformation") . T.unwords . map renderValue

viewBox
  :: forall r i
   . (HasType "viewBox" Text r)
  => Double
  -> Double
  -> Double
  -> Double
  -> IProp r i
viewBox x_ y_ w h_ =
  attr (H.AttrName "viewBox") (T.unwords $ map show [x_, y_, w, h_])

width :: forall r i. (HasType "width" Text r) => Double -> IProp r i
width = attr (H.AttrName "width") . show

height :: forall r i. (HasType "height" Text r) => Double -> IProp r i
height = attr (H.AttrName "height") . show

x :: forall r i. (HasType "x" Text r) => Double -> IProp r i
x = attr (H.AttrName "x") . show

y :: forall r i. (HasType "y" Text r) => Double -> IProp r i
y = attr (H.AttrName "y") . show

x1 :: forall r i. (HasType "x1" Text r) => Double -> IProp r i
x1 = attr (H.AttrName "x1") . show

y1 :: forall r i. (HasType "y1" Text r) => Double -> IProp r i
y1 = attr (H.AttrName "y1") . show

x2 :: forall r i. (HasType "x2" Text r) => Double -> IProp r i
x2 = attr (H.AttrName "x2") . show

y2 :: forall r i. (HasType "y2" Text r) => Double -> IProp r i
y2 = attr (H.AttrName "y2") . show

href :: forall r i. (HasType "href" Text r) => Text -> IProp r i
href = attr (H.AttrName "href")

xlinkHref :: forall r i. (HasType "xlinkHref" Text r) => Text -> IProp r i
xlinkHref = attrNS (H.Namespace "xlink") (H.AttrName "xlink:href")
