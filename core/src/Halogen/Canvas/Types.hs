-- | The vocabulary a canvas scene is described in.
--
-- Backend-neutral by construction: nothing here mentions Pixi, a browser
-- canvas, or SVG. A backend interprets these; it does not extend them, which
-- is what lets a scene be written once and rendered by any of them.
module Halogen.Canvas.Types
  ( Point (..)
  , Transform (..)
  , defaultTransform
  , FillStyle (..)
  , StrokeStyle (..)
  , Font (..)
  , TextAlign (..)
  , TextStyle (..)
  , Texture (..)
  , Camera (..)
  , defaultCamera
  , followCamera
  , Interaction (..)
  , defaultInteraction
  , Shape (..)
  , pathData
  , HitArea (..)
  , EventMode (..)
  , eventModeName
  , Cursor (..)
  , ResizeDirection (..)
  , cursorName
  , textAlignName
  )
where

import Data.Text qualified as T
import HPrelude
import Halogen.Svg.Attributes (PathCommand)

data Point = Point Double Double
  deriving stock (Eq, Show)

data Transform = Transform
  { position :: Point
  , scale :: Point
  , rotation :: Double
  }
  deriving stock (Eq, Show)

defaultTransform :: Transform
defaultTransform = Transform {position = Point 0 0, scale = Point 1 1, rotation = 0}

data FillStyle = FillStyle
  { fillColor :: Int
  , fillAlpha :: Double
  }
  deriving stock (Eq, Show)

data StrokeStyle = StrokeStyle
  { strokeColor :: Int
  , strokeWidth :: Double
  , strokeAlpha :: Double
  }
  deriving stock (Eq, Show)

data Font
  = SystemFont Text
  | AssetFont
      { family :: Text
      , source :: Text
      }
  deriving stock (Eq, Show)

data TextAlign = AlignLeft | AlignCenter | AlignRight
  deriving stock (Eq, Show)

textAlignName :: TextAlign -> Text
textAlignName = \case
  AlignLeft -> "left"
  AlignCenter -> "center"
  AlignRight -> "right"

data TextStyle = TextStyle
  { font :: Font
  , fontSize :: Double
  , textColor :: Int
  , textAlign :: TextAlign
  }
  deriving stock (Eq, Show)

newtype Texture = Texture
  { asset :: Text
  }
  deriving stock (Eq, Ord, Show)

data Camera = Camera
  { focus :: Point
  , zoom :: Double
  }
  deriving stock (Eq, Show)

defaultCamera :: Camera
defaultCamera = Camera {focus = Point 0 0, zoom = 1}

-- | Which camera a renderer should show, given the one the application asked
-- for last time, the one it asks for now, and the one on screen.
--
-- The camera in a view is the application's to set, but the user moves the
-- one on screen, and a renderer only tells the application where it went once
-- the gesture is over. Until then the application still holds the camera from
-- before, and a view rendered meanwhile for any other reason (a hover, a
-- timer) carries that one. Taken at its word, it would put the camera back
-- where the drag began. So a view's camera is followed only when it differs
-- from what the application asked for before: an application that wants to
-- move the camera changes it, one that has nothing to say about it repeats it.
followCamera :: Maybe Camera -> Camera -> Camera -> Camera
followCamera asked next onScreen
  | asked == Just next = onScreen
  | otherwise = next

data Interaction = Interaction
  { pan :: Bool
  , zoomRange :: Maybe (Double, Double)
  }
  deriving stock (Eq, Show)

defaultInteraction :: Interaction
defaultInteraction = Interaction {pan = True, zoomRange = Just (0.25, 3)}

-- | What a graphics element draws.
--
-- One value rather than a prop each for geometry, fill and stroke, because a
-- retained-mode backend replays the whole command sequence when any part of
-- it changes - there is no way to update just the fill of a drawn path. 'Eq'
-- on this is exactly the repaint test.
data Shape
  = Line Point Point StrokeStyle
  | Rectangle Point Point (Maybe FillStyle) (Maybe StrokeStyle)
  | Circle Point Double (Maybe FillStyle) (Maybe StrokeStyle)
  | Ellipse Point Point (Maybe FillStyle) (Maybe StrokeStyle)
  | QuadraticBezier Point Point Point StrokeStyle
  | Bezier Point Point Point Point StrokeStyle
  | Arc Point Double Double Double Bool StrokeStyle
  | -- | An arbitrary outline, in the same path commands SVG uses.
    --
    -- The one primitive the others cannot stand in for: a filled shape that
    -- is not a rectangle, a circle or an ellipse. The commands are the ones
    -- in "Halogen.Svg.Attributes", so a path written for an @\<svg\>@ and
    -- one drawn on a canvas are the same value.
    --
    -- Coordinates are absolute, as they are for 'Line' and the curves.
    Path [PathCommand] (Maybe FillStyle) (Maybe StrokeStyle)
  deriving stock (Eq, Show)

-- | A path's commands as an SVG @d@ attribute, which is what a backend is
-- given to draw.
pathData :: [PathCommand] -> Text
pathData = T.unwords . map show

-- | An explicit hit region, given as a centre and an extent.
--
-- Worth setting more often than you would expect. A stroke-only shape is hit
-- only along the stroke itself. A group is hit only where one of its children
-- is, so a group whose children are not themselves listening - the usual case,
-- when the handler is on the group - is not hit anywhere at all.
data HitArea
  = -- | Centre and size, as 'Rectangle' takes them.
    RectHit Point Point
  | -- | Centre and radius, as 'Circle' takes them.
    CircleHit Point Double
  deriving stock (Eq, Show)

-- | How an element takes part in hit testing.
data EventMode
  = -- | Never hit-tested, and neither are its children.
    EventsNone
  | -- | Not a target itself, but its children are.
    EventsPassive
  | -- | A target whenever something up the tree is listening.
    EventsAuto
  | -- | Always a target.
    EventsStatic
  deriving stock (Eq, Show)

eventModeName :: EventMode -> Text
eventModeName = \case
  EventsNone -> "none"
  EventsPassive -> "passive"
  EventsAuto -> "auto"
  EventsStatic -> "static"

-- | What the pointer looks like over an element.
--
-- The CSS cursor keywords, which is the closed set every backend that has a
-- pointer at all ends up speaking - a canvas is a DOM element, and so is an
-- SVG. An enumeration rather than a string because a misspelled keyword is
-- silently ignored by the browser, which is the worst way to find out.
data Cursor
  = CursorAuto
  | CursorDefault
  | CursorNone
  | CursorContextMenu
  | CursorHelp
  | CursorPointer
  | CursorProgress
  | CursorWait
  | CursorCell
  | CursorCrosshair
  | CursorText
  | CursorVerticalText
  | CursorAlias
  | CursorCopy
  | CursorMove
  | CursorNoDrop
  | CursorNotAllowed
  | CursorGrab
  | CursorGrabbing
  | CursorAllScroll
  | CursorZoomIn
  | CursorZoomOut
  | -- | The resize family, which is large enough to be worth naming as one.
    CursorResize ResizeDirection
  | -- | A custom image, by URL. The one thing that cannot be enumerated.
    CursorUrl Text
  deriving stock (Eq, Show)

-- | Which edge or corner a resize cursor points at.
data ResizeDirection
  = ResizeN
  | ResizeE
  | ResizeS
  | ResizeW
  | ResizeNE
  | ResizeNW
  | ResizeSE
  | ResizeSW
  | -- | Both horizontal directions, for a vertical edge.
    ResizeEW
  | -- | Both vertical directions, for a horizontal edge.
    ResizeNS
  | ResizeNESW
  | ResizeNWSE
  | -- | A column boundary.
    ResizeCol
  | -- | A row boundary.
    ResizeRow
  deriving stock (Eq, Show)

cursorName :: Cursor -> Text
cursorName = \case
  CursorAuto -> "auto"
  CursorDefault -> "default"
  CursorNone -> "none"
  CursorContextMenu -> "context-menu"
  CursorHelp -> "help"
  CursorPointer -> "pointer"
  CursorProgress -> "progress"
  CursorWait -> "wait"
  CursorCell -> "cell"
  CursorCrosshair -> "crosshair"
  CursorText -> "text"
  CursorVerticalText -> "vertical-text"
  CursorAlias -> "alias"
  CursorCopy -> "copy"
  CursorMove -> "move"
  CursorNoDrop -> "no-drop"
  CursorNotAllowed -> "not-allowed"
  CursorGrab -> "grab"
  CursorGrabbing -> "grabbing"
  CursorAllScroll -> "all-scroll"
  CursorZoomIn -> "zoom-in"
  CursorZoomOut -> "zoom-out"
  CursorResize direction -> resizeDirectionName direction <> "-resize"
  -- CSS requires a keyword to fall back on when the image will not load.
  CursorUrl url -> "url(" <> url <> "), auto"

resizeDirectionName :: ResizeDirection -> Text
resizeDirectionName = \case
  ResizeN -> "n"
  ResizeE -> "e"
  ResizeS -> "s"
  ResizeW -> "w"
  ResizeNE -> "ne"
  ResizeNW -> "nw"
  ResizeSE -> "se"
  ResizeSW -> "sw"
  ResizeEW -> "ew"
  ResizeNS -> "ns"
  ResizeNESW -> "nesw"
  ResizeNWSE -> "nwse"
  ResizeCol -> "col"
  ResizeRow -> "row"
