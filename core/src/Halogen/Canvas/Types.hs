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
  , Interaction (..)
  , defaultInteraction
  , Shape (..)
  , HitArea (..)
  , EventMode (..)
  , eventModeName
  , textAlignName
  )
where

import HPrelude

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
  deriving stock (Eq, Show)

-- | An explicit hit region.
--
-- Worth setting more often than you would expect: a stroke-only shape is hit
-- only on the stroke itself, and a group has no bounds of its own at all.
data HitArea
  = RectHit Point Point
  | CircleHit Point Double
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
