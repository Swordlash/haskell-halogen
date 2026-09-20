{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The CSS properties governing how a pointer reaches an element, which
-- Clay does not cover.
module Clay.Extra.Pointer
  ( TouchAction
  , touchAction
  , panX
  , panLeft
  , panRight
  , panY
  , panUp
  , panDown
  , pinchZoom
  , manipulation
  )
where

import Clay.Common (Auto, Inherit, Initial, None, Other, Revert, RevertLayer, Unset)
import Clay.Property (Val, Value)
import Clay.Stylesheet (Css, key)
import Prelude ()

newtype TouchAction = TouchAction Value
  deriving (Val, Other, None, Auto, Inherit, Initial, Unset, Revert, RevertLayer)

-- | Which gestures the browser handles itself instead of passing to the page.
--
-- @none@ is what an element that interprets gestures of its own needs — a
-- canvas that pans and zooms with a finger, say — since the browser would
-- otherwise scroll the page out from under it.
touchAction :: TouchAction -> Css
touchAction = key "touch-action"

-- | Horizontal panning, which the browser keeps.
panX, panLeft, panRight :: TouchAction
panX = TouchAction "pan-x"
panLeft = TouchAction "pan-left"
panRight = TouchAction "pan-right"

-- | Vertical panning, which the browser keeps.
panY, panUp, panDown :: TouchAction
panY = TouchAction "pan-y"
panUp = TouchAction "pan-up"
panDown = TouchAction "pan-down"

-- | Multi-finger zooming, which the browser keeps.
pinchZoom :: TouchAction
pinchZoom = TouchAction "pinch-zoom"

-- | Panning and pinch zoom, but not the double-tap that delays a click by
-- 300ms while the browser waits to see whether a second tap follows.
manipulation :: TouchAction
manipulation = TouchAction "manipulation"
