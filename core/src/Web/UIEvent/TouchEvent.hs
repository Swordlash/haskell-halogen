{-# LANGUAGE TemplateHaskell #-}

-- | Touch events, and the touches they carry. The fields
-- (@e.changedTouches@, @touch.clientX@) are the instances here; the same
-- as functions are meant to be used qualified (@TE.changedTouches@).
module Web.UIEvent.TouchEvent
  ( TouchEvent (..)
  , toEvent
  , fromEvent
  , touches
  , changedTouches
  , firstChangedTouch

    -- * The touches
  , TouchList (..)
  , length
  , item
  , Touch (..)
  , clientX
  , clientY
  )
where

import Data.Foreign (Foreign)
import HPrelude hiding (length)
import Halogen.JSBits (Inert, JSVal, isNull)
import Halogen.JSBits.Fields (Member (..), inlineJS, jsCast, jsFieldFunctions, jsFields)
import Web.Event.Internal.Types

newtype TouchEvent = TouchEvent Event
  deriving newtype (Inert)

-- | Every TouchEvent is an 'Event', which is what the operations on events take.
toEvent :: TouchEvent -> Event
toEvent (TouchEvent e) = e

-- | The touches of an event: a @TouchList@.
newtype TouchList = TouchList (Foreign TouchList)
  deriving newtype (Inert)

-- | One finger on the screen: a @Touch@.
newtype Touch = Touch (Foreign Touch)
  deriving newtype (Inert)

-- Pure, because what an event says about its touches is fixed when it is
-- made.
$( jsFields
     ''TouchEvent
     [ -- Every finger on the screen.
       Fixed "touches" ''TouchList
     , -- The fingers the event is about: the ones that came down, moved or
       -- went up.
       Fixed "changedTouches" ''TouchList
     ]
 )

$( jsFields
     ''TouchList
     [ -- How many touches: fixed, since the list is the event's.
       Fixed "length" ''Int
     , -- The touch at a position, from 0, or 'Nothing' past the end.
       Method "item" [t|Int -> IO (Maybe Touch)|]
     ]
 )

$( jsFields
     ''Touch
     [ -- Where the finger is, in CSS pixels from the top left corner of
       -- the viewport.
       Fixed "clientX" ''Double
     , Fixed "clientY" ''Double
     ]
 )

$(jsFieldFunctions ''TouchEvent [])

$(jsFieldFunctions ''TouchList [])

$(jsFieldFunctions ''Touch [])

$(jsCast "fromEvent" ''Event ''TouchEvent)

$(inlineJS "js_first_changed_touch" "($1.changedTouches[0] ?? null)" [t|TouchEvent -> JSVal|])

-- | The first of the touches the event is about, read purely, so that a
-- handler can decide in the event's listener where a touch started or
-- ended ('Halogen.HTML.Events.handler'' decides there). 'Nothing' for an
-- event with none, and natively.
firstChangedTouch :: TouchEvent -> Maybe Touch
firstChangedTouch event =
  let touch = js_first_changed_touch event
   in if isNull touch then Nothing else Just (coerce touch)
