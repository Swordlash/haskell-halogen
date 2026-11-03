{-# LANGUAGE TemplateHaskell #-}

-- | Focus events. The field (@e.relatedTarget@) is 'FocusEvent''s
-- instance, here; the same as a function is meant to be used qualified
-- (@FE.relatedTarget@).
module Web.UIEvent.FocusEvent
  ( FocusEvent (..)
  , toEvent
  , fromEvent
  , relatedTarget
  )
where

import HPrelude
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsCast, jsFieldFunctions, jsFields)
import Web.Event.Internal.Types

newtype FocusEvent = FocusEvent Event
  deriving newtype (Inert)

-- | Every FocusEvent is an 'Event', which is what the operations on events take.
toEvent :: FocusEvent -> Event
toEvent (FocusEvent e) = e

-- Pure, because what an event says about the change of focus is fixed when
-- it is made.
$( jsFields
     ''FocusEvent
     [ -- The other end of the change of focus: where the focus goes, for
       -- @blur@ and @focusout@, and where it comes from, for @focus@ and
       -- @focusin@. 'Nothing' when that is outside the page, or nothing.
       Fixed "relatedTarget" [t|Maybe EventTarget|]
     ]
 )

$(jsFieldFunctions ''FocusEvent [])

$(jsCast "fromEvent" ''Event ''FocusEvent)
