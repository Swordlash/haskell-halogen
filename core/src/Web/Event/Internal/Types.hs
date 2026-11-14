{-# LANGUAGE TemplateHaskell #-}

-- | The event types, and an event's fields ("Halogen.JSBits.Fields"): here,
-- beside the newtype, so that the instances are not orphans. The same
-- fields as functions are in "Web.Event.Event".
module Web.Event.Internal.Types
  ( Event (..)
  , EventTarget (..)
  )
where

import Data.Foreign (Foreign, readProp)
import HPrelude
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsFields)

newtype Event = Event (Foreign Event)
  deriving newtype (Inert)

newtype EventTarget = EventTarget (Foreign EventTarget)
  deriving newtype (Inert)

$( jsFields
     ''Event
     [ -- The event's type: @"click"@, @"keydown"@.
       Fixed "type" ''Text
     , -- The node the event was dispatched to. Natively it is the event's
       -- @target@ property, as an event made from JSON (a test's, or one a
       -- remote client sent) has it: what the client described of the
       -- element ("Web.DOM.Element" reads it).
       Natively
         [|\(Event value) -> EventTarget <$> readProp "target" Just value|]
         (Fixed "target" [t|Maybe EventTarget|])
     , -- The target whose handler is running. The browser changes it as
       -- the event travels and sets it to null once the dispatch is over,
       -- so read it in the handler itself. Natively it is the event's
       -- @currentTarget@ property, which an event made from JSON (a
       -- test's, or one a remote client sent) has and every other event
       -- lacks.
       Natively
         [|\(Event value) -> EventTarget <$> readProp "currentTarget" Just value|]
         (Fixed "currentTarget" [t|Maybe EventTarget|])
     , -- Whether a handler has prevented the browser's default action.
       Live "defaultPrevented" ''Bool
     , -- Natively nothing is listening and nothing is going to happen by
       -- default, so these three have nothing to do there, and a handler
       -- written for the browser still runs against the in-memory DOM.
       Method "preventDefault" [t|IO ()|]
     , Method "stopPropagation" [t|IO ()|]
     , Method "stopImmediatePropagation" [t|IO ()|]
     ]
 )

$( jsFields
     ''EventTarget
     [ -- Dispatch an event to the target, whose listeners run before this
       -- returns; whether none of them prevented its default. For an event
       -- a program makes ('Web.Event.Event.newEvent'), for listeners of its own.
       Method "dispatchEvent" [t|Event -> IO Bool|]
     ]
 )
