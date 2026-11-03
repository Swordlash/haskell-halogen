{-# LANGUAGE TemplateHaskell #-}

-- | Events, and an event's fields as functions. The fields themselves
-- (@e.target@, @e.defaultPrevented@) are 'Event''s instances, in
-- "Web.Event.Internal.Types".
module Web.Event.Event
  ( EventType (..)
  , Event (..)
  , EventTarget (..)
  , type_
  , target
  , currentTarget
  , defaultPrevented
  , preventDefault
  , stopPropagation
  , stopImmediatePropagation
  , newEvent
  , dispatchEvent
  , defaultPreventedNow
  )
where

import HPrelude
import Halogen.JSBits (JSText, toJSText)
import Halogen.JSBits.Fields (inlineJS, jsFieldFunctions)
import Web.Event.Internal.Types

newtype EventType = EventType Text

$(jsFieldFunctions ''Event ["preventDefault", "stopPropagation", "stopImmediatePropagation"])

$(jsFieldFunctions ''EventTarget [])

$(inlineJS "js_new_event" "new Event($1, { bubbles: $2 })" [t|JSText -> Bool -> IO Event|])

$(inlineJS "js_default_prevented_now" "$1.defaultPrevented" [t|Event -> Bool|])

-- | 'defaultPrevented', read purely, as the event is when the answer is
-- forced: for a handler that decides in the event's listener
-- ('Halogen.HTML.Events.handler'' gives it a pure function of the event),
-- which leaves an event an earlier listener has handled alone. Natively
-- 'False'.
defaultPreventedNow :: Event -> Bool
defaultPreventedNow = js_default_prevented_now

-- | A new event of a type, for 'dispatchEvent' to send to a target: one a
-- program makes for listeners of its own, saying whether it bubbles up
-- from the target through the elements around it. Natively a placeholder.
newEvent :: EventType -> Bool -> IO Event
newEvent (EventType eventType) bubbles = js_new_event (toJSText eventType) bubbles

-- | Ask the browser not to take its default action for this event: not to
-- submit the form, not to follow the link, not to scroll the page.
--
-- Call it from the handler itself. By the time anything the handler forks or
-- schedules runs, the browser has already decided.
preventDefault :: (MonadIO m) => Event -> m ()
preventDefault e = liftIO e.preventDefault

-- | Stop the event travelling further up the tree. Handlers already attached
-- to /this/ target still run; 'stopImmediatePropagation' stops those too.
stopPropagation :: (MonadIO m) => Event -> m ()
stopPropagation e = liftIO e.stopPropagation

-- | Stop the event travelling further up the tree, and stop the other
-- handlers on this target from running.
stopImmediatePropagation :: (MonadIO m) => Event -> m ()
stopImmediatePropagation e = liftIO e.stopImmediatePropagation
