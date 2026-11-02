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
  )
where

import HPrelude
import Halogen.JSBits.Fields (jsFieldFunctions)
import Web.Event.Internal.Types

newtype EventType = EventType Text

$(jsFieldFunctions ''Event ["preventDefault", "stopPropagation", "stopImmediatePropagation"])

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
