{-# LANGUAGE TemplateHaskell #-}

module Web.Event.Event
  ( EventType (..)
  , Event (..)
  , EventTarget (..)
  , currentTarget
  , preventDefault
  , stopPropagation
  , stopImmediatePropagation
  )
where

import Data.Foreign
import Halogen.JSBits (Safety (..), browserJS)
import HPrelude
import Web.Event.Internal.Types

newtype EventType = EventType Text

-- Natively nothing is listening and nothing is going to happen by default, so
-- the last three have nothing to prevent: they do nothing, so that a handler
-- written for the browser can still be run against the in-memory DOM in
-- "Halogen.VDom.DOM.Monad.Native".
$(browserJS ["jsbits/web_event.js"]
  [ ("js_current_target", "js_current_target", Unsafe, [t| Event -> Nullable EventTarget |])
  , ("js_prevent_default", "js_prevent_default", Unsafe, [t| Event -> IO () |])
  , ("js_stop_propagation", "js_stop_propagation", Unsafe, [t| Event -> IO () |])
  , ("js_stop_immediate_propagation", "js_stop_immediate_propagation", Unsafe, [t| Event -> IO () |])
  ])

-- | The target whose handler is running. Natively there is none.
currentTarget :: Event -> Maybe EventTarget
currentTarget e = EventTarget <$> nullableToMaybe (js_current_target e)

-- | Ask the browser not to take its default action for this event: not to
-- submit the form, not to follow the link, not to scroll the page.
--
-- Call it from the handler itself. By the time anything the handler forks or
-- schedules runs, the browser has already decided.
preventDefault :: (MonadIO m) => Event -> m ()
preventDefault = liftIO . js_prevent_default

-- | Stop the event travelling further up the tree. Handlers already attached
-- to /this/ target still run; 'stopImmediatePropagation' stops those too.
stopPropagation :: (MonadIO m) => Event -> m ()
stopPropagation = liftIO . js_stop_propagation

-- | Stop the event travelling further up the tree, and stop the other
-- handlers on this target from running.
stopImmediatePropagation :: (MonadIO m) => Event -> m ()
stopImmediatePropagation = liftIO . js_stop_immediate_propagation
