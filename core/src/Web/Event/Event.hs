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

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Data.Foreign
#endif

import HPrelude
import Web.Event.Internal.Types

newtype EventType = EventType Text

currentTarget :: Event -> Maybe EventTarget

-- | Ask the browser not to take its default action for this event: not to
-- submit the form, not to follow the link, not to scroll the page.
--
-- Call it from the handler itself. By the time anything the handler forks or
-- schedules runs, the browser has already decided.
preventDefault :: (MonadIO m) => Event -> m ()

-- | Stop the event travelling further up the tree. Handlers already attached
-- to /this/ target still run; 'stopImmediatePropagation' stops those too.
stopPropagation :: (MonadIO m) => Event -> m ()

-- | Stop the event travelling further up the tree, and stop the other
-- handlers on this target from running.
stopImmediatePropagation :: (MonadIO m) => Event -> m ()

#if defined(javascript_HOST_ARCH)
foreign import javascript unsafe "js_current_target" js_current_target :: Event -> Nullable EventTarget
currentTarget e = EventTarget <$> nullableToMaybe (js_current_target e)

foreign import javascript unsafe "js_prevent_default" js_prevent_default :: Event -> IO ()
preventDefault = liftIO . js_prevent_default

foreign import javascript unsafe "js_stop_propagation" js_stop_propagation :: Event -> IO ()
stopPropagation = liftIO . js_stop_propagation

foreign import javascript unsafe "js_stop_immediate_propagation" js_stop_immediate_propagation :: Event -> IO ()
stopImmediatePropagation = liftIO . js_stop_immediate_propagation
#elif defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "$1.currentTarget" js_current_target :: Event -> Nullable EventTarget
currentTarget e = EventTarget <$> nullableToMaybe (js_current_target e)

foreign import javascript unsafe "$1.preventDefault()" js_prevent_default :: Event -> IO ()
preventDefault = liftIO . js_prevent_default

foreign import javascript unsafe "$1.stopPropagation()" js_stop_propagation :: Event -> IO ()
stopPropagation = liftIO . js_stop_propagation

foreign import javascript unsafe "$1.stopImmediatePropagation()" js_stop_immediate_propagation :: Event -> IO ()
stopImmediatePropagation = liftIO . js_stop_immediate_propagation
#else
currentTarget _ = panic "currentTarget: not available in GHC" -- TODO

-- Nothing is listening and nothing is going to happen by default, so these
-- three have nothing to prevent. They are no-ops rather than a `panic` so that
-- a handler written for the browser can still be run against the in-memory
-- DOM in "Halogen.VDom.DOM.Monad.Native".
preventDefault _ = pure ()
stopPropagation _ = pure ()
stopImmediatePropagation _ = pure ()
#endif
