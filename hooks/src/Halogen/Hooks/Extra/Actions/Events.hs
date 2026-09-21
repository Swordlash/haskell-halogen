-- | The other half of an event handler: telling the browser not to do what it
-- was going to do.
--
-- A handler that submits a form, or acts on a key, almost always has to stop
-- the browser's own handling of the same event first — otherwise the page
-- reloads, or the key does two things. These wrap the operations in
-- "Web.Event.Event" so that a handler stays one expression.
--
-- Call them from the handler itself rather than from anything it forks: by the
-- time a forked program runs, the browser has already decided.
module Halogen.Hooks.Extra.Actions.Events
  ( preventDefault'
  , preventDefault
  , preventMouseEvent
  , preventKeyEvent
  , stopPropagation'
  , stopPropagation
  )
where

import Halogen.Hooks.HookM (HookM)
import Protolude
import Web.Event.Event (Event)
import Web.Event.Event qualified as EE
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent qualified as KE
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent qualified as ME

-- | Prevent the default action of an 'Event'.
preventDefault' :: forall slots output m. (MonadIO m) => Event -> HookM slots output m ()
preventDefault' = EE.preventDefault

-- | Prevent the default action of anything that is an 'Event' underneath, such
-- as a 'MouseEvent'. Takes the @toEvent@ of the event's own module.
preventDefault :: forall e slots output m. (MonadIO m) => (e -> Event) -> e -> HookM slots output m ()
preventDefault toEvent = preventDefault' . toEvent

-- | 'preventDefault' for a 'MouseEvent'.
preventMouseEvent :: forall slots output m. (MonadIO m) => MouseEvent -> HookM slots output m ()
preventMouseEvent = preventDefault ME.toEvent

-- | 'preventDefault' for a 'KeyboardEvent'.
preventKeyEvent :: forall slots output m. (MonadIO m) => KeyboardEvent -> HookM slots output m ()
preventKeyEvent = preventDefault KE.toEvent

-- | Stop an 'Event' travelling further up the tree.
stopPropagation' :: forall slots output m. (MonadIO m) => Event -> HookM slots output m ()
stopPropagation' = EE.stopPropagation

-- | 'stopPropagation'' for anything that is an 'Event' underneath.
stopPropagation :: forall e slots output m. (MonadIO m) => (e -> Event) -> e -> HookM slots output m ()
stopPropagation toEvent = stopPropagation' . toEvent
