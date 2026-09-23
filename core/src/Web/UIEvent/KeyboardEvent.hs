module Web.UIEvent.KeyboardEvent where

import Web.Event.Internal.Types

newtype KeyboardEvent = KeyboardEvent Event

-- | Every KeyboardEvent is an 'Event', which is what the operations on events take.
toEvent :: KeyboardEvent -> Event
toEvent (KeyboardEvent e) = e
