module Web.UIEvent.WheelEvent where

import Web.Event.Internal.Types

newtype WheelEvent = WheelEvent Event

-- | Every WheelEvent is an 'Event', which is what the operations on events take.
toEvent :: WheelEvent -> Event
toEvent (WheelEvent e) = e
