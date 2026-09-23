module Web.UIEvent.TouchEvent where

import Web.Event.Internal.Types

newtype TouchEvent = TouchEvent Event

-- | Every TouchEvent is an 'Event', which is what the operations on events take.
toEvent :: TouchEvent -> Event
toEvent (TouchEvent e) = e
