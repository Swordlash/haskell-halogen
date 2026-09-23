module Web.UIEvent.PointerEvent where

import Web.Event.Internal.Types

newtype PointerEvent = PointerEvent Event

-- | Every PointerEvent is an 'Event', which is what the operations on events take.
toEvent :: PointerEvent -> Event
toEvent (PointerEvent e) = e
