module Web.UIEvent.MouseEvent where

import Web.Event.Internal.Types

newtype MouseEvent = MouseEvent Event

-- | Every MouseEvent is an 'Event', which is what the operations on events take.
toEvent :: MouseEvent -> Event
toEvent (MouseEvent e) = e
