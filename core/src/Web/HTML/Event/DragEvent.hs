module Web.HTML.Event.DragEvent where

import Web.Event.Internal.Types

newtype DragEvent = DragEvent Event

-- | Every DragEvent is an 'Event', which is what the operations on events take.
toEvent :: DragEvent -> Event
toEvent (DragEvent e) = e
