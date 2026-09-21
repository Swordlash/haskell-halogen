module Web.Clipboard.ClipboardEvent where

import Web.Event.Internal.Types

newtype ClipboardEvent = ClipboardEvent Event

-- | Every ClipboardEvent is an 'Event', which is what the operations on events take.
toEvent :: ClipboardEvent -> Event
toEvent (ClipboardEvent e) = e
