module Web.UIEvent.FocusEvent where

import Web.Event.Internal.Types

newtype FocusEvent = FocusEvent Event

-- | Every FocusEvent is an 'Event', which is what the operations on events take.
toEvent :: FocusEvent -> Event
toEvent (FocusEvent e) = e
