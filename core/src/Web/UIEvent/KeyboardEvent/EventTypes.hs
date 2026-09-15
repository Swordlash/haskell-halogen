module Web.UIEvent.KeyboardEvent.EventTypes where

import Web.Event.Event (EventType (..))

keydown :: EventType
keydown = EventType "keydown"

keyup :: EventType
keyup = EventType "keyup"
