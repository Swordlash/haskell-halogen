module Web.UIEvent.FocusEvent.EventTypes where

import Web.Event.Event (EventType (..))

blur :: EventType
blur = EventType "blur"

focus :: EventType
focus = EventType "focus"

focusin :: EventType
focusin = EventType "focusin"

focusout :: EventType
focusout = EventType "focusout"
