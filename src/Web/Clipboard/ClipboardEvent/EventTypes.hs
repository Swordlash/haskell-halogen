module Web.Clipboard.ClipboardEvent.EventTypes where

import Web.Event.Event (EventType (..))

clipboardchange :: EventType
clipboardchange = EventType "clipboardchange"

copy :: EventType
copy = EventType "copy"

cut :: EventType
cut = EventType "cut"

paste :: EventType
paste = EventType "paste"
