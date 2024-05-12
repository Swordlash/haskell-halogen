module Web.Event.Internal.Types where

import Data.Foreign (Foreign)

newtype Event = Event (Foreign Event)
newtype EventTarget = EventTarget (Foreign EventTarget)