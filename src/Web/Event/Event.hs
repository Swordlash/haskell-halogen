module Web.Event.Event
  ( EventType (..)
  , Event (..)
  , EventTarget (..)
  )
where

import Protolude
import Web.Event.Internal.Types

newtype EventType = EventType Text
