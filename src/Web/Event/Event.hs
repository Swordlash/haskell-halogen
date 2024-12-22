module Web.Event.Event
  ( EventType (..)
  , Event (..)
  , EventTarget (..)
  )
where

import HPrelude
import Web.Event.Internal.Types

newtype EventType = EventType Text
