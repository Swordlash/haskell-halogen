{-# LANGUAGE TemplateHaskell #-}

-- | An event a page made itself, carrying a value: a @CustomEvent@. Meant to
-- be imported qualified (@CustomEvent.detail@).
module Web.Event.CustomEvent
  ( CustomEvent (..)
  , fromEvent
  , toEvent
  , detail
  )
where

import HPrelude
import Halogen.JSBits (Inert, JSVal)
import Halogen.JSBits.Fields (Member (..), jsCast, jsFieldFunctions, jsFields)
import Web.Event.Internal.Types (Event (..))

newtype CustomEvent = CustomEvent Event
  deriving newtype (Inert)

-- | Every CustomEvent is an 'Event', which is what the operations on events take.
toEvent :: CustomEvent -> Event
toEvent = coerce

$( jsFields
     ''CustomEvent
     [ -- The value the event was made with (@new CustomEvent(type, {
       -- detail })@), @null@ when it was made without one. Read it with
       -- "Data.Foreign".
       Fixed "detail" [t|JSVal|]
     ]
 )

$(jsFieldFunctions ''CustomEvent [])

$(jsCast "fromEvent" ''Event ''CustomEvent)
