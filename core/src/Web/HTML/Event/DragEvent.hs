{-# LANGUAGE TemplateHaskell #-}

-- | Drag-and-drop events. The fields (@e.dataTransfer@) are 'DragEvent''s
-- instances, here; the same as functions are meant to be used qualified.
module Web.HTML.Event.DragEvent
  ( DragEvent (..)
  , toEvent
  , fromEvent
  , dataTransfer
  )
where

import Data.Foreign (readProp)
import HPrelude
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsCast, jsFieldFunctions, jsFields)
import Web.Event.Internal.Types
import Web.HTML.Event.DataTransfer (DataTransfer (..))

newtype DragEvent = DragEvent Event
  deriving newtype (Inert)

-- | Every DragEvent is an 'Event', which is what the operations on events take.
toEvent :: DragEvent -> Event
toEvent (DragEvent e) = e

$( jsFields
     ''DragEvent
     [ -- What is being dragged, or dropped: the files of a drop are its
       -- 'Web.HTML.Event.DataTransfer.files'. 'Nothing' for a drag event a
       -- page made without one.
       --
       -- Natively, the event's JSON's @dataTransfer@ ("Web.HTML.Event.DataTransfer"
       -- reads it).
       Natively
         [|\(DragEvent (Event value)) -> DataTransfer <$> readProp "dataTransfer" Just value|]
         (Fixed "dataTransfer" [t|Maybe DataTransfer|])
     ]
 )

$(jsFieldFunctions ''DragEvent [])

$(jsCast "fromEvent" ''Event ''DragEvent)
