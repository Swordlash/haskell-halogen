{-# LANGUAGE TemplateHaskell #-}

-- | Copy, cut and paste events. The fields (@e.clipboardData@) are
-- 'ClipboardEvent''s instances, here; the same as functions are meant to
-- be used qualified.
module Web.Clipboard.ClipboardEvent
  ( ClipboardEvent (..)
  , toEvent
  , fromEvent
  , clipboardData
  )
where

import HPrelude
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsCast, jsFieldFunctions, jsFields)
import Web.Event.Internal.Types
import Web.HTML.Event.DataTransfer (DataTransfer (..))

newtype ClipboardEvent = ClipboardEvent Event
  deriving newtype (Inert)

-- | Every ClipboardEvent is an 'Event', which is what the operations on events take.
toEvent :: ClipboardEvent -> Event
toEvent (ClipboardEvent e) = e

$( jsFields
     ''ClipboardEvent
     [ -- What is pasted, or what a copy puts on the clipboard: the files of
       -- a paste are its 'Web.HTML.Event.DataTransfer.files'. 'Nothing' for
       -- a clipboard event a page made without one.
       Fixed "clipboardData" [t|Maybe DataTransfer|]
     ]
 )

$(jsFieldFunctions ''ClipboardEvent [])

$(jsCast "fromEvent" ''Event ''ClipboardEvent)
