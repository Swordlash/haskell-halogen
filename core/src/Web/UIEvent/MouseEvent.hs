{-# LANGUAGE TemplateHaskell #-}

-- | Mouse events. The fields (@e.clientX@, @e.button@) are 'MouseEvent''s
-- instances, here; the same as functions are meant to be used qualified
-- (@ME.clientX@).
module Web.UIEvent.MouseEvent
  ( MouseEvent (..)
  , toEvent
  , fromEvent
  , clientX
  , clientY
  , button
  , altKey
  , ctrlKey
  , metaKey
  , shiftKey
  )
where

import Data.Foreign (Foreign, foreignToBool, foreignToInt, foreignToNumber, readProp)
import HPrelude
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsCast, jsFieldFunctions, jsFields)
import Web.Event.Internal.Types

newtype MouseEvent = MouseEvent Event
  deriving newtype (Inert)

-- | Every MouseEvent is an 'Event', which is what the operations on events take.
toEvent :: MouseEvent -> Event
toEvent (MouseEvent e) = e

-- Natively an event made from JSON (a test's, or one a remote client sent)
-- carries its properties, and any other reads as 0 or 'False'.
nativeProperty :: Text -> (Foreign Event -> a) -> a -> MouseEvent -> a
nativeProperty name readValue none (MouseEvent (Event value)) = fromMaybe none (readProp name (Just . readValue) value)

-- Pure, because where and how an event happened is fixed when it is made.
$( jsFields
     ''MouseEvent
     [ -- Where the pointer was, in CSS pixels from the left edge of the
       -- viewport (not the page: scrolling does not change it). A 'Double',
       -- as the standard has it, though a browser may round it to whole
       -- pixels.
       Natively [|nativeProperty "clientX" foreignToNumber 0|] (Fixed "clientX" [t|Double|])
     , -- Where the pointer was, in CSS pixels from the top edge of the
       -- viewport.
       Natively [|nativeProperty "clientY" foreignToNumber 0|] (Fixed "clientY" [t|Double|])
     , -- Which button changed: 0 the main one (usually the left), 1 the
       -- middle, 2 the secondary.
       Natively [|nativeProperty "button" foreignToInt 0|] (Fixed "button" [t|Int|])
     , -- Whether each modifier was held.
       Natively [|nativeProperty "altKey" foreignToBool False|] (Fixed "altKey" [t|Bool|])
     , Natively [|nativeProperty "ctrlKey" foreignToBool False|] (Fixed "ctrlKey" [t|Bool|])
     , Natively [|nativeProperty "metaKey" foreignToBool False|] (Fixed "metaKey" [t|Bool|])
     , Natively [|nativeProperty "shiftKey" foreignToBool False|] (Fixed "shiftKey" [t|Bool|])
     ]
 )

$(jsFieldFunctions ''MouseEvent [])

$(jsCast "fromEvent" ''Event ''MouseEvent)
