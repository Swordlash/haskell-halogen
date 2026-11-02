{-# LANGUAGE TemplateHaskell #-}

-- | Keyboard events. The fields (@e.key@, @e.shiftKey@) are
-- 'KeyboardEvent''s instances, here; the same as functions are meant to be
-- used qualified (@KE.key@).
module Web.UIEvent.KeyboardEvent
  ( KeyboardEvent (..)
  , toEvent
  , fromEvent
  , key
  , code
  , altKey
  , ctrlKey
  , metaKey
  , shiftKey
  , isComposing
  )
where

import Data.Foreign (foreignToBool, foreignToString, readProp)
import HPrelude
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsCast, jsFieldFunctions, jsFields)
import Web.Event.Internal.Types

newtype KeyboardEvent = KeyboardEvent Event
  deriving newtype (Inert)

-- | Every KeyboardEvent is an 'Event', which is what the operations on events take.
toEvent :: KeyboardEvent -> Event
toEvent (KeyboardEvent e) = e

-- Natively an event made from JSON (a test's, or one a remote client sent)
-- carries its properties, and any other reads as empty or 'False'.
nativeText :: Text -> KeyboardEvent -> Text
nativeText name (KeyboardEvent (Event value)) = fromMaybe "" (readProp name (Just . foreignToString) value)

nativeBool :: Text -> KeyboardEvent -> Bool
nativeBool name (KeyboardEvent (Event value)) = fromMaybe False (readProp name (Just . foreignToBool) value)

-- Pure, because what an event says about its key is fixed when it is made.
$( jsFields
     ''KeyboardEvent
     [ -- What the key means, under the keyboard's layout and the modifiers
       -- held: @"a"@, @"A"@ with Shift, @"Enter"@, @"ArrowLeft"@, @"Dead"@
       -- while a composition is under way. This is what a shortcut or text
       -- entry wants.
       Natively [|nativeText "key"|] (Fixed "key" ''Text)
     , -- Which physical key it is, whatever the layout says it means:
       -- @"KeyA"@ is the key where a QWERTY keyboard has A, also on an
       -- AZERTY one, where it types Q. This is what a game's movement keys
       -- want.
       Natively [|nativeText "code"|] (Fixed "code" ''Text)
     , -- Whether each modifier was held.
       Natively [|nativeBool "altKey"|] (Fixed "altKey" ''Bool)
     , Natively [|nativeBool "ctrlKey"|] (Fixed "ctrlKey" ''Bool)
     , Natively [|nativeBool "metaKey"|] (Fixed "metaKey" ''Bool)
     , Natively [|nativeBool "shiftKey"|] (Fixed "shiftKey" ''Bool)
     , -- Whether the key is part of a composition under way (an input
       -- method's), whose Enter chooses a word rather than submitting.
       Natively [|nativeBool "isComposing"|] (Fixed "isComposing" ''Bool)
     ]
 )

$(jsFieldFunctions ''KeyboardEvent [])

$(jsCast "fromEvent" ''Event ''KeyboardEvent)
