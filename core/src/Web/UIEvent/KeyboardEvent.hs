{-# LANGUAGE TemplateHaskell #-}

module Web.UIEvent.KeyboardEvent
  ( KeyboardEvent (..)
  , toEvent
  , key
  , code
  )
where

import HPrelude
import Halogen.JSBits (JSText, Safety (..), browserJS, fromJSText)
import Web.Event.Internal.Types

newtype KeyboardEvent = KeyboardEvent Event

-- Pure, because what an event says about its key is fixed when it is made.
-- Natively no event carries a key, and both read as empty.
$( browserJS
     ["jsbits/web_event.js"]
     [ ("js_keyboard_key", "js_keyboard_key", Unsafe, [t|KeyboardEvent -> JSText|])
     , ("js_keyboard_code", "js_keyboard_code", Unsafe, [t|KeyboardEvent -> JSText|])
     ]
 )

-- | Every KeyboardEvent is an 'Event', which is what the operations on events take.
toEvent :: KeyboardEvent -> Event
toEvent (KeyboardEvent e) = e

-- | What the key means, under the keyboard's layout and the modifiers held:
-- @"a"@, @"A"@ with Shift, @"Enter"@, @"ArrowLeft"@, @"Dead"@ while a
-- composition is under way. This is what a shortcut or text entry wants.
key :: KeyboardEvent -> Text
key = fromJSText . js_keyboard_key

-- | Which physical key it is, whatever the layout says it means: @"KeyA"@ is
-- the key where a QWERTY keyboard has A, also on an AZERTY one, where it
-- types Q. This is what a game's movement keys want.
code :: KeyboardEvent -> Text
code = fromJSText . js_keyboard_code
