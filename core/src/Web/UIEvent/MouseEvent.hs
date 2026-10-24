{-# LANGUAGE TemplateHaskell #-}

module Web.UIEvent.MouseEvent
  ( MouseEvent (..)
  , toEvent
  , clientX
  , clientY
  )
where

import HPrelude
import Halogen.JSBits (Safety (..), browserJS)
import Web.Event.Internal.Types

newtype MouseEvent = MouseEvent Event

-- Pure, because where an event happened is fixed when it is made. Natively
-- no event happened anywhere, and both read as 0.
$( browserJS
     ["jsbits/web_event.js"]
     [ ("js_mouse_client_x", "js_mouse_client_x", Unsafe, [t|MouseEvent -> Double|])
     , ("js_mouse_client_y", "js_mouse_client_y", Unsafe, [t|MouseEvent -> Double|])
     ]
 )

-- | Every MouseEvent is an 'Event', which is what the operations on events take.
toEvent :: MouseEvent -> Event
toEvent (MouseEvent e) = e

-- | Where the pointer was, in CSS pixels from the left edge of the viewport
-- (not the page: scrolling does not change it). A 'Double', as the standard
-- has it, though a browser may round it to whole pixels.
clientX :: MouseEvent -> Double
clientX = js_mouse_client_x

-- | Where the pointer was, in CSS pixels from the top edge of the viewport.
clientY :: MouseEvent -> Double
clientY = js_mouse_client_y
