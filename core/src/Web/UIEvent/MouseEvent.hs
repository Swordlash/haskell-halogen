{-# LANGUAGE TemplateHaskell #-}

module Web.UIEvent.MouseEvent
  ( MouseEvent (..)
  , toEvent
  , clientX
  , clientY
  )
where

import Data.Foreign (foreignToNumber, readProp)
import HPrelude
import Halogen.JSBits (Safety (..), browserJS, inBrowser)
import Web.Event.Internal.Types

newtype MouseEvent = MouseEvent Event

-- Pure, because where an event happened is fixed when it is made. Natively
-- an event made from JSON (a test's, or one a remote client sent) carries
-- its @clientX@ and @clientY@ properties, and any other reads as 0.
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
clientX e
  | inBrowser = js_mouse_client_x e
  | otherwise = nativeProperty "clientX" e

-- | Where the pointer was, in CSS pixels from the top edge of the viewport.
clientY :: MouseEvent -> Double
clientY e
  | inBrowser = js_mouse_client_y e
  | otherwise = nativeProperty "clientY" e

nativeProperty :: Text -> MouseEvent -> Double
nativeProperty name (MouseEvent (Event value)) = fromMaybe 0 (readProp name (Just . foreignToNumber) value)
