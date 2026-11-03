{-# LANGUAGE TemplateHaskell #-}

-- | An element, as functions. Meant to be imported qualified
-- (@Element.closest@). The fields themselves (@el.getAttribute@) are
-- 'Element''s instances, in "Web.DOM.Internal.Types".
--
-- Two reads are pure, 'closestNow' and 'getAttributeNow', for a handler
-- that decides in the event's listener: 'Halogen.HTML.Events.handler''
-- gives it a pure function of the event, which runs while the browser
-- dispatches the event, when what it reads is the page as of the event and
-- the browser can still be kept from its default action
-- ('Halogen.HTML.Events.preventingDefault'). They read the tree as it is
-- when the value is forced, so force it there (@Just $! …@).
module Web.DOM.Element
  ( Element
  , fromEventTarget
  , toEventTarget
  , textContent
  , setTextContent
  , getAttribute
  , hasAttribute
  , closest
  , querySelector
  , scrollIntoView
  , ScrollBlock (..)
  , scrollIntoViewBlock
  , isConnected
  , nextElementSibling
  , setAttribute
  , contains
  , appendChild
  , requestFullscreen
  , remove
  , closestNow
  , getAttributeNow
  )
where

import HPrelude
import Halogen.JSBits (JSText, JSVal, isNull, jsValText, toJSText)
import Halogen.JSBits.Fields (inlineJS, jsCast, jsFieldFunctions)
import Web.DOM.Internal.Types (Element (..))
import Web.Event.Internal.Types (EventTarget (..))

$(jsFieldFunctions ''Element [])

-- Neither read is guarded: a selector that does not parse throws a
-- SyntaxError, which ends the program on WebAssembly, so the selector must
-- be one that parses, as a literal is.
$(inlineJS "js_closest_now" "($1.closest($2) ?? null)" [t|Element -> JSText -> JSVal|])

$(inlineJS "js_attribute_now" "($1.getAttribute($2) ?? null)" [t|Element -> JSText -> JSVal|])

-- | 'closest', read purely: the element itself or its nearest ancestor that
-- a CSS selector matches, as the tree is when the answer is forced. For a
-- handler that decides in the event's listener (see the module's
-- description). The selector must parse: a literal. Natively 'Nothing'.
closestNow :: Element -> Text -> Maybe Element
closestNow element selector =
  let found = js_closest_now element (toJSText selector)
   in if isNull found then Nothing else Just (coerce found)

-- | 'getAttribute', read purely: an attribute's value as written, as the
-- element is when the answer is forced, for a handler that decides in the
-- event's listener. Natively 'Nothing'.
getAttributeNow :: Element -> Text -> Maybe Text
getAttributeNow element name =
  let value = js_attribute_now element (toJSText name)
   in if isNull value then Nothing else Just (jsValText value)

-- | Where 'scrollIntoViewBlock' puts the element in the viewport: at its
-- top, in the middle, at its bottom, or wherever is nearest to where the
-- element is (not scrolling at all if it is in view).
data ScrollBlock = ToStart | ToCenter | ToEnd | ToNearest
  deriving stock (Eq, Show)

$(inlineJS "js_scroll_into_view_block" "$1.scrollIntoView({ block: $2 })" [t|Element -> JSText -> IO ()|])

-- | 'scrollIntoView', saying where in the viewport the element goes.
scrollIntoViewBlock :: Element -> ScrollBlock -> IO ()
scrollIntoViewBlock element block = js_scroll_into_view_block element (toJSText name)
  where
    name = case block of
      ToStart -> "start"
      ToCenter -> "center"
      ToEnd -> "end"
      ToNearest -> "nearest"

-- | Every element is an 'EventTarget'.
toEventTarget :: Element -> EventTarget
toEventTarget = coerce

$(jsCast "fromEventTarget" ''EventTarget ''Element)
