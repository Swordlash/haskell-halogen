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
--
-- Natively an element of the in-memory DOM ("Halogen.VDom.DOM.Monad.Native")
-- answers from its tree: an event it dispatches has the element as its
-- target, its fields ('isConnected', 'contains', 'getAttribute', 'closest',
-- 'querySelector', 'textContent') read the tree, and so do 'closestNow',
-- 'getAttributeNow' and 'fromEventTarget'. On a page where the event was
-- described rather than dispatched (haskell-halogen-remote's, and a test's
-- event made from JSON), an element is what the event's JSON says of it:
-- an object with a @tagName@ is an element ('fromEventTarget'), its
-- @closest@ holds the elements found for the selectors a handler's match
-- named ('closestNow'), and its @attributes@ its attributes
-- ('getAttributeNow'). "Halogen.Remote.Protocol" spells what a remote
-- client sends.
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

import Data.Aeson qualified as Aeson
import Data.Foreign (foreignToString, nativeInvokeNow, readProp, toForeign)
import HPrelude
import Halogen.JSBits (JSText, JSVal, inBrowser, isNull, jsValText, nativeCall, nativeInstanceOf, textValue, toJSText)
import Halogen.JSBits.Fields (inlineJS, jsFieldFunctions)
import Web.DOM.Internal.Types (Element (..))
import Web.Event.Internal.Types (EventTarget (..))

$(jsFieldFunctions ''Element [])

-- Neither read throws: a selector that does not parse finds nothing, since
-- a SyntaxError out of a synchronous import would end the program on
-- WebAssembly. An element a remote client described is a plain object: its
-- closest and attributes are objects keyed by selector and by name.
$( inlineJS
     "js_closest_now"
     "((e, s) => { if (typeof e.closest === 'function') { try { return e.closest(s) ?? null; } catch { return null; } } const c = e.closest; return c !== null && typeof c === 'object' && Object.prototype.hasOwnProperty.call(c, s) && c[s] !== null && typeof c[s] === 'object' ? c[s] : null; })($1, $2)"
     [t|Element -> JSText -> JSVal|]
 )

$( inlineJS
     "js_attribute_now"
     "((e, n) => { if (typeof e.getAttribute === 'function') return e.getAttribute(n) ?? null; const a = e.attributes; return a !== null && typeof a === 'object' && Object.prototype.hasOwnProperty.call(a, n) && typeof a[n] === 'string' ? a[n] : null; })($1, $2)"
     [t|Element -> JSText -> JSVal|]
 )

-- | 'closest', read purely: the element itself or its nearest ancestor that
-- a CSS selector matches, as the tree is when the answer is forced. For a
-- handler that decides in the event's listener (see the module's
-- description). A selector that does not parse finds nothing.
--
-- Natively, an element of the in-memory DOM finds it in its tree, with the
-- selectors it understands, and throws for one it does not, rather than
-- find nothing (see "Halogen.VDom.DOM.Monad.Native"'s @querySelector@).
-- An element described in an event's JSON is the one the JSON says was
-- found for the selector: @"closest": {"a[href]": {...}}@ on the element,
-- which a remote client sends on the event's target for each selector the
-- listener's match names ('Halogen.HTML.Events.within'); 'Nothing' for a
-- selector it does not have. There the selector is a key.
closestNow :: Element -> Text -> Maybe Element
closestNow element selector
  | inBrowser =
      let found = js_closest_now element (toJSText selector)
       in if isNull found then Nothing else Just (coerce found)
  | Just found <- nativeInvokeNow value "closest" [textValue selector] = if isNull found then Nothing else Just (coerce found)
  | otherwise = coerce <$> readProp "closest" (readProp selector Just) value
  where
    value = coerce element :: JSVal

-- | 'getAttribute', read purely: an attribute's value as written, as the
-- element is when the answer is forced, for a handler that decides in the
-- event's listener. Natively, an element of the in-memory DOM's attribute
-- (see "Halogen.VDom.DOM.Monad.Native"'s @attribute@), or the value under
-- the element's @"attributes"@ in the event's JSON (a remote client sends
-- all of an element it found for a 'Halogen.HTML.Events.within'), and
-- 'Nothing' for one it does not have.
getAttributeNow :: Element -> Text -> Maybe Text
getAttributeNow element name
  | inBrowser =
      let value = js_attribute_now element (toJSText name)
       in if isNull value then Nothing else Just (jsValText value)
  | Just found <- nativeInvokeNow held "getAttribute" [textValue name] = if isNull found then Nothing else Just (jsValText found)
  | otherwise = readProp "attributes" (readProp name (Just . foreignToString)) held
  where
    held = coerce element :: JSVal

-- | Where 'scrollIntoViewBlock' puts the element in the viewport: at its
-- top, in the middle, at its bottom, or wherever is nearest to where the
-- element is (not scrolling at all if it is in view).
data ScrollBlock = ToStart | ToCenter | ToEnd | ToNearest
  deriving stock (Eq, Show)

$(inlineJS "js_scroll_into_view_block" "$1.scrollIntoView({ block: $2 })" [t|Element -> JSText -> IO ()|])

-- | 'scrollIntoView', saying where in the viewport the element goes.
-- Natively it is @scrollIntoView@ with its options, @{block: ...}@ (as
-- JSON), for an element that answers for itself
-- ('Halogen.JSBits.Value.withNativeObject').
scrollIntoViewBlock :: Element -> ScrollBlock -> IO ()
scrollIntoViewBlock element block
  | inBrowser = js_scroll_into_view_block element (toJSText name)
  | otherwise = nativeCall (coerce element) "scrollIntoView" [toForeign (Aeson.object ["block" Aeson..= name])]
  where
    name = case block of
      ToStart -> "start"
      ToCenter -> "center"
      ToEnd -> "end"
      ToNearest -> "nearest"

-- | Every element is an 'EventTarget'.
toEventTarget :: Element -> EventTarget
toEventTarget = coerce

$( inlineJS
     "js_is_element"
     "((typeof Element === 'function' && $1 instanceof Element) || ($1 !== null && typeof $1 === 'object' && typeof $1.tagName === 'string'))"
     [t|EventTarget -> Bool|]
 )

-- | The target as an 'Element', if it is one (an event's target can be a
-- text node, the document or the window). An element is an instance of
-- @Element@, or an object with a @tagName@ (one a remote client described,
-- haskell-halogen-remote's loopback).
--
-- Natively, a value that answers for itself
-- ('Halogen.JSBits.Value.withNativeObject', a node of the in-memory DOM) is
-- asked whether it is an @Element@, and an object of JSON (an event's
-- @target@, as a test or a remote client makes it) is one when it has a
-- @tagName@.
fromEventTarget :: EventTarget -> Maybe Element
fromEventTarget target
  | inBrowser = if js_is_element target then Just (coerce target) else Nothing
  | nativeInstanceOf value "Element" || isJust (readProp "tagName" (Just . foreignToString) value) = Just (coerce target)
  | otherwise = Nothing
  where
    value = coerce target :: JSVal
