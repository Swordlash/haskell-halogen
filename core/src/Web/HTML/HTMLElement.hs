{-# LANGUAGE TemplateHaskell #-}

-- | An HTML element, as functions. Meant to be imported qualified
-- (@HTMLElement.focus@). The fields are 'HTMLElement''s own instances, in
-- "Web.DOM.Internal.Types".
--
-- This module's 'fromElement' asks the browser whether the element is an
-- HTML one (an SVG element is not); "Web.DOM.Internal.Types"'
-- 'Web.DOM.Internal.Types.fromElement' does not ask, and always gives
-- 'Just'.
module Web.HTML.HTMLElement
  ( HTMLElement (..)
  , fromElement
  , toElement
  , focus
  , focusWithoutScrolling
  , blur
  , click
  )
where

import Data.Aeson qualified as Aeson
import Data.Foreign (toForeign)
import HPrelude
import Halogen.JSBits (inBrowser, nativeCall)
import Halogen.JSBits.Fields (inlineJS, jsCast, jsFieldFunctions)
import Web.DOM.Internal.Types (Element (..), HTMLElement (..))

$(inlineJS "js_focus_without_scrolling" "$1.focus({ preventScroll: true })" [t|HTMLElement -> IO ()|])

-- | Give the element the focus, as 'focus' does, without scrolling it into
-- view: for a control the page shows where it is, such as the text of an
-- editor coming back from its preview.
--
-- Natively it is @focus@ with its options, @{preventScroll: true}@ (as
-- JSON), for an element that answers for itself
-- ('Halogen.JSBits.Value.answering': a remote client's page focuses it so).
focusWithoutScrolling :: HTMLElement -> IO ()
focusWithoutScrolling element
  | inBrowser = js_focus_without_scrolling element
  | otherwise = nativeCall (coerce element) "focus" [toForeign (Aeson.object ["preventScroll" Aeson..= True])]

-- | Every HTML element is an 'Element'.
toElement :: HTMLElement -> Element
toElement = coerce

$(jsFieldFunctions ''HTMLElement [])

$(jsCast "fromElement" ''Element ''HTMLElement)
