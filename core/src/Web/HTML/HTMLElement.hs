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
  , blur
  , click
  )
where

import HPrelude
import Halogen.JSBits.Fields (jsCast, jsFieldFunctions)
import Web.DOM.Internal.Types (Element (..), HTMLElement (..))

-- | Every HTML element is an 'Element'.
toElement :: HTMLElement -> Element
toElement = coerce

$(jsFieldFunctions ''HTMLElement [])

$(jsCast "fromElement" ''Element ''HTMLElement)
