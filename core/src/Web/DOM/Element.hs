{-# LANGUAGE TemplateHaskell #-}

-- | An element, as functions. Meant to be imported qualified
-- (@Element.closest@). The fields themselves (@el.getAttribute@) are
-- 'Element''s instances, in "Web.DOM.Internal.Types".
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
  )
where

import HPrelude
import Halogen.JSBits.Fields (jsCast, jsFieldFunctions)
import Web.DOM.Internal.Types (Element (..))
import Web.Event.Internal.Types (EventTarget (..))

$(jsFieldFunctions ''Element [])

-- | Every element is an 'EventTarget'.
toEventTarget :: Element -> EventTarget
toEventTarget = coerce

$(jsCast "fromEventTarget" ''EventTarget ''Element)
