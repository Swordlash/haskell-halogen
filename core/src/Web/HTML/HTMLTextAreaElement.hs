{-# LANGUAGE TemplateHaskell #-}

-- | A @\<textarea>@ element. Meant to be imported qualified
-- (@HTMLTextAreaElement.value@).
module Web.HTML.HTMLTextAreaElement
  ( HTMLTextAreaElement (..)
  , fromElement
  , toElement
  , value
  , setValue
  , selectionStart
  , setSelectionStart
  , selectionEnd
  , setSelectionEnd
  )
where

import Data.Foreign (Foreign)
import HPrelude
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsCast, jsFieldFunctions, jsFields)
import Web.DOM.Internal.Types (Element (..))

newtype HTMLTextAreaElement = HTMLTextAreaElement (Foreign HTMLTextAreaElement)
  deriving newtype (Inert)

-- | Every textarea is an 'Element'.
toElement :: HTMLTextAreaElement -> Element
toElement = coerce

$( jsFields
     ''HTMLTextAreaElement
     [ -- The text in it, its line endings made @"\\n"@ by the browser.
       Writable "value" ''Text
     , -- Where the selection starts and ends, in UTF-16 code units from
       -- the start of the text (two for a character beyond the Basic
       -- Multilingual Plane); the two are equal for a cursor. Setting one
       -- moves the selection.
       Writable "selectionStart" ''Int
     , Writable "selectionEnd" ''Int
     ]
 )

$(jsFieldFunctions ''HTMLTextAreaElement [])

$(jsCast "fromElement" ''Element ''HTMLTextAreaElement)
