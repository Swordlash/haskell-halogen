{-# LANGUAGE TemplateHaskell #-}

-- | An @\<input>@ element. Meant to be imported qualified
-- (@HTMLInputElement.value@).
module Web.HTML.HTMLInputElement
  ( HTMLInputElement (..)
  , fromElement
  , toElement
  , value
  , setValue
  , files
  )
where

import Data.Foreign (Foreign)
import HPrelude
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsCast, jsFieldFunctions, jsFields)
import Web.DOM.Internal.Types (Element (..))
import Web.File.FileList (FileList (..))

newtype HTMLInputElement = HTMLInputElement (Foreign HTMLInputElement)
  deriving newtype (Inert)

-- | Every input is an 'Element'.
toElement :: HTMLInputElement -> Element
toElement = coerce

$( jsFields
     ''HTMLInputElement
     [ -- What the input holds. Setting a file input's value to empty
       -- forgets the files chosen; any other value throws there.
       Writable "value" ''Text
     , -- The files chosen in a file input; 'Nothing' for any other input.
       Live "files" [t|Maybe FileList|]
     ]
 )

$(jsFieldFunctions ''HTMLInputElement [])

$(jsCast "fromElement" ''Element ''HTMLInputElement)
