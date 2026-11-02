{-# LANGUAGE TemplateHaskell #-}

-- | A @\<dialog>@ element. Meant to be imported qualified
-- (@HTMLDialogElement.showModal@).
module Web.HTML.HTMLDialogElement
  ( HTMLDialogElement (..)
  , fromElement
  , toElement
  , open
  , show
  , showModal
  , close
  )
where

import Data.Foreign (Foreign)
import HPrelude hiding (show)
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsCast, jsFieldFunctions, jsFields)
import Web.DOM.Internal.Types (Element (..))

newtype HTMLDialogElement = HTMLDialogElement (Foreign HTMLDialogElement)
  deriving newtype (Inert)

-- | Every dialog is an 'Element'.
toElement :: HTMLDialogElement -> Element
toElement = coerce

$( jsFields
     ''HTMLDialogElement
     [ -- Whether the dialog is shown.
       Live "open" [t|IO Bool|]
     , -- Show it in place, without making the rest of the page inert.
       Method "show" [t|IO ()|]
     , -- Show it as a modal: above the page, with a backdrop, the rest of
       -- the page inert, closed by Escape, and the focus given back to
       -- where it was when it closes. The browser throws for a dialog that
       -- is already open as a non-modal one, or is not in the document,
       -- which on WebAssembly ends the program: ask 'open' first.
       Method "showModal" [t|IO ()|]
     , -- Close it, if it is open.
       Method "close" [t|IO ()|]
     ]
 )

$(jsFieldFunctions ''HTMLDialogElement [])

$(jsCast "fromElement" ''Element ''HTMLDialogElement)
