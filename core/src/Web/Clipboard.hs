{-# LANGUAGE TemplateHaskell #-}

-- | The system clipboard, @navigator.clipboard@, as far as programs here use
-- it. Meant to be imported qualified (@Clipboard.writeText@).
module Web.Clipboard
  ( Clipboard (..)
  , writeText
  )
where

import Data.Foreign (Foreign)
import HPrelude
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsFieldFunctions, jsFields)
import Halogen.JSBits.Promise (Promise (..))

-- | The navigator's @Clipboard@: @nav.clipboard@ ("Web.HTML.Navigator"),
-- which only a secure context has.
newtype Clipboard = Clipboard (Foreign Clipboard)
  deriving newtype (Inert)

$( jsFields
     ''Clipboard
     [ -- Put text on the clipboard, and wait until it is there. The browser
       -- may refuse (the page does not have the focus, or the reader has
       -- not allowed it), and the refusal is thrown as a
       -- 'Halogen.JSBits.Promise.JSError' when the promise is awaited.
       -- Natively it does nothing.
       Method "writeText" [t|Text -> IO (Promise ())|]
     ]
 )

$(jsFieldFunctions ''Clipboard [])
