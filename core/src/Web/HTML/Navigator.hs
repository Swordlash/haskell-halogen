{-# LANGUAGE TemplateHaskell #-}

-- | The navigator, @window.navigator@: what the browser says about itself
-- and the device. Meant to be imported qualified (@Navigator.onLine@).
module Web.HTML.Navigator
  ( Navigator (..)
  , onLine
  , clipboard
  )
where

import Data.Foreign (Foreign)
import HPrelude
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsFieldFunctions, jsFields)
import Web.Clipboard (Clipboard (..))

-- | A window's navigator: @w.navigator@ ("Web.HTML.Window").
newtype Navigator = Navigator (Foreign Navigator)
  deriving newtype (Inert)

$( jsFields
     ''Navigator
     [ -- Whether the browser thinks it is online. 'False' means certainly
       -- offline; 'True' only that some network is there. The window's
       -- @online@ and @offline@ events tell when it changes.
       Live "onLine" [t|IO Bool|]
     , -- The system clipboard, which only a secure context (HTTPS, or a
       -- page on localhost) has.
       Fixed "clipboard" [t|Maybe Clipboard|]
     ]
 )

$(jsFieldFunctions ''Navigator [])
