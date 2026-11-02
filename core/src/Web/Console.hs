{-# LANGUAGE TemplateHaskell #-}

-- | The browser's console, @window.console@, as far as programs here use
-- it. Meant to be imported qualified (@Console.warn@).
module Web.Console
  ( Console (..)
  , warn
  )
where

import Data.Foreign (Foreign)
import HPrelude
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsFieldFunctions, jsFields)

-- | A window's console: @w.console@ ("Web.HTML.Window").
newtype Console = Console (Foreign Console)
  deriving newtype (Inert)

$( jsFields
     ''Console
     [ -- A warning in the developer tools' console. Natively nothing.
       Method "warn" [t|Text -> IO ()|]
     ]
 )

$(jsFieldFunctions ''Console [])
