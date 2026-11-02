{-# LANGUAGE TemplateHaskell #-}

-- | The window's cryptography, @window.crypto@, as far as programs here use
-- it. Meant to be imported qualified (@Crypto.randomUUID@).
module Web.Crypto
  ( Crypto (..)
  , randomUUID
  )
where

import Data.Foreign (Foreign)
import HPrelude
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsFieldFunctions, jsFields)

-- | A window's @Crypto@ object: @w.crypto@ ("Web.HTML.Window").
newtype Crypto = Crypto (Foreign Crypto)
  deriving newtype (Inert)

$( jsFields
     ''Crypto
     [ -- A random version 4 UUID, as text. The method exists only in a
       -- secure context (HTTPS, or a page on localhost); on a page served
       -- over plain HTTP the call throws, which on WebAssembly ends the
       -- program. 'Control.Monad.UUID.generateV4' works on every page.
       -- Natively, empty.
       Method "randomUUID" [t|IO Text|]
     ]
 )

$(jsFieldFunctions ''Crypto [])
