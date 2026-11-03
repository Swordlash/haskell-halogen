{-# LANGUAGE TemplateHaskell #-}

-- | The caches a service worker keeps, @window.caches@: the @CacheStorage@,
-- as far as programs here use it. Meant to be imported qualified
-- (@CacheStorage.keys@).
module Web.Cache
  ( CacheStorage (..)
  , keys
  , delete
  )
where

import Data.Foreign (Foreign)
import HPrelude
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsFieldFunctions, jsFields)
import Halogen.JSBits.Promise (Promise (..))

-- | A window's caches: @w.caches@ ("Web.HTML.Window"), which only a secure
-- context (HTTPS, or a page on localhost) has.
newtype CacheStorage = CacheStorage (Foreign CacheStorage)
  deriving newtype (Inert)

$( jsFields
     ''CacheStorage
     [ -- The names of the caches, once the promise resolves.
       Method "keys" [t|IO (Promise [Text])|]
     , -- Delete the cache of a name; whether there was one.
       Method "delete" [t|Text -> IO (Promise Bool)|]
     ]
 )

$(jsFieldFunctions ''CacheStorage [])
