{-# LANGUAGE TemplateHaskell #-}

-- | The address of the page, @window.location@. Meant to be imported
-- qualified (@Location.pathname@).
module Web.HTML.Location
  ( Location (..)
  , href
  , setHref
  , origin
  , pathname
  , search
  , hash
  )
where

import Data.Foreign (Foreign)
import HPrelude hiding (Location, hash)
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsFieldFunctions, jsFields)

-- | A window's location: @w.location@ ("Web.HTML.Window").
newtype Location = Location (Foreign Location)
  deriving newtype (Inert)

$( jsFields
     ''Location
     [ -- The whole address. Setting it loads that address.
       Writable "href" ''Text
     , -- The scheme, host and port: @https://example.com@.
       Live "origin" ''Text
     , -- The path, from its first @/@.
       Live "pathname" ''Text
     , -- The query, @?@ included, or empty.
       Live "search" ''Text
     , -- The fragment, @#@ included, or empty.
       Live "hash" ''Text
     ]
 )

$(jsFieldFunctions ''Location [])
