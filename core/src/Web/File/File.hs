{-# LANGUAGE TemplateHaskell #-}

-- | A file the reader chose or dropped on the page: a @File@. Meant to be
-- imported qualified (@File.name@).
module Web.File.File
  ( File (..)
  , toBlob
  , name
  , type_
  , size
  , lastModified
  , text
  , bytes
  )
where

import Data.Foreign (Foreign)
import HPrelude
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsFieldFunctions, jsFields)
import Web.File.Blob (Blob (..))

-- | A @File@, from a 'Web.File.FileList.FileList'.
newtype File = File (Foreign File)
  deriving newtype (Inert)

-- | Every @File@ is a 'Blob', which is what the browser's other APIs take.
toBlob :: File -> Blob
toBlob = coerce

-- What a file says about itself does not change once the browser has made
-- it, so those fields are pure.
$( jsFields
     ''File
     [ -- Its name, without a directory.
       Fixed "name" [t|Text|]
     , -- Its media type, as the browser guessed it from the name, or
       -- empty.
       Fixed "type" [t|Text|]
     , -- How many bytes it holds.
       Fixed "size" [t|Double|]
     , -- When it was last changed, in milliseconds since 1970.
       Fixed "lastModified" [t|Double|]
     , -- Its bytes read as UTF-8 text.
       Awaited "text" [t|IO Text|]
     , -- Its bytes, copied out.
       Awaited "bytes" [t|IO ByteString|]
     ]
 )

$(jsFieldFunctions ''File [])
