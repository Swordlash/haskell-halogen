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
import Halogen.JSBits.Promise (Promise (..))
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
       Fixed "name" ''Text
     , -- Its media type, as the browser guessed it from the name, or
       -- empty.
       Fixed "type" ''Text
     , -- How many bytes it holds.
       Fixed "size" ''Double
     , -- When it was last changed, in milliseconds since 1970.
       Fixed "lastModified" ''Double
     , -- Its bytes read as UTF-8 text, once the promise resolves.
       Method "text" [t|IO (Promise Text)|]
     , -- Its bytes, copied out, once the promise resolves.
       Method "bytes" [t|IO (Promise ByteString)|]
     ]
 )

$(jsFieldFunctions ''File [])
