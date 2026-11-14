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

import Data.Foreign (Foreign, foreignToNumber, foreignToString, readProp)
import HPrelude
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsFieldFunctions, jsFields)
import Halogen.JSBits.Promise (Promise (..))
import Web.File.Blob (Blob (..))

-- | A @File@, from a 'Web.File.FileList.FileList'.
--
-- Natively a file is what an event made from JSON says of it (a test's, or
-- one a remote client sent): @{"name": "a.png", "type": "image/png",
-- "size": 1024}@, with @lastModified@ if it says. Its bytes are not there:
-- 'text' and 'bytes' give a promise rejected with a
-- 'Halogen.JSBits.Promise.JSError' named @NotInBrowserError@, which
-- 'Halogen.JSBits.Promise.wait' throws, as natively for any promise.
newtype File = File (Foreign File)
  deriving newtype (Inert)

nativeText :: Text -> File -> Text
nativeText name (File value) = fromMaybe "" (readProp name (Just . foreignToString) value)

nativeNumber :: Text -> File -> Double
nativeNumber name (File value) = fromMaybe 0 (readProp name (Just . foreignToNumber) value)

-- | Every @File@ is a 'Blob', which is what the browser's other APIs take.
toBlob :: File -> Blob
toBlob = coerce

-- What a file says about itself does not change once the browser has made
-- it, so those fields are pure.
$( jsFields
     ''File
     [ -- Its name, without a directory.
       Natively [|nativeText "name"|] (Fixed "name" ''Text)
     , -- Its media type, as the browser guessed it from the name, or
       -- empty.
       Natively [|nativeText "type"|] (Fixed "type" ''Text)
     , -- How many bytes it holds.
       Natively [|nativeNumber "size"|] (Fixed "size" ''Double)
     , -- When it was last changed, in milliseconds since 1970.
       Natively [|nativeNumber "lastModified"|] (Fixed "lastModified" ''Double)
     , -- Its bytes read as UTF-8 text, once the promise resolves.
       Method "text" [t|IO (Promise Text)|]
     , -- Its bytes, copied out, once the promise resolves.
       Method "bytes" [t|IO (Promise ByteString)|]
     ]
 )

$(jsFieldFunctions ''File [])
