{-# LANGUAGE TemplateHaskell #-}

-- | Bytes the browser holds, with a media type: a @Blob@. Meant to be
-- imported qualified (@Blob.text@).
module Web.File.Blob
  ( Blob (..)
  , fromBytes
  , size
  , type_
  , text
  , bytes
  )
where

import Data.Foreign (Foreign)
import Data.MediaType (MediaType (..))
import HPrelude
import Halogen.JSBits (Inert, JSText, JSVal, toJSBytes, toJSText)
import Halogen.JSBits.Fields (Member (..), inlineJS, jsFieldFunctions, jsFields)
import Halogen.JSBits.Promise (Promise (..))

-- | A @Blob@: bytes the browser holds, which a page reads asynchronously
-- and can hand to the browser by address ("Web.URL"'s 'Web.URL.createObjectURL').
newtype Blob = Blob (Foreign Blob)
  deriving newtype (Inert)

$( jsFields
     ''Blob
     [ -- How many bytes it holds.
       Fixed "size" [t|Double|]
     , -- Its media type, or empty when nobody gave one.
       Fixed "type" [t|Text|]
     , -- Its bytes read as UTF-8 text, once the promise resolves.
       Method "text" [t|IO (Promise Text)|]
     , -- Its bytes, copied out (@Blob.bytes()@), once the promise resolves.
       Method "bytes" [t|IO (Promise ByteString)|]
     ]
 )

$(jsFieldFunctions ''Blob [])

$(inlineJS "js_new_blob" "new Blob([$1], { type: $2 })" [t|JSVal -> JSText -> IO Blob|])

-- | A @Blob@ of these bytes, of this media type. Natively a placeholder.
fromBytes :: ByteString -> MediaType -> IO Blob
fromBytes content (MediaType mediaType) = do
  array <- toJSBytes content
  js_new_blob array (toJSText mediaType)
