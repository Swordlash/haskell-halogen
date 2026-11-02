{-# LANGUAGE TemplateHaskell #-}

-- | Addresses, as the browser parses them: a @URL@, and the addresses of
-- 'Blob's. Meant to be imported qualified (@URL.parse@).
module Web.URL
  ( URL (..)
  , parse
  , href
  , setHref
  , origin
  , pathname
  , search
  , hash
  , createObjectURL
  , revokeObjectURL
  )
where

import Data.Foreign (Foreign)
import HPrelude hiding (hash)
import Halogen.JSBits (Inert, JSText, JSVal, fromJSText, isNull, toJSText)
import Halogen.JSBits.Fields (Member (..), inlineJS, jsFieldFunctions, jsFields)
import Web.File.Blob (Blob (..))

-- | A parsed address, which the page can change part by part.
newtype URL = URL (Foreign URL)
  deriving newtype (Inert)

$( jsFields
     ''URL
     [ -- The whole address, written out. Setting it parses the new one.
       Writable "href" [t|IO Text|]
     , -- The scheme, host and port: @https://example.com@.
       Live "origin" [t|IO Text|]
     , -- The path, from its first @/@.
       Live "pathname" [t|IO Text|]
     , -- The query, @?@ included, or empty.
       Live "search" [t|IO Text|]
     , -- The fragment, @#@ included, or empty.
       Live "hash" [t|IO Text|]
     ]
 )

$(jsFieldFunctions ''URL [])

-- The constructor throws for an address it cannot parse, so the imports
-- catch that and give null.
$( inlineJS
     "js_parse"
     "(() => { try { return new URL($1, $2); } catch { return null; } })()"
     [t|JSText -> JSText -> IO JSVal|]
 )

$( inlineJS
     "js_parse_absolute"
     "(() => { try { return new URL($1); } catch { return null; } })()"
     [t|JSText -> IO JSVal|]
 )

$(inlineJS "js_create_object_url" "URL.createObjectURL($1)" [t|Blob -> IO JSText|])

$(inlineJS "js_revoke_object_url" "URL.revokeObjectURL($1)" [t|JSText -> IO ()|])

-- | An address, read against a base when it is relative (as a link's
-- @href@ is read against the page's @document.baseURI@), or 'Nothing' when
-- it is not one. Natively always 'Nothing'.
parse :: Text -> Maybe Text -> IO (Maybe URL)
parse address base = do
  url <- maybe (js_parse_absolute (toJSText address)) (js_parse (toJSText address) . toJSText) base
  pure (if isNull url then Nothing else Just (URL url))

-- | An address the browser serves the blob's bytes at (@blob:...@), for as
-- long as the page lives or until 'revokeObjectURL'. Natively empty.
createObjectURL :: Blob -> IO Text
createObjectURL blob = fromJSText <$> js_create_object_url blob

-- | Let the browser forget an address 'createObjectURL' made, and the
-- bytes, once nothing else holds them.
revokeObjectURL :: Text -> IO ()
revokeObjectURL = js_revoke_object_url . toJSText
