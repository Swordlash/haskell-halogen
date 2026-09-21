-- | The document, as HTML sees it.
module Web.HTML.HTMLDocument
  ( toParentNode
  , cookie
  , setCookie
  )
where

import Data.Coerce
import Data.Foreign
import HPrelude
import Web.DOM.Internal.Types
import Web.DOM.ParentNode (ParentNode (..))

toParentNode :: HTMLDocument -> ParentNode
toParentNode = coerce

-- | Every cookie the document can see, as the browser writes them: pairs
-- separated by @"; "@. "Web.HTML.Cookie" reads and writes this in terms of
-- single cookies, which is almost always what is wanted.
cookie :: (MonadIO m) => HTMLDocument -> m Text

-- | Write one cookie. The string is a single cookie and its attributes, and
-- assigning it adds or replaces that cookie rather than replacing the lot.
setCookie :: (MonadIO m) => Text -> HTMLDocument -> m ()

#if defined(javascript_HOST_ARCH)
foreign import javascript unsafe "js_document_cookie" js_document_cookie :: HTMLDocument -> IO (Foreign Text)
foreign import javascript unsafe "js_document_set_cookie" js_document_set_cookie :: Foreign Text -> HTMLDocument -> IO ()
#elif defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "$1.cookie" js_document_cookie :: HTMLDocument -> IO (Foreign Text)
foreign import javascript unsafe "$2.cookie = $1" js_document_set_cookie :: Foreign Text -> HTMLDocument -> IO ()
#endif

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
cookie doc = liftIO $ foreignToString <$> js_document_cookie doc
setCookie value doc = liftIO $ js_document_set_cookie (stringToForeign value) doc
#else
-- No browser, no cookie jar: reads find nothing and writes go nowhere, as
-- with "Web.Storage.Storage".
cookie _ = pure ""
setCookie _ _ = pass
#endif
