{-# LANGUAGE TemplateHaskell #-}

-- | The document, as HTML sees it.
module Web.HTML.HTMLDocument
  ( toParentNode
  , cookie
  , setCookie
  )
where

import Data.Coerce
import Halogen.JSBits (JSText, Safety (..), browserJS, fromJSText, toJSText)
import HPrelude
import Web.DOM.Internal.Types
import Web.DOM.ParentNode (ParentNode (..))

-- No browser, no cookie jar: natively reads find nothing and writes go
-- nowhere, as with "Web.Storage.Storage".
$(browserJS ["jsbits/web_browser.js"]
  [ ("js_document_cookie", "js_document_cookie", Unsafe, [t| HTMLDocument -> IO JSText |])
  , ("js_document_set_cookie", "js_document_set_cookie", Unsafe, [t| JSText -> HTMLDocument -> IO () |])
  ])

toParentNode :: HTMLDocument -> ParentNode
toParentNode = coerce

-- | Every cookie the document can see, as the browser writes them: pairs
-- separated by @"; "@. "Web.HTML.Cookie" reads and writes this in terms of
-- single cookies, which is almost always what is wanted.
cookie :: (MonadIO m) => HTMLDocument -> m Text
cookie doc = liftIO $ fromJSText <$> js_document_cookie doc

-- | Write one cookie. The string is a single cookie and its attributes, and
-- assigning it adds or replaces that cookie rather than replacing the lot.
setCookie :: (MonadIO m) => Text -> HTMLDocument -> m ()
setCookie value doc = liftIO $ js_document_set_cookie (toJSText value) doc

