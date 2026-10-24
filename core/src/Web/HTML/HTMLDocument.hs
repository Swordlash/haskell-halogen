{-# LANGUAGE TemplateHaskell #-}

-- | The document, as HTML sees it.
module Web.HTML.HTMLDocument
  ( toParentNode
  , cookie
  , setCookie
  , visibilityState
  )
where

import Data.Coerce
import HPrelude
import Halogen.JSBits (JSText, Safety (..), browserJS, fromJSText, toJSText)
import Web.DOM.Internal.Types
import Web.DOM.ParentNode (ParentNode (..))
import Web.HTML.HTMLDocument.VisibilityState (VisibilityState (..))
import Web.HTML.HTMLDocument.VisibilityState qualified as VisibilityState

-- No browser, no cookie jar: natively reads find nothing and writes go
-- nowhere, as with "Web.Storage.Storage".
$( browserJS
     ["jsbits/web_browser.js"]
     [ ("js_document_cookie", "js_document_cookie", Unsafe, [t|HTMLDocument -> IO JSText|])
     , ("js_document_set_cookie", "js_document_set_cookie", Unsafe, [t|JSText -> HTMLDocument -> IO ()|])
     , ("js_document_visibility_state", "js_document_visibility_state", Unsafe, [t|HTMLDocument -> IO JSText|])
     ]
 )

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

-- | Whether the page can be seen now.
--
-- A state this does not know is taken as 'Visible', and so is the empty
-- answer a page gets natively, with no browser: what this is read for is
-- mostly to stop work nobody can see, and a page that cannot tell whether it
-- is seen should go on with it.
visibilityState :: (MonadIO m) => HTMLDocument -> m VisibilityState
visibilityState doc =
  liftIO $ fromMaybe Visible . VisibilityState.parse . fromJSText <$> js_document_visibility_state doc
