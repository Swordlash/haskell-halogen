{-# LANGUAGE TemplateHaskell #-}

-- | The document, as HTML sees it, as functions. Meant to be imported
-- qualified (@HTMLDocument.setTitle@). The fields themselves
-- (@doc.title_set@) are 'HTMLDocument''s instances, in
-- "Web.DOM.Internal.Types".
module Web.HTML.HTMLDocument
  ( toParentNode
  , cookie
  , setCookie
  , visibilityState
  , title
  , setTitle
  , baseURI
  , fullscreenElement
  , querySelector
  , getElementById
  , exitFullscreen
  , head
  , body
  , activeElement
  , createElement
  , createRange
  , execCommand
  )
where

import Data.Coerce
import HPrelude hiding (head)
import Halogen.JSBits.Fields (jsFieldFunctions)
import Web.DOM.Internal.Types
import Web.DOM.ParentNode (ParentNode (..))
import Web.HTML.HTMLDocument.VisibilityState (VisibilityState (..))
import Web.HTML.HTMLDocument.VisibilityState qualified as VisibilityState

$(jsFieldFunctions ''HTMLDocument ["cookie", "cookie_set", "visibilityState"])

toParentNode :: HTMLDocument -> ParentNode
toParentNode = coerce

-- | Every cookie the document can see, as the browser writes them: pairs
-- separated by @"; "@. "Web.HTML.Cookie" reads and writes this in terms of
-- single cookies, which is almost always what is wanted. No browser, no
-- cookie jar: natively it is empty, as with "Web.Storage.Storage".
cookie :: (MonadIO m) => HTMLDocument -> m Text
cookie doc = liftIO doc.cookie

-- | Write one cookie. The string is a single cookie and its attributes, and
-- assigning it adds or replaces that cookie rather than replacing the lot.
-- Natively it goes nowhere.
setCookie :: (MonadIO m) => Text -> HTMLDocument -> m ()
setCookie value doc = liftIO (doc.cookie_set value)

-- | Whether the page can be seen now.
--
-- A state this does not know is taken as 'Visible', and so is the empty
-- answer a page gets natively, with no browser: what this is read for is
-- mostly to stop work nobody can see, and a page that cannot tell whether it
-- is seen should go on with it.
visibilityState :: (MonadIO m) => HTMLDocument -> m VisibilityState
visibilityState doc =
  liftIO $ fromMaybe Visible . VisibilityState.parse <$> doc.visibilityState
