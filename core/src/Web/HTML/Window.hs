{-# LANGUAGE TemplateHaskell #-}

-- | The window globals that are not about the document tree.
--
-- The two stores are not here: they are the storage methods of
-- 'Halogen.VDom.DOM.Monad.Class.MonadBrowserDOM', so that a backend with no
-- browser can have them too.
--
-- Obtaining the window itself is 'Halogen.VDom.DOM.Monad.Class.window', a
-- method of @MonadBrowserDOM@, because a backend that is not a browser does
-- not have one.
module Web.HTML.Window
  ( innerWidth
  , innerHeight
  , locationHash
  )
where

import Halogen.JSBits (JSText, Safety (..), browserJS, fromJSText)
import HPrelude
import Web.DOM.Internal.Types (Window (..))

$(browserJS ["jsbits/web_browser.js"]
  [ ("js_window_inner_width", "js_window_inner_width", Unsafe, [t| Window -> IO Int |])
  , ("js_window_inner_height", "js_window_inner_height", Unsafe, [t| Window -> IO Int |])
  , ("js_window_location_hash", "js_window_location_hash", Unsafe, [t| Window -> IO JSText |])
  ])

-- | The width of the viewport, in CSS pixels. Natively, with no viewport, 0.
innerWidth :: (MonadIO m) => Window -> m Int
innerWidth = liftIO . js_window_inner_width

-- | The height of the viewport, in CSS pixels. Natively 0.
innerHeight :: (MonadIO m) => Window -> m Int
innerHeight = liftIO . js_window_inner_height

-- | The fragment of the page's URL, @#@ included, or empty when there is
-- none. A link to @#/somewhere@ changes it without loading a page, adds a
-- history entry, and fires @hashchange@ on the window, which together make it
-- the router a page served from static files can have: back, forward, reload
-- and deep links all arrive as the same event and the same read. A page that
-- was not loaded from a URL, natively, has no fragment.
locationHash :: (MonadIO m) => Window -> m Text
locationHash w = liftIO $ fromJSText <$> js_window_location_hash w
