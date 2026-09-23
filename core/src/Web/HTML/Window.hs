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

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Data.Foreign
#endif

import HPrelude
import Web.DOM.Internal.Types (Window (..))

-- | The width of the viewport, in CSS pixels.
innerWidth :: (MonadIO m) => Window -> m Int

-- | The height of the viewport, in CSS pixels.
innerHeight :: (MonadIO m) => Window -> m Int

-- | The fragment of the page's URL, @#@ included, or empty when there is
-- none. A link to @#/somewhere@ changes it without loading a page, adds a
-- history entry, and fires @hashchange@ on the window, which together make it
-- the router a page served from static files can have: back, forward, reload
-- and deep links all arrive as the same event and the same read.
locationHash :: (MonadIO m) => Window -> m Text

#if defined(javascript_HOST_ARCH)
foreign import javascript unsafe "js_window_inner_width" js_window_inner_width :: Window -> IO (Foreign Int)
foreign import javascript unsafe "js_window_inner_height" js_window_inner_height :: Window -> IO (Foreign Int)
foreign import javascript unsafe "js_window_location_hash" js_window_location_hash :: Window -> IO (Foreign Text)
#elif defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "$1.innerWidth" js_window_inner_width :: Window -> IO (Foreign Int)
foreign import javascript unsafe "$1.innerHeight" js_window_inner_height :: Window -> IO (Foreign Int)
foreign import javascript unsafe "$1.location.hash" js_window_location_hash :: Window -> IO (Foreign Text)
#endif

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
innerWidth w = liftIO $ foreignToInt <$> js_window_inner_width w
innerHeight w = liftIO $ foreignToInt <$> js_window_inner_height w
locationHash w = liftIO $ foreignToString <$> js_window_location_hash w
#else
-- There is no viewport to measure, and no number that would be less of a lie
-- than another.
innerWidth _ = panic "innerWidth: not available in GHC"
innerHeight _ = panic "innerHeight: not available in GHC"

-- A page that was not loaded from a URL has no fragment in it.
locationHash _ = pure ""
#endif
