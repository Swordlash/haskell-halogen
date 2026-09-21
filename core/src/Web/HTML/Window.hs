-- | The window globals that are not about the document tree.
--
-- Obtaining the window itself is 'Halogen.VDom.DOM.Monad.Class.window', a
-- method of @MonadBrowserDOM@, because a backend that is not a browser does
-- not have one.
module Web.HTML.Window
  ( localStorage
  , sessionStorage
  , innerWidth
  , innerHeight
  )
where

import Data.Foreign
import HPrelude
import Web.DOM.Internal.Types (Window (..))
import Web.Storage.Storage (Storage (..))

-- | Storage that outlives the tab.
localStorage :: (MonadIO m) => Window -> m Storage

-- | Storage that is emptied when the tab is closed.
sessionStorage :: (MonadIO m) => Window -> m Storage

-- | The width of the viewport, in CSS pixels.
innerWidth :: (MonadIO m) => Window -> m Int

-- | The height of the viewport, in CSS pixels.
innerHeight :: (MonadIO m) => Window -> m Int

#if defined(javascript_HOST_ARCH)
foreign import javascript unsafe "js_window_local_storage" js_window_local_storage :: Window -> IO (Foreign Storage)
foreign import javascript unsafe "js_window_session_storage" js_window_session_storage :: Window -> IO (Foreign Storage)
foreign import javascript unsafe "js_window_inner_width" js_window_inner_width :: Window -> IO (Foreign Int)
foreign import javascript unsafe "js_window_inner_height" js_window_inner_height :: Window -> IO (Foreign Int)
#elif defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "$1.localStorage" js_window_local_storage :: Window -> IO (Foreign Storage)
foreign import javascript unsafe "$1.sessionStorage" js_window_session_storage :: Window -> IO (Foreign Storage)
foreign import javascript unsafe "$1.innerWidth" js_window_inner_width :: Window -> IO (Foreign Int)
foreign import javascript unsafe "$1.innerHeight" js_window_inner_height :: Window -> IO (Foreign Int)
#endif

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
localStorage w = liftIO $ Storage <$> js_window_local_storage w
sessionStorage w = liftIO $ Storage <$> js_window_session_storage w
innerWidth w = liftIO $ foreignToInt <$> js_window_inner_width w
innerHeight w = liftIO $ foreignToInt <$> js_window_inner_height w
#else
-- A storage nothing can be stored in: see "Web.Storage.Storage".
localStorage _ = pure (Storage (toForeign ()))
sessionStorage _ = pure (Storage (toForeign ()))

-- There is no viewport to measure, and no number that would be less of a lie
-- than another.
innerWidth _ = panic "innerWidth: not available in GHC"
innerHeight _ = panic "innerHeight: not available in GHC"
#endif
