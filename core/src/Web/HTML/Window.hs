{-# LANGUAGE TemplateHaskell #-}

-- | The window, as functions. Meant to be imported qualified
-- (@Window.navigator@). The fields themselves (@w.navigator.onLine@) are
-- 'Window''s instances, in "Web.DOM.Internal.Types".
--
-- The two stores are not here: they are the storage methods of
-- 'Halogen.VDom.DOM.Monad.Class.MonadBrowserDOM', so that a backend with no
-- browser can have them too.
--
-- Obtaining the window itself is 'Halogen.VDom.DOM.Monad.Class.window', a
-- method of @MonadBrowserDOM@, because a backend that is not a browser does
-- not have one; 'window' here is for a program that only runs in one.
module Web.HTML.Window
  ( window
  , innerWidth
  , innerHeight
  , locationHash
  , document
  , navigator
  , history
  , location
  , crypto
  , console
  , isSecureContext
  , print
  , getSelection
  , caches
  )
where

import HPrelude hiding (print)
import Halogen.JSBits.Fields (inlineJS, jsFieldFunctions)
import Web.DOM.Internal.Types (Window (..))

$(jsFieldFunctions ''Window ["innerWidth", "innerHeight"])

$(inlineJS "js_window" "window" [t|IO Window|])

-- | The window the program runs in. Natively there is none, and this gives
-- a placeholder whose every field has its inert answer: nothing done,
-- 'False', 0, empty, 'Nothing'. A program written for any backend asks
-- 'Halogen.VDom.DOM.Monad.Class.window' instead.
window :: IO Window
window = js_window

-- | The width of the viewport, in CSS pixels. Natively, with no viewport, 0.
innerWidth :: (MonadIO m) => Window -> m Int
innerWidth w = liftIO w.innerWidth

-- | The height of the viewport, in CSS pixels. Natively 0.
innerHeight :: (MonadIO m) => Window -> m Int
innerHeight w = liftIO w.innerHeight

-- | The fragment of the page's URL, @#@ included, or empty when there is
-- none. A link to @#/somewhere@ changes it without loading a page, adds a
-- history entry, and fires @hashchange@ on the window, which together make it
-- the router a page served from static files can have: back, forward, reload
-- and deep links all arrive as the same event and the same read. A page that
-- was not loaded from a URL, natively, has no fragment.
locationHash :: (MonadIO m) => Window -> m Text
locationHash w = liftIO w.location.hash
