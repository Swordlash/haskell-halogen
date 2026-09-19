-- | The monad the browser DOM is spoken in.
--
-- This is a newtype over 'IO' rather than 'IO' itself so that @MonadDOM@ can
-- have more than one instance in a single build. @instance MonadDOM IO@ would
-- be a claim that @IO@ /means/ the browser, which is what used to force one
-- backend per build and the architecture-selected module list.
--
-- The instance itself lives with the FFI it calls: "Halogen.VDom.DOM.Monad.JS"
-- on the GHC JavaScript backend and "Halogen.VDom.DOM.Monad.WASM" on wasm.
-- There is deliberately no instance on native — there is no browser there.
module Halogen.VDom.DOM.Monad.Browser
  ( BrowserDOM (..)
  , runBrowserDOM
  )
where

import HPrelude

newtype BrowserDOM a = BrowserDOM (IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

runBrowserDOM :: BrowserDOM a -> IO a
runBrowserDOM (BrowserDOM io) = io
