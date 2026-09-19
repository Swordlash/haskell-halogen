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

import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Fork (MonadFork, MonadKill)
import Control.Monad.Parallel (MonadParallel)
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.UUID (MonadUUID)
import HPrelude

-- | Everything the driver asks of a component monad is derived from 'IO',
-- so a backend can be the component monad directly and an application monad
-- can be a transformer stack over one.
newtype BrowserDOM a = BrowserDOM (IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadFork
    , MonadKill
    , MonadParallel
    , MonadUUID
    )

instance PrimMonad BrowserDOM where
  type PrimState BrowserDOM = PrimState IO
  primitive = BrowserDOM . primitive

runBrowserDOM :: BrowserDOM a -> IO a
runBrowserDOM (BrowserDOM io) = io
