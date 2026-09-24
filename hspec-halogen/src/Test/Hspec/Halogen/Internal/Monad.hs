{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}

-- | What the harness needs of the monad a component runs in.
module Test.Hspec.Halogen.Internal.Monad
  ( MonadBrowserTest (..)
  )
where

import Control.Monad.Catch (MonadMask)
import Control.Monad.Fork (MonadFork, MonadKill)
import Control.Monad.Parallel (MonadParallel)
import Control.Monad.UUID (MonadUUID)
import Halogen.Component (Component)
import Halogen.IO.Driver (HalogenSocket)
import Halogen.VDom.DOM.Monad (BrowserDOM, MonadBrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Protolude
import UnliftIO (MonadUnliftIO)
import Web.DOM.Internal.Types (Element, fromElement)

-- | A monad a component can be tested in: the harness mounts the component
-- into the page with it, and runs it from a test to query the component or
-- read its outputs.
--
-- Nothing in the harness names a monad; a suite chooses one where it starts
-- ('Test.Hspec.Halogen.runBrowserTests'), typically 'BrowserDOM', or an
-- application's own monad over it. 'mountInto' defaults to
-- 'Halogen.VDom.Driver.runUI', so an instance for a monad that speaks the
-- browser's DOM only has to say how to run it.
class (MonadUnliftIO m, MonadFork m, MonadKill m, MonadParallel m, MonadMask m, MonadUUID m) => MonadBrowserTest m where
  -- | Run the monad from a test.
  runTest :: m a -> IO a

  -- | Mount a component into an element, as 'Halogen.VDom.Driver.runUI' does.
  mountInto :: Component q i o m -> i -> Element -> m (HalogenSocket q o m)
  default mountInto :: (MonadBrowserDOM m) => Component q i o m -> i -> Element -> m (HalogenSocket q o m)
  mountInto component input element = case fromElement element of
    Just container -> runUI component input container
    Nothing -> panic "hspec-halogen: the test container is not an HTML element"

-- Natively BrowserDOM has no DOM to mount into, but a suite written for it
-- should still type-check there, so the language server can load it.
instance MonadBrowserTest BrowserDOM where
  runTest = runBrowserDOM
#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
  mountInto _ _ _ = panic "hspec-halogen needs a browser: run the suite on the WebAssembly backend (npm run test-wasm)"
#endif
