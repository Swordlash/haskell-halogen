{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}

-- | What the harness needs of the monad a component runs in.
module Test.Hspec.Halogen.Internal.Monad
  ( MonadBrowserTest (..)
  )
where

import Halogen.Component (Component)
import Halogen.IO.Driver (HalogenSocket)
import Halogen.VDom.DOM.Monad (BrowserDOM, MonadBrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (hydrateUIListening, runUIListening)
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
-- 'Halogen.VDom.Driver.runUIListening' and 'hydrateInto' to
-- 'Halogen.VDom.Driver.hydrateUIListening', so an instance for a monad that
-- speaks the browser's DOM only has to say how to run it.
class (MonadUnliftIO m) => MonadBrowserTest m where
  -- | Run the monad from a test.
  runTest :: m a -> IO a

  -- | Mount a component into an element, as
  -- 'Halogen.VDom.Driver.runUIListening' does: the listener hears every
  -- output of the component, from before any of its code runs, which is
  -- how 'Test.Hspec.Halogen.outputs' has what it raised while it was being
  -- mounted.
  mountInto :: (o -> m ()) -> Component q i o m -> i -> Element -> m (HalogenSocket q o m)
  default mountInto :: (MonadBrowserDOM m) => (o -> m ()) -> Component q i o m -> i -> Element -> m (HalogenSocket q o m)
  mountInto listener component input element = case fromElement element of
    Just container -> runUIListening listener component input container
    Nothing -> panic "hspec-halogen: the test container is not an HTML element"

  -- | Mount a component into an element that holds its HTML already,
  -- adopting it, as 'Halogen.VDom.Driver.hydrateUIListening' does, with
  -- the listener as in 'mountInto'.
  hydrateInto :: (o -> m ()) -> Component q i o m -> i -> Element -> m (HalogenSocket q o m)
  default hydrateInto :: (MonadBrowserDOM m) => (o -> m ()) -> Component q i o m -> i -> Element -> m (HalogenSocket q o m)
  hydrateInto listener component input element = case fromElement element of
    Just container -> hydrateUIListening listener component input container
    Nothing -> panic "hspec-halogen: the test container is not an HTML element"

-- Natively BrowserDOM has no DOM to mount into, but a suite written for it
-- should still type-check there, so the language server can load it.
instance MonadBrowserTest BrowserDOM where
  runTest = runBrowserDOM
#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
  mountInto _ _ _ _ = panic "hspec-halogen needs a browser: run the suite on the WebAssembly backend (npm run test-wasm)"
  hydrateInto _ _ _ _ = panic "hspec-halogen needs a browser: run the suite on the WebAssembly backend (npm run test-wasm)"
#endif
