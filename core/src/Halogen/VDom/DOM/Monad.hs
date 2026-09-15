{-# LANGUAGE CPP #-}

-- | The 'MonadDOM' class together with the backend this build selected.
--
-- All the interesting code lives elsewhere: the class in
-- "Halogen.VDom.DOM.Monad.Class", the @instance MonadDOM IO@ in one of the
-- three backend modules. This module only picks the backend, which is also why
-- it still needs CPP — an instance is only visible through the import graph, so
-- something has to import the chosen one.
--
-- The cabal file makes the same choice for @exposed-modules@; keep the two in
-- step when adding a backend.
module Halogen.VDom.DOM.Monad
  ( module Halogen.VDom.DOM.Monad.Class
  )
where

import Halogen.VDom.DOM.Monad.Class

#if defined(javascript_HOST_ARCH)
import Halogen.VDom.DOM.Monad.JS ()
#elif defined(wasm32_HOST_ARCH)
import Halogen.VDom.DOM.Monad.WASM ()
#else
import Halogen.VDom.DOM.Monad.Native ()
#endif
