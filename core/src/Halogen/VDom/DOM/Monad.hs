{-# LANGUAGE CPP #-}

-- | The 'MonadDOM' class together with the backend this build selected.
--
-- The class no longer has an instance at 'IO'; each backend is its own
-- newtype, so more than one can exist in a single build. Both newtypes are
-- always in scope — they are only wrappers — but an instance is visible only
-- through the import graph, so this module still imports one, and that is
-- what 'Halogen.VDom.Driver.runUI' renders through by default.
--
-- The cabal file makes the same choice for @exposed-modules@; keep the two in
-- step when adding a backend.
module Halogen.VDom.DOM.Monad
  ( module Halogen.VDom.DOM.Monad.Class
  , module Halogen.VDom.DOM.Monad.Browser
  , module Halogen.VDom.DOM.Monad.Mem
  )
where

import Halogen.VDom.DOM.Monad.Browser
import Halogen.VDom.DOM.Monad.Class
import Halogen.VDom.DOM.Monad.Mem

#if defined(javascript_HOST_ARCH)
import Halogen.VDom.DOM.Monad.JS ()
#elif defined(wasm32_HOST_ARCH)
import Halogen.VDom.DOM.Monad.WASM ()
#else
import Halogen.VDom.DOM.Monad.Native ()
#endif
