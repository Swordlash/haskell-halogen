{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module Halogen.VDom.DOM.Monad (module Halogen.VDom.DOM.Monad.Common, module X) where

import Halogen.VDom.DOM.Monad.Common

#if defined(javascript_HOST_ARCH)
import Halogen.VDom.DOM.Monad.JS as X
#elif defined(wasm32_HOST_ARCH)
import Halogen.VDom.DOM.Monad.WASM as X
#else
import HPrelude as X ()
#endif