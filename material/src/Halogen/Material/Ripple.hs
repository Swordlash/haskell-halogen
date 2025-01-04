module Halogen.Material.Ripple (MDCRipple (..), initRipple, destroyRipple) where

import Data.Foreign
import Protolude
import Web.DOM.Internal.Types (HTMLElement)

#if defined(javascript_HOST_ARCH)
foreign import javascript unsafe "window.Halogen.init_material_ripple" initRipple :: HTMLElement -> IO MDCRipple
foreign import javascript unsafe "window.Halogen.destroy_material_ripple" destroyRipple :: MDCRipple -> IO ()
#else
initRipple :: HTMLElement -> IO MDCRipple
initRipple _ = panic "can only be run in JS"

destroyRipple :: MDCRipple -> IO ()
destroyRipple _ = panic "can only be run in JS"
#endif

newtype MDCRipple = MDCRipple (Foreign MDCRipple)
