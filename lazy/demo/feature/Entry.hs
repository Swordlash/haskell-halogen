-- | The demo library's entry, which hands "Feature"'s component over to
-- "Halogen.Lazy.Dyld.load". Only the wasm backend builds it, into
-- liblazy-demo.so (toolchain/build-learn-rootfs.sh): a @foreign export
-- javascript@ means nothing natively.
module Entry () where

import Data.Dynamic (Dynamic)
import Feature (feature)
import Foreign.StablePtr (StablePtr)
import Halogen.Lazy.Demo (Feature)
import Halogen.Lazy.Dyld qualified as Dyld
import Halogen.VDom.DOM.Monad (BrowserDOM)

foreign export javascript "halogen_lazy_demo" entry :: IO (StablePtr Dynamic)

entry :: IO (StablePtr Dynamic)
entry = Dyld.offer (feature :: Feature BrowserDOM)
