module Main (main) where

import Halogen.VDom.DOM.Monad (BrowserDOM)
import Protolude
import Test.Hspec (describe)
import Test.Hspec.Halogen (runBrowserTests)
import Test.Tabs qualified
import Test.TextField qualified

-- | Built as a reactor exporting @hs_start@, which the browser test runner
-- calls once the page has loaded the material components' JavaScript and CSS.
-- Under browser GHCi (toolchain/dev-test.sh) it is run with @:main@ instead.
--
-- The specs name no monad; this is where the suite picks the one the
-- components run in.
main :: IO ()
main = runBrowserTests $ do
  describe "Tabs" (Test.Tabs.spec BrowserDOM)
  describe "TextField" (Test.TextField.spec BrowserDOM)

#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)
foreign export javascript "hs_start" main :: IO ()
#endif
