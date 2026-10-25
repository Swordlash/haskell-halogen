{-# LANGUAGE TemplateHaskell #-}

-- | The harness without its runner: the other specs' tests that act on the
-- page, again, with the runner's actions taken away, so that the harness
-- performs them itself, with synthetic events (page.js's in-page mode).
-- That is how a suite runs in a page with no Playwright behind it, as the
-- haskell-halogen tutorial's does; the components cannot tell the
-- difference.
module Test.InPage (spec) where

import Halogen.JSBits (Safety (..), browserJS)
import Protolude
import Test.Agreement qualified
import Test.Counter qualified
import Test.Digits qualified
import Test.Drag qualified
import Test.Hspec (around_)
import Test.Hspec.Halogen (MonadBrowserTest, Spec, describe)
import Test.Todo qualified

$( browserJS
     ["test/jsbits/inpage.js"]
     [ ("js_hide_runner", "halogen_inpage_hide_runner", Unsafe, [t|IO ()|])
     , ("js_restore_runner", "halogen_inpage_restore_runner", Unsafe, [t|IO ()|])
     ]
 )

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = around_ (bracket_ js_hide_runner js_restore_runner) $ do
  describe "Counter" (Test.Counter.spec m)
  describe "Agreement" (Test.Agreement.spec m)
  describe "Digits" (Test.Digits.spec m)
  describe "Todo" (Test.Todo.spec m)
  describe "Drag" (Test.Drag.spec m)
