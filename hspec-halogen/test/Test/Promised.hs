-- | The Promised example: work started in an effect and awaited in a later
-- action, while the page goes on.
module Test.Promised (spec) where

import Example.Promised qualified as Promised
import Halogen qualified as H
import Protolude hiding (find)
import Test.Hspec.Halogen

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  it "awaits work it started earlier, and goes on responding meanwhile" $ runPage $ do
    ui <- mount m (Promised.component :: H.Component H.VoidF Int Void m) 3000
    find ui ".start" >>= click
    result <- find ui ".result"
    result `shouldHaveText` "started"
    find ui ".collect" >>= click
    result `shouldHaveText` "waiting"
    -- The work takes three seconds; these clicks are rendered long before,
    -- while the other action waits for it.
    ticks <- find ui ".ticks"
    tick <- find ui ".tick"
    click tick
    eventuallyWithin 500 (ticks `shouldHaveText` "1")
    click tick
    eventuallyWithin 500 (ticks `shouldHaveText` "2")
    textContent result `shouldReturn` "waiting"
    eventuallyWithin 6000 (result `shouldHaveText` "resolved: ready")

  it "shows a rejection as a value" $ runPage $ do
    ui <- mount m (Promised.component :: H.Component H.VoidF Int Void m) 100
    find ui ".start-failing" >>= click
    find ui ".collect" >>= click
    find ui ".result" >>= (`shouldHaveText` "rejected: NotFoundError: nothing here")
