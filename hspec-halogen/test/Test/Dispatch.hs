module Test.Dispatch (spec) where

import Example.Dispatch qualified as Dispatch
import Halogen qualified as H
import Protolude hiding (find)
import Test.Hspec.Halogen

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m =
  it "handles an event a synchronous effect dispatches before the dispatch returns" $ runPage $ do
    ui <- mount m (Dispatch.component :: H.Component H.VoidF () Void m) ()
    find ui ".fire" >>= click
    find ui ".result" >>= (`shouldHaveText` "cancelled, handled during dispatch, stopped: poke inner poked")
