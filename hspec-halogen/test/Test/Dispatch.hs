module Test.Dispatch (spec, subscribedSpec) where

import Control.Monad.UUID (MonadUUID)
import Example.Dispatch qualified as Dispatch
import Halogen qualified as H
import Halogen.VDom.DOM.Monad (MonadBrowserDOM)
import Protolude hiding (find)
import Test.Hspec.Halogen

spec :: forall m -> (MonadBrowserTest m, MonadUUID m) => Spec
spec m =
  it "handles an event a synchronous effect dispatches before the dispatch returns" $ runPage $ do
    ui <- mount m (Dispatch.component :: H.Component H.VoidF () Void m) ()
    find ui ".fire" >>= click
    find ui ".result" >>= (`shouldHaveText` "cancelled, handled during dispatch, stopped: poke inner poked")

-- | The same through listeners the component subscribed to: their handlers
-- run inside the effect's dispatch too, on both browser backends, so that
-- cancelling the event, or preventing a checkbox's click from checking it,
-- is still in time.
subscribedSpec :: forall m -> (MonadBrowserTest m, MonadBrowserDOM m, MonadUUID m) => Spec
subscribedSpec m = do
  it "handles an event a synchronous effect dispatches to a subscribed listener before the dispatch returns" $ runPage $ do
    ui <- mount m (Dispatch.subscribed :: H.Component H.VoidF () Void m) ()
    find ui ".poke" >>= click
    find ui ".poked" >>= (`shouldHaveText` "cancelled, handled during dispatch")
    find ui ".steps" >>= (`shouldHaveText` "poke heard poked")

  it "keeps a checkbox an effect clicks unchecked when a subscribed handler prevents the default" $ runPage $ do
    ui <- mount m (Dispatch.subscribed :: H.Component H.VoidF () Void m) ()
    find ui ".click-box" >>= click
    find ui ".clicked" >>= (`shouldHaveText` "unchecked, handled during dispatch")
    find ui ".steps" >>= (`shouldHaveText` "click box clicked")
    find ui ".box" >>= (`getProperty` "checked") >>= (`shouldBe` "false")
