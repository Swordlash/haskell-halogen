-- | 'outputs' holds what a component raised from the start of its mount:
-- in its @Initialize@ and in what that starts, as well as afterwards.
module Test.Raising (spec) where

import Example.Raising qualified as Raising
import Halogen qualified as H
import Protolude hiding (find)
import Test.Hspec.Halogen

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  it "holds what the component raised while it was mounted, and afterwards" $ runPage $ do
    ui <- mount m Raising.component ()
    void (query ui (H.mkTell Raising.Tell))
    outputs ui `shouldReturn` [Raising.Initializing, Raising.Forked, Raising.Told]

  it "holds what the component raised while it was hydrated" $ runPage $ do
    container <- htmlContainer "<p>raising</p>"
    ui <- hydrate m container Raising.component ()
    outputs ui `shouldReturn` [Raising.Initializing, Raising.Forked]
