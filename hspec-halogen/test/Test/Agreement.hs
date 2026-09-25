module Test.Agreement (spec) where

import Example.Agreement qualified as Agreement
import Protolude hiding (find)
import Test.Hspec.Halogen

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m =
  it "keeps a box the user ticked when it re-renders" $ runPage $ do
    ui <- mount m Agreement.component ()
    box <- find ui ".agree"
    click box
    getProperty box "checked" `shouldReturn` "true"
    find ui ".rerender" >>= click
    find ui ".renders" >>= (`shouldHaveText` "1")
    getProperty box "checked" `shouldReturn` "true"
