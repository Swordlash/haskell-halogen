module Test.Counter (spec) where

import Example.Counter qualified as Counter
import Halogen qualified as H
import Protolude hiding (find)
import Test.Hspec.Halogen

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  it "counts clicks" $ runPage $ do
    ui <- mount m Counter.component ()
    find ui ".count" >>= (`shouldHaveText` "0")
    button <- find ui "button.increment"
    click button
    click button
    find ui ".count" >>= (`shouldHaveText` "2")

  it "raises every new count" $ runPage $ do
    ui <- mount m Counter.component ()
    replicateM_ 3 (find ui "button.increment" >>= click)
    outputs ui `shouldReturn` [1, 2, 3]

  it "tells its count when asked" $ runPage $ do
    ui <- mount m Counter.component ()
    find ui "button.increment" >>= click
    query ui (H.mkRequest Counter.GetCount) `shouldReturn` Just 1
