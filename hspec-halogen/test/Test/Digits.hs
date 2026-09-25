module Test.Digits (spec) where

import Example.Digits qualified as Digits
import Protolude hiding (find)
import Test.Hspec.Halogen

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  it "keeps only the digits typed" $ runPage $ do
    ui <- mount m Digits.component ()
    input <- find ui ".digits"
    typeText input "a1b2"
    getProperty input "value" `shouldReturn` "12"

  it "raises the digits after every keystroke" $ runPage $ do
    ui <- mount m Digits.component ()
    find ui ".digits" >>= (`typeText` "a1b2")
    outputs ui `shouldReturn` ["", "1", "1", "12"]
