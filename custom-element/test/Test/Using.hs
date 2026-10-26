-- | A Halogen component using a custom element made elsewhere, through
-- 'Halogen.CustomElement.onEvent'.
module Test.Using (spec) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Example.Listener (listener)
import Test.Hspec.Halogen
import Test.Page (dispatch)

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  it "reads a CustomEvent's detail from an element it rendered" $ runPage $ do
    ui <- mount m listener ()
    find ui ".heard" >>= (`shouldHaveText` "nothing")
    unsafeIOToPageM (dispatch "signal" "ping" (object ["from" .= ("Ada" :: Text), "times" .= (3 :: Int)]))
    find ui ".heard" >>= (`shouldHaveText` "Ada pinged 3")

  it "makes no action of a detail that does not decode" $ runPage $ do
    ui <- mount m listener ()
    unsafeIOToPageM (dispatch "signal" "ping" (object ["from" .= ("Ada" :: Text), "times" .= (1 :: Int)]))
    find ui ".heard" >>= (`shouldHaveText` "Ada pinged 1")
    unsafeIOToPageM (dispatch "signal" "ping" ("not a ping" :: Text))
    settle
    find ui ".heard" >>= (`shouldHaveText` "Ada pinged 1")
