-- | The protocol's JSON: every operation as the client reads it, and back.
module Test.Protocol (spec) where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy qualified as Lazy
import Halogen.Remote.Protocol
import Halogen.VDom.DOM.Monad (PropScalar (..))
import Protolude
import Test.Hspec (Spec, describe, it, shouldBe)

every :: [Op]
every =
  [ Clear
  , Create 2 Nothing "div"
  , Create 3 (Just "http://www.w3.org/2000/svg") "circle"
  , CreateText 4 "a \"quoted\" text"
  , SetText 4 ""
  , Insert 1 0 2
  , Remove 1 2
  , SetAttribute 3 Nothing "r" "5"
  , SetAttribute 3 (Just "http://www.w3.org/1999/xlink") "href" "#a"
  , RemoveAttribute 3 Nothing "r"
  , SetProperty 2 "value" (ScalarText "typed")
  , SetProperty 2 "tabIndex" (ScalarInt 3)
  , SetProperty 2 "valueAsNumber" (ScalarNum 1.5)
  , SetProperty 2 "checked" (ScalarBool True)
  , RemoveProperty 2 "checked"
  , Listen 2 "click" 7
  , Unlisten 2 "click" 7
  ]

spec :: Spec
spec = describe "the protocol" $ do
  it "reads back every operation it writes" $
    for_ every $
      \op -> Aeson.decode (Aeson.encode op) `shouldBe` Just op

  it "writes an operation as an array with its name first" $ do
    Aeson.toJSON (Create 2 Nothing "div") `shouldBe` Aeson.toJSON (["create", Aeson.Number 2, Aeson.Null, "div"] :: [Aeson.Value])
    Aeson.toJSON (SetProperty 2 "tabIndex" (ScalarNum 3)) `shouldBe` Aeson.toJSON (["prop", Aeson.Number 2, "tabIndex", Aeson.Number 3] :: [Aeson.Value])

  it "reads back a batch and an event message" $ do
    let batch = Batch {sequence = 3, ops = every}
    Aeson.decode (Aeson.encode batch) `shouldBe` Just batch
    Aeson.decodeStrictText (encodeBatch batch) `shouldBe` Just batch
    let message = EventMessage {listener = 7, node = 2, eventType = "input", event = object ["target" .= object ["value" .= ("hi" :: Text)]]}
    decodeEventMessage (Lazy.toStrict (encodeToLazyText message)) `shouldBe` Just message
