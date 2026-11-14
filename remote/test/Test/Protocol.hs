-- | The protocol's JSON: every operation as the client reads it, and back.
module Test.Protocol (spec) where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy qualified as Lazy
import Halogen.Remote.Protocol
import Halogen.VDom.DOM.HandlerOptions (EventMatch (..), HandlerOptions (..), KeyMatch (..), handlerOptions, keyMatch)
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
  , ListenerOptions 2 "click" 7 handlerOptions {preventDefault = True, matching = Just PlainLeftClick}
  , ListenerOptions 2 "keydown" 8 handlerOptions {stopPropagation = True, matching = Just (Keys [keyMatch "/", (keyMatch "s") {ctrlKey = Just True, altKey = Just False}])}
  , ListenerOptions 2 "input" 9 handlerOptions {matching = Just AnyEvent}
  , ListenerOptions 2 "submit" 10 handlerOptions
  , Call 2 "focus" []
  , Call 2 "focus" [object ["preventScroll" .= True]]
  , Call 5 "close" [Aeson.String "done"]
  , Assign 0 "title" (Aeson.String "Notes")
  ]

spec :: Spec
spec = describe "the protocol" $ do
  it "reads back every operation it writes" $
    for_ every $
      \op -> Aeson.decode (Aeson.encode op) `shouldBe` Just op

  it "writes an operation as an array with its name first" $ do
    Aeson.toJSON (Create 2 Nothing "div") `shouldBe` Aeson.toJSON (["create", Aeson.Number 2, Aeson.Null, "div"] :: [Aeson.Value])
    Aeson.toJSON (SetProperty 2 "tabIndex" (ScalarNum 3)) `shouldBe` Aeson.toJSON (["prop", Aeson.Number 2, "tabIndex", Aeson.Number 3] :: [Aeson.Value])
    Aeson.toJSON (Call 2 "scrollIntoView" [object ["block" .= ("center" :: Text)]])
      `shouldBe` Aeson.toJSON (["call", Aeson.Number 2, "scrollIntoView", Aeson.toJSON [object ["block" .= ("center" :: Text)]]] :: [Aeson.Value])
    Aeson.toJSON (Assign 0 "title" "Notes") `shouldBe` Aeson.toJSON (["set", Aeson.Number 0, "title", "Notes"] :: [Aeson.Value])

  it "writes a listener's options as the client reads them" $ do
    Aeson.toJSON (ListenerOptions 2 "keydown" 8 handlerOptions {preventDefault = True, matching = Just (Keys [(keyMatch "s") {ctrlKey = Just True}])})
      `shouldBe` Aeson.toJSON
        ( [ "options"
          , Aeson.Number 2
          , "keydown"
          , Aeson.Number 8
          , object
              [ "preventDefault" .= True
              , "stopPropagation" .= False
              , "match" .= [object ["key" .= ("s" :: Text), "ctrlKey" .= True]]
              ]
          ]
            :: [Aeson.Value]
        )
    Aeson.toJSON (ListenerOptions 2 "click" 7 handlerOptions)
      `shouldBe` Aeson.toJSON (["options", Aeson.Number 2, "click", Aeson.Number 7, object ["preventDefault" .= False, "stopPropagation" .= False, "match" .= Aeson.Null]] :: [Aeson.Value])

  it "reads back a batch and an event message" $ do
    let batch = Batch {sequence = 3, ops = every}
    Aeson.decode (Aeson.encode batch) `shouldBe` Just batch
    Aeson.decodeStrictText (encodeBatch batch) `shouldBe` Just batch
    let message = EventMessage {listener = 7, node = 2, eventType = "input", event = object ["target" .= object ["value" .= ("hi" :: Text)]]}
    decodeEventMessage (Lazy.toStrict (encodeToLazyText message)) `shouldBe` Just message
