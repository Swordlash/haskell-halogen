-- | The protocol's JSON: every operation as the client reads it, and back.
module Test.Protocol (spec) where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as Lazy
import Halogen.Remote.Protocol
import Halogen.VDom.DOM.HandlerOptions (EventMatch (..), HandlerOptions (..), KeyMatch (..), handlerOptions, keyMatch)
import Halogen.VDom.DOM.Monad (PropScalar (..), StorageKind (..))
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
  , ListenerOptions 2 "click" 11 handlerOptions {preventDefault = True, matching = Just (Within "a[href]" PlainLeftClick)}
  , ListenerOptions 2 "click" 12 handlerOptions {matching = Just (Within "article" (Within "a.note" AnyEvent))}
  , ListenerOptions 2 "paste" 13 handlerOptions {preventDefault = True, matching = Just (Carrying ["Files", "text/uri-list"])}
  , ListenerOptions 2 "submit" 10 handlerOptions
  , Call 2 "focus" []
  , Call 2 "focus" [object ["preventScroll" .= True]]
  , Call 5 "close" [Aeson.String "done"]
  , Assign 0 "title" (Aeson.String "Notes")
  , HistoryOp (Push (Just "/blog/post/1"))
  , HistoryOp (Replace Nothing)
  , HistoryOp (Go (-1))
  , StorageOp LocalStorage "haskell-halogen:count" (Just "NA==")
  , StorageOp SessionStorage "haskell-halogen:count" Nothing
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
    Aeson.toJSON (HistoryOp (Push (Just "/a"))) `shouldBe` Aeson.toJSON (["history", "push", "/a"] :: [Aeson.Value])
    Aeson.toJSON (HistoryOp (Replace Nothing)) `shouldBe` Aeson.toJSON (["history", "replace", Aeson.Null] :: [Aeson.Value])
    Aeson.toJSON (HistoryOp (Go (-1))) `shouldBe` Aeson.toJSON (["history", "go", Aeson.Number (-1)] :: [Aeson.Value])
    Aeson.toJSON (StorageOp LocalStorage "k" Nothing) `shouldBe` Aeson.toJSON (["storage", "local", "k", Aeson.Null] :: [Aeson.Value])

  it "reads what the page says of itself as the client writes it, and ignores what it does not know" $ do
    let hello =
          Hello
            { location = "http://example.test/blog/?q=1#top"
            , base = Nothing
            , local = Map.fromList [("haskell-halogen:count", "NA==")]
            , session = Map.empty
            }
    decodeFromPage
      "{\"hello\":{\"location\":\"http://example.test/blog/?q=1#top\",\"base\":null,\"storage\":{\"local\":{\"haskell-halogen:count\":\"NA==\"},\"session\":{}}}}"
      `shouldBe` Just (PageHello hello)
    decodeFromPage (encodeFromPage (PageHello hello)) `shouldBe` Just (PageHello hello)
    decodeFromPage "{\"location\":\"http://example.test/about\"}" `shouldBe` Just (PageLocation "http://example.test/about")
    decodeFromPage "{\"storage\":{\"kind\":\"session\",\"key\":\"haskell-halogen:a\",\"value\":\"Mg==\"}}"
      `shouldBe` Just (PageStorage SessionStorage (Just "haskell-halogen:a") (Just "Mg=="))
    decodeFromPage "{\"storage\":{\"kind\":\"local\",\"key\":null,\"value\":null}}" `shouldBe` Just (PageStorage LocalStorage Nothing Nothing)
    for_ [PageLocation "/x", PageStorage LocalStorage (Just "k") Nothing, ProbeQuestion 3] $ \m ->
      decodeFromPage (encodeFromPage m) `shouldBe` Just m
    decodeFromPage "{\"greeting\":1}" `shouldBe` Nothing

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

  it "writes the matches that are objects as the client reads them, and reads them back" $ do
    let optionsOf m = Aeson.toJSON (ListenerOptions 2 "click" 7 handlerOptions {matching = Just m})
        written match = Aeson.toJSON (["options", Aeson.Number 2, "click", Aeson.Number 7, object ["preventDefault" .= False, "stopPropagation" .= False, "match" .= match]] :: [Aeson.Value])
    optionsOf (Within "a[href]" PlainLeftClick) `shouldBe` written (object ["within" .= ("a[href]" :: Text), "match" .= ("plain-left-click" :: Text)])
    optionsOf (Within "article" (Within "a" (Keys [keyMatch "Enter"])))
      `shouldBe` written (object ["within" .= ("article" :: Text), "match" .= object ["within" .= ("a" :: Text), "match" .= [object ["key" .= ("Enter" :: Text)]]]])
    optionsOf (Carrying ["Files"]) `shouldBe` written (object ["carrying" .= ["Files" :: Text]])
    -- An object that is neither is no match the program knows.
    Aeson.decode "[\"options\", 2, \"click\", 7, {\"preventDefault\": false, \"stopPropagation\": false, \"match\": {\"near\": \"a\"}}]"
      `shouldBe` (Nothing :: Maybe Op)
    Aeson.decode "[\"options\", 2, \"click\", 7, {\"preventDefault\": false, \"stopPropagation\": false, \"match\": {\"within\": \"a\"}}]"
      `shouldBe` (Nothing :: Maybe Op)

  it "reads back a batch and an event message" $ do
    let batch = Batch {sequence = 3, ops = every}
    Aeson.decode (Aeson.encode batch) `shouldBe` Just batch
    Aeson.decodeStrictText (encodeBatch batch) `shouldBe` Just batch
    let message = EventMessage {listener = 7, node = 2, eventType = "input", event = object ["target" .= object ["value" .= ("hi" :: Text)]]}
    decodeEventMessage (Lazy.toStrict (encodeToLazyText message)) `shouldBe` Just message
