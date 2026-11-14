{-# LANGUAGE CPP #-}

-- | Natively a value holds what it was made from, so an event made from
-- JSON has the properties a browser's would, and the handlers that read an
-- event ('HE.onValueInput', 'HE.onChecked', a key handler, a click's
-- position) read them from it under the in-memory DOM as they would in a
-- page.
module Test.NativeEvents (spec) where

import Prelude

#if !defined(HALOGEN_WASI) && (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "native events" $ pure ()

#else

import Control.Monad.State.Class (modify)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Foldable (for_)
import Data.Foreign (foreignToBool, foreignToInt, foreignToNumber, foreignToString, readElements, readProp, stringToForeign, toForeign, unsafeFromForeign)
import Data.IORef (readIORef)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (isNull, jsNull, jsValText, toJSInt)
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.Driver (runUI)
import Halogen.JSBits.Promise (JSError (..), waitCatch)
import Halogen.VDom.DOM.HandlerOptions (EventMatch (..), isPlainLeftClick, matchesEvent)
import Halogen.VDom.Types (ElemName (..))
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)
import Web.Clipboard.ClipboardEvent qualified as CE
import Web.DOM.Element qualified as Element
import Web.Event.Event qualified as Event
import Web.File.File qualified as File
import Web.File.FileList qualified as FileList
import Web.HTML.Event.DataTransfer qualified as DataTransfer
import Web.HTML.Event.DragEvent qualified as DE
import Web.UIEvent.KeyboardEvent qualified as KE
import Web.UIEvent.MouseEvent qualified as ME

-- | What the handlers below have read, echoed in a paragraph.
data Seen = Seen {typed :: Text, ticked :: Bool, pressed :: Text}

data Heard = Typed Text | Ticked Bool | Pressed Text

echo :: H.Component H.VoidF () Void MemDOM
echo =
  H.component
    (\_ -> pure Seen {typed = "", ticked = False, pressed = ""})
    render
    [ H.onAction $ \case
        Typed t -> modify $ \s -> s {typed = t}
        Ticked b -> modify $ \s -> s {ticked = b}
        Pressed k -> modify $ \s -> s {pressed = k}
    ]
  where
    render :: Seen -> H.ComponentHTML Heard Empty MemDOM
    render s =
      HH.div_
        [ HH.input [HE.onValueInput Typed, HE.onKeyDown (Pressed . KE.key)]
        , HH.input [HP.checked False, HE.onChecked Ticked]
        , HH.p_ [HH.text (s.typed <> "|" <> (if s.ticked then "ticked" else "unticked") <> "|" <> s.pressed)]
        ]

-- | Where the last click was, as its handler read it.
pointer :: H.Component H.VoidF () Void MemDOM
pointer =
  H.component
    (\_ -> pure (-1, -1))
    render
    [H.onAction (\at -> modify (const at))]
  where
    render :: (Double, Double) -> H.ComponentHTML (Double, Double) Empty MemDOM
    render (x, y) =
      HH.div_
        [ HH.button [HE.onClick (\e -> (ME.clientX e, ME.clientY e))] []
        , HH.p_ [HH.text (T.pack (show x <> "," <> show y))]
        ]

-- | Run the listeners of one type an element has, with the event.
fire :: Text -> Value -> N.NativeNode -> IO ()
fire ty ev node = do
  listeners <- readIORef node.listeners
  assertWith ("the element has a " <> show ty <> " listener") (any ((== ty) . (.eventType)) listeners)
  for_ [l | l <- listeners, l.eventType == ty] $ \l -> runMemDOM (l.fire (N.event ev))

-- | An event for the element whose listener runs, which a browser's
-- @target@ and @currentTarget@ both name.
onElement :: [(Key, Value)] -> Value
onElement fields = object ["target" .= object fields', "currentTarget" .= object fields']
  where
    fields' = [k .= v | (k, v) <- fields]

-- | A container whose one click handler follows the links inside it, as
-- a page of rendered Markdown does: declared as a plain click on a link
-- inside, and reading the link the way an application's handler does, from
-- the event's target ('Element.closestNow', 'Element.getAttributeNow').
delegated :: H.Component H.VoidF () Void MemDOM
delegated =
  H.component
    (\_ -> pure "")
    render
    [H.onAction (\href -> modify (const href))]
  where
    render :: Text -> H.ComponentHTML Text Empty MemDOM
    render followed =
      HH.div_
        [ HH.div
            [HE.preventingDefault (HE.matching (HE.within "a[href]" HE.plainLeftClick) (HE.handler' (Event.EventType "click") followed'))]
            [HH.text "rendered"]
        , HH.p_ [HH.text followed]
        ]
    followed' event = do
      target <- Event.target event >>= Element.fromEventTarget
      anchor <- Element.closestNow target "a[href]"
      Element.getAttributeNow anchor "href"

-- | A container whose paste handler takes the files of a paste and leaves
-- a paste of text alone, reading the files' names, types and sizes.
pasting :: H.Component H.VoidF () Void MemDOM
pasting =
  H.component
    (\_ -> pure "")
    render
    [H.onAction (\seen -> modify (const seen))]
  where
    render :: Text -> H.ComponentHTML Text Empty MemDOM
    render seen =
      HH.div_
        [ HH.div [HE.preventingDefault (HE.matching (HE.carrying ["Files"]) (HE.onPaste pasted))] []
        , HH.p_ [HH.text seen]
        ]
    pasted event =
      let files = maybe [] (FileList.now . (.files)) (CE.clipboardData event)
       in T.intercalate ";" [File.name f <> "," <> File.type_ f <> "," <> T.pack (show (File.size f)) | f <- files]

-- | A link as a remote client describes the element it found for a
-- selector: its link members and every attribute.
link :: [(Key, Value)] -> [(Key, Value)] -> Value
link members attributes = object (members <> ["tagName" .= ("A" :: Text), "attributes" .= object attributes])

-- | A click whose target is a span inside the link, as a remote client
-- sends it for a listener on a container whose match names the selector.
clickOn :: [(Key, Value)] -> [(Key, Value)] -> Value -> Value
clickOn fields container found =
  object
    ( fields
        <> [ "target" .= object ["tagName" .= ("SPAN" :: Text), "closest" .= object ["a[href]" .= found]]
           , "currentTarget" .= object (("tagName" .= ("DIV" :: Text)) : [k .= v | (k, v) <- container])
           ]
    )

plain :: [(Key, Value)]
plain = ["button" .= (0 :: Int)]

mounted :: IO (N.NativeNode, [N.NativeNode], MemDOM ())
mounted = do
  container <- N.newElement Nothing (ElemName "div")
  H.HalogenSocket {H.dispose = dispose} <- runMemDOM (runUI echo () (N.fromNative container))
  roots <- N.childNodes container
  children <- concat <$> traverse N.childNodes roots
  pure (container, children, dispose)

spec :: Spec
spec = describe "native events" $ do
  describe "values" $ do
    it "hold what they were made from" $ do
      assertEqual "text" "hello" (foreignToString (stringToForeign "hello"))
      assertEqual "an int" 42 (foreignToInt (toForeign (42 :: Int)))
      assertWith "a bool" (foreignToBool (toForeign True))
      assertEqual "a double" 2.5 (foreignToNumber (toForeign (2.5 :: Double)))
      assertEqual "an int as a number" 3 (foreignToNumber (toForeign (3 :: Int)))
      assertEqual "a held value comes back" 'x' (unsafeFromForeign (toForeign 'x'))
      assertEqual "jsValText" "held" (jsValText (toForeign ("held" :: Text)))
      assertWith "null is null" (isNull jsNull)
      assertWith "a number is not null" (not (isNull (toJSInt 1)))
    it "read JSON scalars and objects' properties" $ do
      let v =
            toForeign
              $ object
                [ "s" .= ("str" :: Text)
                , "n" .= (7.9 :: Double)
                , "b" .= True
                , "o" .= object ["deep" .= (1 :: Int)]
                , "z" .= Null
                ]
      assertEqual "a string" (Just "str") (readProp "s" (Just . foreignToString) v)
      assertEqual "a number, truncated" (Just 7) (readProp "n" (Just . foreignToInt) v)
      assertEqual "a number, as it is" (Just 7.9) (readProp "n" (Just . foreignToNumber) v)
      assertEqual "a bool" (Just True) (readProp "b" (Just . foreignToBool) v)
      assertEqual "a nested property" (Just 1) (readProp "o" (readProp "deep" (Just . foreignToInt)) v)
      assertEqual "null is no property" Nothing (readProp "z" (Just . foreignToInt) v)
      assertEqual "a missing one" Nothing (readProp "missing" (Just . foreignToInt) v)
    it "give the stubs' answers for what they do not hold" $ do
      assertEqual "no property on ()" Nothing (readProp "x" (Just . foreignToInt) (toForeign ()))
      assertEqual "no property on a number" Nothing (readProp "x" (Just . foreignToInt) (toForeign (3 :: Int)))
      assertEqual "no text in a number" "" (foreignToString (toForeign (3 :: Int)))
      assertEqual "no number in text" 0 (foreignToInt (stringToForeign "3"))
      assertEqual "no double in text" 0 (foreignToNumber (stringToForeign "3"))
      assertWith "no bool in text" (not (foreignToBool (stringToForeign "true")))
      assertEqual "no text in jsNull" "" (jsValText jsNull)

  describe "handlers that read the event" $ do
    it "deliver what the event says" $ do
      (_, children, dispose) <- mounted
      case children of
        [field, box, paragraph] -> do
          fire "input" (onElement [("value", "hi")]) field
          fire "change" (onElement [("checked", Bool True)]) box
          fire "keydown" (object ["key" .= ("Enter" :: Text), "code" .= ("Enter" :: Text)]) field
          N.renderToText paragraph >>= assertEqual "the paragraph" "<p>hi|ticked|Enter</p>"
        _ -> assertWith "the component renders two inputs and a paragraph" False
      runMemDOM dispose
    it "deliver nothing for an event without the property" $ do
      (_, children, dispose) <- mounted
      case children of
        [field, _, paragraph] -> do
          fire "input" (object []) field
          fire "input" (object ["target" .= object ["value" .= ("only the target" :: Text)]]) field
          N.renderToText paragraph >>= assertEqual "the paragraph" "<p>|unticked|</p>"
        _ -> assertWith "the component renders two inputs and a paragraph" False
      runMemDOM dispose
    it "read where a click was" $ do
      container <- N.newElement Nothing (ElemName "div")
      H.HalogenSocket {H.dispose = dispose} <- runMemDOM (runUI pointer () (N.fromNative container))
      children <- concat <$> (traverse N.childNodes =<< N.childNodes container)
      case children of
        [button, paragraph] -> do
          fire "click" (object ["clientX" .= (12.5 :: Double), "clientY" .= (40 :: Int)]) button
          N.renderToText paragraph >>= assertEqual "the paragraph" "<p>12.5,40.0</p>"
          -- What the event does not say reads as 0, as before.
          fire "click" (object ["clientX" .= ("left" :: Text)]) button
          N.renderToText paragraph >>= assertEqual "the paragraph" "<p>0.0,0.0</p>"
        _ -> assertWith "the component renders a button and a paragraph" False
      runMemDOM dispose

  describe "arrays" $
    it "have a length and elements by index, as JavaScript reads them" $ do
      let v = toForeign (toJSON ["a", "b" :: Text])
      assertEqual "length" (Just 2) (readProp "length" (Just . foreignToInt) v)
      assertEqual "the first" (Just "a") (readProp "0" (Just . foreignToString) v)
      assertEqual "the second" (Just "b") (readProp "1" (Just . foreignToString) v)
      assertEqual "past the end" Nothing (readProp "2" (Just . foreignToString) v)
      assertEqual "not an index as JavaScript writes one" Nothing (readProp "01" (Just . foreignToString) v)
      assertEqual "negative" Nothing (readProp "-1" (Just . foreignToString) v)
      assertEqual "every element" ["a", "b"] (map foreignToString (readElements v))
      assertEqual "no elements in an object" [] (map foreignToString (readElements (toForeign (object ["length" .= ("2" :: Text)]))))

  describe "a delegated match, from the event's JSON" $ do
    let delegatedClick = Within "a[href]" PlainLeftClick
        matches m = matchesEvent m . N.event
        found = link [] ["href" .= ("/p/1" :: Text)]
    it "matches a plain click on a link inside, judged on the link" $ do
      assertWith "a plain click" (matches delegatedClick (clickOn plain [] found))
      -- The container names another target; the link does not, and the
      -- link is the one judged.
      assertWith "the container's target is not the link's" (matches delegatedClick (clickOn plain ["target" .= ("_blank" :: Text)] found))
      assertWith "any event on a link inside" (matches (Within "a[href]" AnyEvent) (clickOn [] [] found))
    it "does not match a click the browser would follow itself" $ do
      assertWith "a ctrl-click" (not (matches delegatedClick (clickOn (plain <> ["ctrlKey" .= True]) [] found)))
      assertWith "another button" (not (matches delegatedClick (clickOn ["button" .= (1 :: Int)] [] found)))
      assertWith "prevented already" (not (matches delegatedClick (clickOn (plain <> ["defaultPrevented" .= True]) [] found)))
      assertWith "a link to another tab" (not (matches delegatedClick (clickOn plain [] (link ["target" .= ("_blank" :: Text)] ["href" .= ("/x" :: Text), "target" .= ("_blank" :: Text)]))))
      assertWith "a download" (not (matches delegatedClick (clickOn plain [] (link ["download" .= True] ["href" .= ("/x" :: Text), "download" .= ("" :: Text)]))))
    it "does not match without an element for the selector" $ do
      let bare = object (plain <> ["target" .= object ["tagName" .= ("P" :: Text)], "currentTarget" .= object ["tagName" .= ("DIV" :: Text)]])
          other = object (plain <> ["target" .= object ["tagName" .= ("SPAN" :: Text), "closest" .= object ["button" .= found]]])
          nullFound = object (plain <> ["target" .= object ["closest" .= object ["a[href]" .= Null]]])
      assertWith "a click elsewhere" (not (matches delegatedClick bare))
      assertWith "an element for another selector" (not (matches delegatedClick other))
      assertWith "null for the selector" (not (matches delegatedClick nullFound))
      assertWith "no target at all" (not (matches delegatedClick (object plain)))
      -- With no match, a click on the container is judged on the
      -- container, as before.
      assertWith "the container itself" (isPlainLeftClick (N.event (clickOn plain [] found)))
    it "looks for a nested match's element too" $ do
      let nested = Within "article" (Within "a[href]" PlainLeftClick)
          both = object (plain <> ["target" .= object ["closest" .= object ["article" .= object ["tagName" .= ("ARTICLE" :: Text)], "a[href]" .= found]]])
      assertWith "both found" (matches nested both)
      assertWith "the inner one alone" (not (matches nested (clickOn plain [] found)))

  describe "a match on what an event carries, from the event's JSON" $ do
    let transfer name types = object [name .= object ["types" .= (types :: [Text])]]
        matches m = matchesEvent m . N.event
    it "matches a paste or a drag carrying one of the types" $ do
      assertWith "a paste of files" (matches (Carrying ["Files"]) (transfer "clipboardData" ["Files"]))
      assertWith "a drag of files" (matches (Carrying ["Files"]) (transfer "dataTransfer" ["text/plain", "Files"]))
      assertWith "one of several" (matches (Carrying ["text/uri-list", "text/plain"]) (transfer "clipboardData" ["text/plain"]))
    it "does not match one carrying other types, or nothing" $ do
      assertWith "a paste of text" (not (matches (Carrying ["Files"]) (transfer "clipboardData" ["text/plain"])))
      assertWith "no types" (not (matches (Carrying ["Files"]) (object ["clipboardData" .= object []])))
      assertWith "no transfer" (not (matches (Carrying ["Files"]) (object ["type" .= ("paste" :: Text)])))
      assertWith "an empty list" (not (matches (Carrying []) (transfer "clipboardData" ["Files"])))

  describe "the reads a handler makes in the listener" $ do
    it "read the target, the element found for a selector and its attributes" $ do
      let event = N.event (clickOn plain [] (link ["id" .= ("ref-1" :: Text)] ["href" .= ("/p/1" :: Text), "class" .= ("footnote-ref" :: Text)]))
          target = Event.target event >>= Element.fromEventTarget
          anchor = target >>= (`Element.closestNow` "a[href]")
      assertWith "the target is an element" (isJust' target)
      assertEqual "its href" (Just "/p/1") (anchor >>= (`Element.getAttributeNow` "href"))
      assertEqual "its class" (Just "footnote-ref") (anchor >>= (`Element.getAttributeNow` "class"))
      assertEqual "an attribute it does not have" Nothing (anchor >>= (`Element.getAttributeNow` "download"))
      assertWith "a selector the event does not have" (not (isJust' (target >>= (`Element.closestNow` "sup > a"))))
      assertWith "the link's own closest, which nothing sent" (not (isJust' (anchor >>= (`Element.closestNow` "a.heading-anchor"))))
    it "take a target with no tagName for no element" $ do
      let textNode = N.event (object ["target" .= object ["nodeType" .= (3 :: Int)]])
      assertWith "not an element" (not (isJust' (Event.target textNode >>= Element.fromEventTarget)))
      assertWith "no target" (not (isJust' (Event.target (N.event (object [])))))
    it "read whether the default was prevented" $ do
      assertWith "prevented" (Event.defaultPreventedNow (N.event (object ["defaultPrevented" .= True])))
      assertWith "not said" (not (Event.defaultPreventedNow (N.event (object []))))
    it "read a transfer's types, files and text" $ do
      let pasted =
            N.event $
              object
                [ "clipboardData"
                    .= object
                      [ "types" .= ["Files", "text/plain" :: Text]
                      , "files" .= [object ["name" .= ("a.png" :: Text), "type" .= ("image/png" :: Text), "size" .= (1024 :: Int)]]
                      , "data" .= object ["text/plain" .= ("hello" :: Text)]
                      ]
                ]
      case CE.clipboardData (CE.ClipboardEvent pasted) of
        Nothing -> assertWith "the paste carries its data" False
        Just carried -> do
          assertEqual "types" ["Files", "text/plain"] (DataTransfer.types carried)
          let files = FileList.now carried.files
          assertEqual "names" ["a.png"] (map File.name files)
          assertEqual "types of the files" ["image/png"] (map File.type_ files)
          assertEqual "sizes" [1024] (map File.size files)
          assertEqual "length" 1 =<< FileList.length carried.files
          assertEqual "an item" ["a.png"] . map File.name . maybe [] pure =<< FileList.item carried.files 0
          assertEqual "past the end" [] . map File.name . maybe [] pure =<< FileList.item carried.files 1
          assertEqual "the text" "hello" =<< DataTransfer.getData carried "text/plain"
          assertEqual "the text by its old name" "hello" =<< DataTransfer.getData carried "text"
          assertEqual "a format it does not have" "" =<< DataTransfer.getData carried "text/html"
          -- A file's bytes do not cross: reading them fails, as natively
          -- for any promise.
          case files of
            file : _ -> do
              outcome <- waitCatch =<< File.text file
              case outcome of
                Left e -> assertEqual "why" "NotInBrowserError" (jsErrorName e)
                Right _ -> assertWith "a crossed file's bytes are not there" False
            [] -> assertWith "a file" False
      let dropped = N.event (object ["dataTransfer" .= object ["types" .= ["Files" :: Text]]])
      case DE.dataTransfer (DE.DragEvent dropped) of
        Nothing -> assertWith "the drop carries its data" False
        Just carried -> do
          assertEqual "types" ["Files"] (DataTransfer.types carried)
          assertEqual "no files listed" [] (map File.name (FileList.now carried.files))
          assertEqual "no text" "" =<< DataTransfer.getData carried "text/plain"
      assertWith "an event with neither" (not (isJust' (CE.clipboardData (CE.ClipboardEvent (N.event (object []))))))

  describe "a delegated handler on the in-memory DOM" $ do
    it "makes its action from the link the event says was found" $ do
      container <- N.newElement Nothing (ElemName "div")
      H.HalogenSocket {H.dispose = dispose} <- runMemDOM (runUI delegated () (N.fromNative container))
      children <- concat <$> (traverse N.childNodes =<< N.childNodes container)
      case children of
        [links, paragraph] -> do
          fire "click" (clickOn plain [] (link [] ["href" .= ("/p/1" :: Text)])) links
          N.renderToText paragraph >>= assertEqual "followed" "<p>/p/1</p>"
          fire "click" (clickOn (plain <> ["metaKey" .= True]) [] (link [] ["href" .= ("/p/2" :: Text)])) links
          fire "click" (clickOn plain [] (link ["target" .= ("_blank" :: Text)] ["href" .= ("/p/3" :: Text)])) links
          fire "click" (object (plain <> ["target" .= object ["tagName" .= ("P" :: Text)]])) links
          N.renderToText paragraph >>= assertEqual "nothing else followed" "<p>/p/1</p>"
        _ -> assertWith "the component renders a container and a paragraph" False
      runMemDOM dispose
    it "takes a paste of files and leaves one of text" $ do
      container <- N.newElement Nothing (ElemName "div")
      H.HalogenSocket {H.dispose = dispose} <- runMemDOM (runUI pasting () (N.fromNative container))
      children <- concat <$> (traverse N.childNodes =<< N.childNodes container)
      case children of
        [area, paragraph] -> do
          fire "paste" (object ["clipboardData" .= object ["types" .= ["text/plain" :: Text], "data" .= object ["text/plain" .= ("words" :: Text)]]]) area
          N.renderToText paragraph >>= assertEqual "a paste of text" "<p></p>"
          fire "paste" (object ["clipboardData" .= object ["types" .= ["Files" :: Text], "files" .= [object ["name" .= ("a.txt" :: Text), "type" .= ("text/plain" :: Text), "size" .= (3 :: Int)]]]]) area
          N.renderToText paragraph >>= assertEqual "a paste of a file" "<p>a.txt,text/plain,3.0</p>"
        _ -> assertWith "the component renders a container and a paragraph" False
      runMemDOM dispose
  where
    isJust' :: Maybe a -> Bool
    isJust' = maybe False (const True)
    jsErrorName :: JSError -> Text
    jsErrorName e = e.name

#endif
