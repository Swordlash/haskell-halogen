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
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key (Key)
import Data.Foldable (for_)
import Data.Foreign (foreignToBool, foreignToInt, foreignToNumber, foreignToString, readProp, stringToForeign, toForeign, unsafeFromForeign)
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
import Halogen.VDom.Types (ElemName (..))
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)
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

#endif
