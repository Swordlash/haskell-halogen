{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

-- | What a page does to custom elements, as the suite does it
-- (test/jsbits/elements.js): elements are named by their id, and what
-- crosses is JSON, as it would for a page's own JavaScript.
module Test.Page
  ( setAttribute
  , setProperty
  , propertyJSON
  , call
  , setThenCall
  , hasMethod
  , move
  , remove
  , create
  , shadowText
  , shadowColour
  , shadowClick
  , lightHTML
  , Heard (..)
  , listen
  , heard
  , dispatch
  , defineElement
  , undefine
  )
where

import Data.Aeson (FromJSON, ToJSON, Value, eitherDecodeStrictText, encode)
import Data.ByteString.Lazy qualified as Lazy
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Halogen qualified as H
import Halogen.CustomElement (Definition, Registration (..))
import Halogen.CustomElement qualified as CE
import Halogen.JSBits (JSText, Safety (..), browserJS, fromJSText, jsValText, toJSText)
import Halogen.JSBits.Promise (JSError, Promise (..), await)
import Halogen.VDom.DOM.Monad (MonadBrowserDOM)
import Test.Hspec.Halogen (MonadBrowserTest (..), PageM, settle, unsafeIOToPageM)

$( browserJS
     ["test/jsbits/elements.js"]
     [ ("js_set_attribute", "halogen_ce_test_set_attribute", Unsafe, [t|JSText -> JSText -> JSText -> IO ()|])
     , ("js_set_property", "halogen_ce_test_set_property", Unsafe, [t|JSText -> JSText -> JSText -> IO ()|])
     , ("js_get_property", "halogen_ce_test_get_property", Unsafe, [t|JSText -> JSText -> IO JSText|])
     , ("js_call", "halogen_ce_test_call", Unsafe, [t|JSText -> JSText -> JSText -> IO Promise|])
     , ("js_set_then_call", "halogen_ce_test_set_then_call", Unsafe, [t|JSText -> JSText -> JSText -> JSText -> IO Promise|])
     , ("js_has_method", "halogen_ce_test_has_method", Unsafe, [t|JSText -> JSText -> IO Bool|])
     , ("js_move", "halogen_ce_test_move", Unsafe, [t|JSText -> JSText -> IO ()|])
     , ("js_remove", "halogen_ce_test_remove", Unsafe, [t|JSText -> IO ()|])
     , ("js_create", "halogen_ce_test_create", Unsafe, [t|JSText -> JSText -> JSText -> JSText -> JSText -> IO ()|])
     , ("js_shadow_text", "halogen_ce_test_shadow_text", Unsafe, [t|JSText -> IO JSText|])
     , ("js_shadow_colour", "halogen_ce_test_shadow_colour", Unsafe, [t|JSText -> JSText -> IO JSText|])
     , ("js_shadow_click", "halogen_ce_test_shadow_click", Unsafe, [t|JSText -> JSText -> IO ()|])
     , ("js_light_html", "halogen_ce_test_light_html", Unsafe, [t|JSText -> IO JSText|])
     , ("js_listen", "halogen_ce_test_listen", Unsafe, [t|JSText -> IO ()|])
     , ("js_heard", "halogen_ce_test_heard", Unsafe, [t|JSText -> IO JSText|])
     , ("js_dispatch", "halogen_ce_test_dispatch", Unsafe, [t|JSText -> JSText -> JSText -> IO ()|])
     ]
 )

setAttribute :: Text -> Text -> Text -> IO ()
setAttribute element name value = js_set_attribute (toJSText element) (toJSText name) (toJSText value)

-- | Set a property to a value JSON can carry, as a framework sets one.
setProperty :: (ToJSON a) => Text -> Text -> a -> IO ()
setProperty element name value = js_set_property (toJSText element) (toJSText name) (toJSText (json value))

-- | A property as JSON (@undefined@ for none).
propertyJSON :: Text -> Text -> IO Text
propertyJSON element name = fromJSText <$> js_get_property (toJSText element) (toJSText name)

-- | Call the element's method with these arguments, and wait for what its
-- promise gives, as JSON.
call :: Text -> Text -> [Value] -> IO (Either JSError Text)
call element name args = do
  promise <- js_call (toJSText element) (toJSText name) (toJSText (json args))
  fmap jsValText <$> await promise

-- | Set an attribute and call a method of no arguments in the same
-- JavaScript task, as a page's script would, and wait for the answer.
setThenCall :: Text -> Text -> Text -> Text -> IO (Either JSError Text)
setThenCall element attribute value name = do
  promise <- js_set_then_call (toJSText element) (toJSText attribute) (toJSText value) (toJSText name)
  fmap jsValText <$> await promise

hasMethod :: Text -> Text -> IO Bool
hasMethod element name = js_has_method (toJSText element) (toJSText name)

-- | Move the element to the end of another, in one call, as appendChild does.
move :: Text -> Text -> IO ()
move element to = js_move (toJSText element) (toJSText to)

remove :: Text -> IO ()
remove = js_remove . toJSText

-- | Make an element of a name from JavaScript, with an id and a property,
-- and put it into another.
create :: (ToJSON a) => Text -> Text -> Text -> Text -> a -> IO ()
create into tag element property value = js_create (toJSText into) (toJSText tag) (toJSText element) (toJSText property) (toJSText (json value))

shadowText :: Text -> IO Text
shadowText = fmap fromJSText . js_shadow_text . toJSText

-- | The computed colour of the first element in the shadow root matching
-- the selector.
shadowColour :: Text -> Text -> IO Text
shadowColour element selector = fromJSText <$> js_shadow_colour (toJSText element) (toJSText selector)

-- | Click, with the element's own @click()@, the first element in the
-- shadow root matching the selector, which the runner's selectors do not
-- reach.
shadowClick :: Text -> Text -> IO ()
shadowClick element selector = js_shadow_click (toJSText element) (toJSText selector)

-- | What the element holds outside its shadow root.
lightHTML :: Text -> IO Text
lightHTML = fmap fromJSText . js_light_html . toJSText

-- | An event the document heard.
data Heard = Heard {detail :: Value, bubbles :: Bool, composed :: Bool, target :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | Listen to the event on the document, from now on.
listen :: Text -> IO ()
listen = js_listen . toJSText

heard :: Text -> IO [Heard]
heard name = do
  text <- fromJSText <$> js_heard (toJSText name)
  either (fail . ("what the document heard does not decode: " <>)) pure (eitherDecodeStrictText text)

-- | Dispatch a bubbling @CustomEvent@ with this detail on the element.
dispatch :: (ToJSON a) => Text -> Text -> a -> IO ()
dispatch element name detail = js_dispatch (toJSText element) (toJSText name) (toJSText (json detail))

-- | Define an element in the monad the suite runs its components in.
defineElement :: forall m -> (MonadBrowserTest m, MonadBrowserDOM m) => Text -> Definition q i o -> H.Component q i o m -> PageM s (Registration m)
defineElement _ name definition component = unsafeIOToPageM (runTest (CE.define name definition component)) <* settle

-- | Take the definition back, as a test does when it is over: a definition
-- outlives the test's page, as it outlives everything on a real one.
undefine :: (MonadBrowserTest m) => Registration m -> PageM s ()
undefine registration = unsafeIOToPageM (runTest registration.dispose) >> settle

json :: (ToJSON a) => a -> Text
json = decodeUtf8 . Lazy.toStrict . encode
