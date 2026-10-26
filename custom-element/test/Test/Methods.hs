-- | An element's methods: each sends the component a query, and its
-- promise resolves with the answer as JSON.
module Test.Methods (spec) where

import Control.Applicative ((<|>))
import Data.Aeson (toJSON)
import Data.IORef (newIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Example.Counter qualified as Counter
import Halogen.CustomElement (Definition (..), JSError (..))
import Halogen.CustomElement qualified as CE
import Halogen.VDom.DOM.Monad (MonadBrowserDOM)
import Test.Hspec.Halogen
import Test.Page

spec :: forall m -> (MonadBrowserTest m, MonadBrowserDOM m) => Spec
spec m = do
  it "sends a request and resolves with the component's answer" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    registration <- defineElement m "x-ce-request" Counter.definition (Counter.counter finalized)
    page <- htmlContainer "<x-ce-request id=\"request\"></x-ce-request>"
    button <- findIn page "button.increment"
    click button
    click button
    unsafeIOToPageM (call "request" "count" []) `shouldReturn` Right "2"
    undefine registration

  it "resolves with null when the component does not answer" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    registration <- defineElement m "x-ce-refuse" Counter.definition (Counter.counter finalized)
    _ <- htmlContainer "<x-ce-refuse id=\"refuse\"></x-ce-refuse>"
    unsafeIOToPageM (call "refuse" "refuse" []) `shouldReturn` Right "null"
    undefine registration

  it "tells the component, with the method's argument, and resolves with null" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    registration <- defineElement m "x-ce-tell" Counter.definition (Counter.counter finalized)
    page <- htmlContainer "<x-ce-tell id=\"tell\"></x-ce-tell>"
    unsafeIOToPageM (call "tell" "add" [toJSON (5 :: Int)]) `shouldReturn` Right "null"
    findIn page ".count" >>= (`shouldHaveText` "5")
    unsafeIOToPageM (call "tell" "reset" []) `shouldReturn` Right "null"
    findIn page ".count" >>= (`shouldHaveText` "0")
    undefine registration

  -- An attribute the first definition did not observe is watched with a
  -- MutationObserver, which delivers later than attributeChangedCallback;
  -- a method called in the same task must still see the change.
  it "answers a method with an attribute set in the same task, for a name a later definition observes" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    first <- defineElement m "x-ce-same-task" Counter.definition (Counter.counter finalized)
    page <- htmlContainer "<x-ce-same-task id=\"same-task\" label=\"Apples\"></x-ce-same-task>"
    findIn page ".label" >>= (`shouldHaveText` "Apples")
    let wider = Counter.definition {input = \props -> fromMaybe "none" (CE.attr "title-label" props <|> CE.attr "label" props), observed = ["label", "title-label"]}
    second <- defineElement m "x-ce-same-task" wider (Counter.counter finalized)
    unsafeIOToPageM (setThenCall "same-task" "title-label" "Pears" "currentLabel") `shouldReturn` Right "\"Pears\""
    findIn page ".label" >>= (`shouldHaveText` "Pears")
    undefine first
    undefine second

  it "rejects arguments that do not decode" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    registration <- defineElement m "x-ce-arguments" Counter.definition (Counter.counter finalized)
    _ <- htmlContainer "<x-ce-arguments id=\"arguments\"></x-ce-arguments>"
    unsafeIOToPageM (call "arguments" "add" [toJSON ("five" :: Text)]) >>= \case
      Left refused -> refused.message `shouldSatisfy` Text.isInfixOf "do not decode"
      Right answer -> expectationFailure ("add(\"five\") resolved with " <> answer)
    undefine registration
