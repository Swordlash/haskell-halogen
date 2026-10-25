-- | Defining an element: its component renders in every element of the
-- name, whether the element was in the page first or came after, and hears
-- what the page sets on the element as its input.
module Test.Define (spec) where

import Control.Exception (SomeException, try)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
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
  it "renders its component in an element inserted with innerHTML" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    registration <- defineElement m "x-ce-renders" Counter.definition (Counter.counter finalized)
    page <- htmlContainer "<x-ce-renders label=\"Apples\"></x-ce-renders>"
    findIn page ".label" >>= (`shouldHaveText` "Apples")
    findIn page ".count" >>= (`shouldHaveText` "0")
    undefine registration

  it "renders in the elements already in the page when it is defined" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    page <- htmlContainer "<x-ce-upgrades label=\"first\"></x-ce-upgrades><x-ce-upgrades label=\"second\"></x-ce-upgrades>"
    registration <- defineElement m "x-ce-upgrades" Counter.definition (Counter.counter finalized)
    labels <- findAllIn page ".label"
    traverse textContent labels `shouldReturn` ["first", "second"]
    undefine registration

  it "hears an observed attribute's change as its new input, keeping its state" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    registration <- defineElement m "x-ce-attribute" Counter.definition (Counter.counter finalized)
    page <- htmlContainer "<x-ce-attribute id=\"attribute\" label=\"Apples\"></x-ce-attribute>"
    findIn page "button.increment" >>= click
    unsafeIOToPageM (setAttribute "attribute" "label" "Pears")
    findIn page ".label" >>= (`shouldHaveText` "Pears")
    findIn page ".count" >>= (`shouldHaveText` "1")
    undefine registration

  it "hears an observed property set from JavaScript as its new input" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    registration <- defineElement m "x-ce-property" Counter.definition (Counter.counter finalized)
    page <- htmlContainer "<x-ce-property id=\"property\" label=\"Apples\"></x-ce-property>"
    findIn page ".label" >>= (`shouldHaveText` "Apples")
    unsafeIOToPageM (setProperty "property" "label" ("Plums" :: Text))
    findIn page ".label" >>= (`shouldHaveText` "Plums")
    -- The property reads back as set, and the attribute is left as it was.
    unsafeIOToPageM (propertyJSON "property" "label") `shouldReturn` "\"Plums\""
    findIn page "#property" >>= (`getAttribute` "label") >>= (`shouldBe` Just "Apples")
    undefine registration

  it "takes over a property a page set before the name was defined" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    page <- htmlContainer "<div id=\"holder\"></div>"
    unsafeIOToPageM (create "holder" "x-ce-early" "early" "label" ("Early" :: Text))
    registration <- defineElement m "x-ce-early" Counter.definition (Counter.counter finalized)
    findIn page ".label" >>= (`shouldHaveText` "Early")
    unsafeIOToPageM (setProperty "early" "label" ("Later" :: Text))
    findIn page ".label" >>= (`shouldHaveText` "Later")
    undefine registration

  it "renders into an open shadow root, with its own styles" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    let shadowed = Counter.definition {shadow = CE.OpenShadow, styles = ".label { color: rgb(1, 2, 3); }"}
    registration <- defineElement m "x-ce-shadow" shadowed (Counter.counter finalized)
    _ <- htmlContainer "<x-ce-shadow id=\"shadow\" label=\"Apples\"></x-ce-shadow>"
    eventually (unsafeIOToPageM (shadowText "shadow") `shouldReturn` "Apples+0")
    unsafeIOToPageM (shadowColour "shadow" ".label") `shouldReturn` "rgb(1, 2, 3)"
    unsafeIOToPageM (lightHTML "shadow") `shouldReturn` ""
    unsafeIOToPageM (shadowClick "shadow" "button.increment")
    eventually (unsafeIOToPageM (shadowText "shadow") `shouldReturn` "Apples+1")
    undefine registration

  it "leaves nothing behind when an element already on the page fails to mount" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    page <- htmlContainer "<x-ce-fragile id=\"sound\" label=\"sound\"></x-ce-fragile><x-ce-fragile id=\"broken\" label=\"boom\"></x-ce-fragile>"
    outcome <- unsafeIOToPageM (try (runTest (CE.define "x-ce-fragile" Counter.definition (Counter.counter finalized) :: m (CE.Registration m))))
    case outcome of
      Left (failure :: SomeException) -> show failure `shouldSatisfy` isInfixOf "boom"
      Right _ -> expectationFailure "defined x-ce-fragile although one of its elements failed to mount"
    -- The element mounted before the failure was finalized, and the name
    -- is inert: a new element renders nothing and answers no method.
    settle
    unsafeIOToPageM (readIORef finalized) `shouldReturn` 1
    length <$> findAllIn page ".count" `shouldReturn` 0
    later <- htmlContainer "<x-ce-fragile id=\"inert-fragile\" label=\"later\"></x-ce-fragile>"
    settle
    length <$> findAllIn later ".count" `shouldReturn` 0
    unsafeIOToPageM (call "inert-fragile" "count" []) `shouldReturn` Right "null"

  it "throws for a name without a hyphen, as the browser would" $ runPage $ do
    finalized <- unsafeIOToPageM (newIORef 0)
    outcome <- unsafeIOToPageM (try (runTest (CE.define "counter" Counter.definition (Counter.counter finalized) :: m (CE.Registration m))))
    case outcome of
      Left (refused :: JSError) -> do
        refused.name `shouldBe` "SyntaxError"
        refused.message `shouldSatisfy` Text.isInfixOf "\"counter\""
      Right _ -> expectationFailure "defined counter"
