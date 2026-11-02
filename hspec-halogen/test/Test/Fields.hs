{-# LANGUAGE TemplateHaskell #-}

-- | Fields of the Web types ("Halogen.JSBits.Fields"), in a real browser:
-- one test for each kind of member (fixed, live, writable, a method, a
-- method whose Promise is awaited), a rejected Promise, an exception thrown
-- by a method, and checked casts,
-- one that holds and ones that do not. Natively each field gives its inert
-- answer, which core's suite tests (Test.WebFields).
module Test.Fields (spec) where

import Data.MediaType (MediaType (..))
import Data.Text qualified as T
import Halogen.JSBits (JSText, toJSText)
import Halogen.JSBits.Fields (inlineJS)
import Halogen.JSBits.Promise (JSError (..))
import Halogen.VDom.DOM.Monad (MonadBrowserDOM, window)
import Protolude
import Test.Hspec.Halogen
import Web.DOM.Element qualified as Element
import Web.DOM.Internal.Types (Window)
import Web.Event.CustomEvent qualified as CustomEvent
import Web.Event.Internal.Types (Event (..))
import Web.File.Blob qualified as Blob
import Web.File.File (File (..))
import Web.HTML.HTMLDialogElement qualified as HTMLDialogElement
import Web.HTML.HTMLDocument qualified as HTMLDocument
import Web.HTML.History qualified as History
import Web.UIEvent.KeyboardEvent qualified as KE
import Web.UIEvent.MouseEvent qualified as ME
import Web.URL qualified as URL

-- Values only a page can make: a file, and events that nobody dispatches.
$(inlineJS "js_new_file" "new File([$1], $2, { type: $3 })" [t|JSText -> JSText -> JSText -> IO File|])
$(inlineJS "js_new_event" "new Event($1)" [t|JSText -> IO Event|])
$(inlineJS "js_new_keydown" "new KeyboardEvent(\"keydown\", { key: $1, shiftKey: true })" [t|JSText -> IO Event|])
$(inlineJS "js_new_custom_event" "new CustomEvent(\"fields-test\", { detail: $1 })" [t|JSText -> IO Event|])

spec :: forall m -> (MonadBrowserTest m, MonadBrowserDOM m) => Spec
spec m = do
  let inPage :: (Window -> IO a) -> PageM s a
      inPage f = unsafeIOToPageM (runTest @m window >>= f)
      newFile = js_new_file (toJSText "hello") (toJSText "notes.txt") (toJSText "text/plain")

  it "reads a fixed member purely, and chains fields" $ runPage $ do
    file <- inPage (const newFile)
    file.name `shouldBe` "notes.txt"
    file.type_ `shouldBe` "text/plain"
    file.size `shouldBe` 5
    -- The window's document, and its address, which a page always has.
    inPage (\w -> w.document.baseURI) >>= (`shouldSatisfy` (not . T.null))

  it "reads a live member each time" $ runPage $ do
    inPage (\w -> w.innerWidth) >>= (`shouldSatisfy` (> 0))
    inPage (\w -> History.length w.history) >>= (`shouldSatisfy` (>= 1))
    inPage (\w -> w.location.href) >>= (`shouldSatisfy` ("http" `T.isPrefixOf`))

  it "writes a writable member through its _set field" $ runPage $ do
    before <- inPage (\w -> w.document.title)
    inPage (\w -> w.document.title_set "Fields")
    inPage (\w -> w.document.title) `shouldReturn` "Fields"
    inPage (\w -> HTMLDocument.setTitle w.document "Fields again")
    inPage (\w -> HTMLDocument.title w.document) `shouldReturn` "Fields again"
    inPage (\w -> w.document.title_set before)

  it "calls a method" $ runPage $ do
    inPage (\w -> isJust <$> w.document.querySelector "body") `shouldReturn` True
    inPage (\w -> isJust <$> w.document.getElementById "no-such-element-here") `shouldReturn` False
    inPage (\w -> w.document.querySelector "body" >>= traverse (`Element.hasAttribute` "no-such-attribute"))
      `shouldReturn` Just False
    -- randomUUID exists only in a secure context, which the runner's page
    -- on 127.0.0.1 is.
    inPage (\w -> if w.isSecureContext then T.length <$> w.crypto.randomUUID else pure 36) `shouldReturn` 36

  it "awaits the Promise a method returns" $ runPage $ do
    inPage (const (newFile >>= (.text))) `shouldReturn` "hello"
    inPage (const (newFile >>= (.bytes))) `shouldReturn` "hello"
    inPage (const (Blob.fromBytes "bytes" (MediaType "text/plain") >>= Blob.text)) `shouldReturn` "bytes"

  it "throws the rejection of a Promise it awaits" $ runPage $ do
    -- Nothing is full screen, so leaving full screen is rejected.
    outcome <- inPage (\w -> try @JSError w.document.exitFullscreen)
    outcome `shouldSatisfy` isLeft

  it "throws a JavaScript exception out of a method as a JSError" $ runPage $ do
    -- A selector that does not parse is a SyntaxError, which on WebAssembly
    -- would end the program if the field did not catch it.
    outcome <- inPage (\w -> try @JSError (w.document.querySelector "::"))
    either (.name) (const "no exception") outcome `shouldBe` "SyntaxError"
    -- The program goes on, and the next call answers as usual.
    inPage (\w -> isJust <$> w.document.querySelector "body") `shouldReturn` True

  it "casts an event or an element only to what it is" $ runPage $ do
    plain <- inPage (const (js_new_event (toJSText "fields-test")))
    isJust (KE.fromEvent plain) `shouldBe` False
    isJust (ME.fromEvent plain) `shouldBe` False
    isJust (CustomEvent.fromEvent plain) `shouldBe` False
    keydown <- inPage (const (js_new_keydown (toJSText "a")))
    ((.key) <$> KE.fromEvent keydown) `shouldBe` Just "a"
    ((.shiftKey) <$> KE.fromEvent keydown) `shouldBe` Just True
    custom <- inPage (const (js_new_custom_event (toJSText "detail")))
    isJust (CustomEvent.fromEvent custom) `shouldBe` True
    body <- inPage (\w -> w.document.querySelector "body")
    (isJust . HTMLDialogElement.fromElement <$> body) `shouldBe` Just False

  it "parses an address against a base, and refuses one that is none" $ runPage $ do
    inPage (const (URL.parse "b/c?d" (Just "https://example.com/a/") >>= traverse URL.pathname))
      `shouldReturn` Just "/a/b/c"
    inPage (const (isJust <$> URL.parse "not an address" Nothing)) `shouldReturn` False
