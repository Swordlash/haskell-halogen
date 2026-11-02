{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Natively the fields of the Web types ("Halogen.JSBits.Fields") have no
-- engine to ask, and each gives its inert answer: nothing done, 'False', 0,
-- empty, 'Nothing'. The events' fields that core reads from an event made
-- from JSON give what the JSON says, both as fields and as functions. In a
-- browser the same fields are tested for real, in hspec-halogen's suite.
module Test.WebFields (spec) where

import Prelude

#if !defined(HALOGEN_WASI) && (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "Web fields, natively" $ pure ()

#else

import Data.Aeson (object, (.=))
import Data.Foreign (toForeign)
import Data.MediaType (MediaType (..))
import Data.Text (Text)
import Data.Either (isLeft)
import Halogen.JSBits (Inert (..))
import Halogen.JSBits.Promise (await)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)
import Web.DOM.Element qualified as Element
import Web.DOM.Internal.Types (Element, HTMLDocument, Window)
import Web.Event.CustomEvent qualified as CustomEvent
import Web.Event.Event qualified as Event
import Web.Event.Internal.Types (Event (..), EventTarget)
import Web.File.Blob qualified as Blob
import Web.File.File (File)
import Web.File.File qualified as File
import Web.File.FileList (FileList)
import Web.File.FileList qualified as FileList
import Web.HTML.HTMLDialogElement qualified as HTMLDialogElement
import Web.HTML.HTMLDocument qualified as HTMLDocument
import Web.HTML.History qualified as History
import Web.HTML.Window qualified as Window
import Web.UIEvent.KeyboardEvent qualified as KE
import Web.UIEvent.MouseEvent qualified as ME
import Web.URL qualified as URL

spec :: Spec
spec = describe "Web fields, natively" $ do
  let w = inert :: Window
      doc = inert :: HTMLDocument
      el = inert :: Element
      ev = inert :: Event

  it "reads a fixed member as its inert value, and chains" $ do
    w.isSecureContext `shouldBe` False
    w.document.title `shouldReturn` ""
    (inert :: File).name `shouldBe` ""
    Window.isSecureContext w `shouldBe` False

  it "reads a live member as its inert value" $ do
    (inert :: FileList).length `shouldReturn` 0
    w.navigator.onLine `shouldReturn` False
    w.innerWidth `shouldReturn` 0
    w.history.length `shouldReturn` 0
    History.length w.history `shouldReturn` 0
    w.location.hash `shouldReturn` ""
    Window.locationHash w `shouldReturn` ""
    doc.fullscreenElement >>= (`shouldBe` True) . null

  it "writes a writable member to nowhere" $ do
    doc.title_set "Notes"
    HTMLDocument.setTitle doc "Notes"
    doc.title `shouldReturn` ""
    el.textContent_set "text"
    Element.textContent el `shouldReturn` ""

  it "calls a method and gets its inert answer" $ do
    w.crypto.randomUUID `shouldReturn` ""
    w.history.go (-1)
    doc.querySelector "body" >>= (`shouldBe` True) . null
    el.getAttribute "href" >>= (`shouldBe` (Nothing :: Maybe Text))
    el.hasAttribute "href" `shouldReturn` False
    (inert :: FileList).item 0 >>= (`shouldBe` True) . null
    FileList.items inert >>= (`shouldBe` 0) . length

  it "gives a promise that waits for nothing and resolves with nothing" $ do
    (doc.exitFullscreen >>= await) >>= (`shouldSatisfy` isLeft)
    ((inert :: File).bytes >>= await) >>= (`shouldSatisfy` isLeft)
    blob <- Blob.fromBytes "hello" (MediaType "text/plain")
    (blob.text >>= await) >>= (`shouldSatisfy` isLeft)
    (File.text inert >>= await) >>= (`shouldSatisfy` isLeft)

  it "casts nothing" $ do
    null (KE.fromEvent ev) `shouldBe` True
    null (ME.fromEvent ev) `shouldBe` True
    null (CustomEvent.fromEvent ev) `shouldBe` True
    null (HTMLDialogElement.fromElement el) `shouldBe` True
    null (Element.fromEventTarget (inert :: EventTarget)) `shouldBe` True

  it "parses no address" $ do
    URL.parse "https://example.com/a" Nothing >>= (`shouldBe` True) . null
    URL.parse "b" (Just "https://example.com/a") >>= (`shouldBe` True) . null
    URL.createObjectURL inert `shouldReturn` ""

  it "reads an event made from JSON through its fields and its functions alike" $ do
    let key = KE.KeyboardEvent (Event (toForeign (object ["key" .= ("Enter" :: Text), "shiftKey" .= True])))
        click = ME.MouseEvent (Event (toForeign (object ["clientX" .= (12 :: Int), "button" .= (2 :: Int), "ctrlKey" .= True])))
    key.key `shouldBe` "Enter"
    KE.key key `shouldBe` "Enter"
    key.shiftKey `shouldBe` True
    KE.ctrlKey key `shouldBe` False
    click.clientX `shouldBe` 12
    click.button `shouldBe` 2
    ME.ctrlKey click `shouldBe` True
    null (Event.target ev) `shouldBe` True

#endif
