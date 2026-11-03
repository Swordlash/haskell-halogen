{-# LANGUAGE TemplateHaskell #-}

-- | Fields of the Web types ("Halogen.JSBits.Fields"), in a real browser:
-- one test for each kind of member (fixed, live, writable, a method, a
-- method whose Promise is awaited), a rejected Promise, an exception thrown
-- by a method, and checked casts,
-- one that holds and ones that do not; then the window itself, the pure
-- reads a listener makes, elements made and put in the page, the
-- selection and the caches. Natively each field gives its inert answer,
-- which core's suite tests (Test.WebFields).
module Test.Fields (spec) where

import Data.MediaType (MediaType (..))
import Data.Text qualified as T
import Halogen.JSBits (JSText, toJSText)
import Halogen.JSBits.Fields (inlineJS)
import Halogen.JSBits.Promise (JSError (..), wait, waitCatch)
import Halogen.VDom.DOM.Monad (MonadBrowserDOM, window)
import Protolude hiding (wait, waitCatch)
import Test.Hspec.Halogen
import Web.DOM.Element qualified as Element
import Web.DOM.Internal.Types (Window, toNode)
import Web.DOM.Internal.Types qualified as DOM
import Web.Event.CustomEvent qualified as CustomEvent
import Web.Event.Event (EventType (..))
import Web.Event.Event qualified as Event
import Web.Event.Internal.Types (Event (..))
import Web.File.Blob qualified as Blob
import Web.File.File (File (..))
import Web.File.FileList (FileList (..))
import Web.File.FileList qualified as FileList
import Web.HTML.HTMLDialogElement qualified as HTMLDialogElement
import Web.HTML.HTMLDocument qualified as HTMLDocument
import Web.HTML.HTMLElement qualified as HTMLElement
import Web.HTML.HTMLTextAreaElement qualified as HTMLTextAreaElement
import Web.HTML.History qualified as History
import Web.HTML.Window qualified as Window
import Web.UIEvent.FocusEvent qualified as FE
import Web.UIEvent.KeyboardEvent qualified as KE
import Web.UIEvent.MouseEvent qualified as ME
import Web.UIEvent.TouchEvent qualified as TE
import Web.URL qualified as URL

-- Values only a page can make: a file, and events that nobody dispatches.
$(inlineJS "js_new_file" "new File([$1], $2, { type: $3 })" [t|JSText -> JSText -> JSText -> IO File|])
$(inlineJS "js_new_event" "new Event($1)" [t|JSText -> IO Event|])
$(inlineJS "js_new_keydown" "new KeyboardEvent(\"keydown\", { key: $1, shiftKey: true })" [t|JSText -> IO Event|])
$(inlineJS "js_new_custom_event" "new CustomEvent(\"fields-test\", { detail: $1 })" [t|JSText -> IO Event|])

-- A FileList, which only a drop, a paste or a file input makes, through a
-- DataTransfer of the test's own; a touch; and a change of focus.
$(inlineJS "js_new_file_list" "(() => { const transfer = new DataTransfer(); transfer.items.add($1); return transfer.files; })()" [t|File -> IO FileList|])

$(inlineJS "js_new_touchstart" "new TouchEvent(\"touchstart\", { changedTouches: [new Touch({ identifier: 1, target: $1, clientX: 5, clientY: 6 })] })" [t|DOM.Element -> IO Event|])

$(inlineJS "js_new_focusout" "new FocusEvent(\"focusout\", { relatedTarget: $1 })" [t|DOM.Element -> IO Event|])

spec :: forall m -> (MonadBrowserTest m, MonadBrowserDOM m) => Spec
spec m = do
  let inPage :: (Window -> IO a) -> PageM s a
      inPage f = unsafeIOToPageM (runTest @m window >>= f)
      newFile = js_new_file (toJSText "hello") (toJSText "notes.txt") (toJSText "text/plain")
      theBody :: PageM s DOM.Element
      theBody = inPage (\w -> w.document.querySelector "body") >>= maybe (panic "the page has no body") pure

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
    -- An entry replaced at the page's own address keeps the address.
    before <- inPage (\w -> w.location.href)
    inPage (\w -> w.history.state >>= \s -> w.history.replaceState s "" Nothing)
    inPage (\w -> w.location.href) `shouldReturn` before

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
    inPage (const (newFile >>= (.text) >>= wait)) `shouldReturn` "hello"
    inPage (const (newFile >>= (.bytes) >>= wait)) `shouldReturn` "hello"
    inPage (const (Blob.fromBytes "bytes" (MediaType "text/plain") >>= Blob.text >>= waitCatch)) `shouldReturn` Right "bytes"

  it "gives back the rejection of a Promise it awaits" $ runPage $ do
    -- Nothing is full screen, so leaving full screen is rejected.
    outcome <- inPage (\w -> w.document.exitFullscreen >>= waitCatch)
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

  it "gives the window, and the page in it" $ runPage $ do
    inPage (const (Window.window >>= \w -> w.document.baseURI)) >>= (`shouldSatisfy` (not . T.null))
    -- The head comes first in the document, with the body after it.
    inPage (\w -> isJust <$> Element.nextElementSibling (HTMLElement.toElement w.document.head)) `shouldReturn` True

  it "reads purely what a listener needs" $ runPage $ do
    body <- theBody
    isJust (Element.closestNow body "html") `shouldBe` True
    isJust (Element.closestNow body "table") `shouldBe` False
    inPage (const (body.setAttribute "data-fields" "yes"))
    Element.getAttributeNow body "data-fields" `shouldBe` Just "yes"
    Element.getAttributeNow body "data-absent" `shouldBe` Nothing
    taken <- inPage (const (newFile >>= js_new_file_list))
    map (.name) (FileList.now taken) `shouldBe` ["notes.txt"]
    touch <- inPage (const (js_new_touchstart body))
    let touchEvent = TE.TouchEvent touch
    touchEvent.changedTouches.length `shouldBe` 1
    ((.clientX) <$> TE.firstChangedTouch touchEvent) `shouldBe` Just 5
    focusout <- inPage (const (js_new_focusout body))
    isJust ((FE.FocusEvent focusout).relatedTarget) `shouldBe` True

  it "makes an element, puts it in the page, and dispatches an event to it" $ runPage $ do
    body <- theBody
    made <- inPage (\w -> w.document.createElement "textarea")
    inPage (const made.isConnected) `shouldReturn` False
    _ <- inPage (const (body.appendChild (toNode made)))
    inPage (const made.isConnected) `shouldReturn` True
    inPage (const (body.contains (toNode made))) `shouldReturn` True
    isJust (HTMLTextAreaElement.fromElement made) `shouldBe` True
    for_ (HTMLTextAreaElement.fromElement made) $ \area -> do
      inPage (const (area.value_set "hello"))
      inPage (const (area.selectionStart_set 2 >> area.selectionEnd_set 4))
      inPage (const area.selectionStart) `shouldReturn` 2
      inPage (const area.selectionEnd) `shouldReturn` 4
    for_ (HTMLElement.fromElement made) $ \element -> inPage (const (HTMLElement.focusWithoutScrolling element))
    inPage (\w -> isJust <$> w.document.activeElement) `shouldReturn` True
    inPage (const (Element.scrollIntoViewBlock made Element.ToNearest))
    event <- inPage (const (Event.newEvent (EventType "fields-made") False))
    Event.defaultPreventedNow event `shouldBe` False
    inPage (const ((Element.toEventTarget made).dispatchEvent event)) `shouldReturn` True
    inPage (const (made.remove >> made.isConnected)) `shouldReturn` False
    inPage (\w -> isJust <$> w.document.body) `shouldReturn` True
    -- Without a user gesture the browser refuses full screen.
    inPage (const (made.requestFullscreen >>= waitCatch)) >>= (`shouldSatisfy` isLeft)

  it "selects the page's contents, and looks at the caches" $ runPage $ do
    body <- theBody
    range <- inPage (\w -> w.document.createRange)
    inPage (const (range.selectNodeContents (toNode body)))
    selection <- inPage (\w -> w.getSelection)
    isJust selection `shouldBe` True
    for_ selection $ \s -> inPage (const (s.removeAllRanges >> s.addRange range))
    -- Copying may be refused; what matters is that the call answers.
    _ <- inPage (\w -> w.document.execCommand "copy")
    -- The runner's page is a secure context, so it has caches.
    caches <- inPage (\w -> w.caches)
    isJust caches `shouldBe` True
    for_ caches $ \c -> do
      (_ :: [Text]) <- inPage (const (c.keys >>= wait))
      inPage (const (c.delete "fields-test-no-such-cache" >>= wait)) `shouldReturn` False

  it "parses an address against a base, and refuses one that is none" $ runPage $ do
    inPage (const (URL.parse "b/c?d" (Just "https://example.com/a/") >>= traverse URL.pathname))
      `shouldReturn` Just "/a/b/c"
    inPage (const (isJust <$> URL.parse "not an address" Nothing)) `shouldReturn` False
