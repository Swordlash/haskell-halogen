{-# LANGUAGE CPP #-}

-- | Natively a value can answer for itself when the program uses a member
-- of it through the @Web.*@ types ("Halogen.JSBits.Fields"): a call of a
-- method that returns nothing, an assignment, an object it has, a checked
-- cast. A value that does not answer has the inert answers, as before. The
-- in-memory DOM's handles answer by noting what was done to their node, so
-- a test sees that a component focused an element or opened a dialog.
module Test.NativeAnswers (spec) where

import Prelude

#if !defined(HALOGEN_WASI) && (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "values that answer for themselves, natively" $ pure ()

#else

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (get)
import Control.Monad.UUID (MonadUUID)
import Data.Coerce (coerce)
import Data.Foldable (for_, traverse_)
import Data.Foreign (foreignToBool, foreignToInt, foreignToString, readProp)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust, isNothing)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (JSVal, NativeObject (..), withNativeObject, jsValText, toJSInt)
import Halogen.VDom.DOM.Monad (MonadBrowserDOM, runMemDOM)
import Halogen.VDom.DOM.Monad qualified as DOM
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.Driver qualified as VD
import Halogen.VDom.Types (ElemName (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Web.DOM.Element qualified as Element
import Web.DOM.Internal.Types (Element (..), HTMLDocument (..), HTMLElement (..), Window (..))
import Web.HTML.HTMLDialogElement qualified as HTMLDialogElement
import Web.HTML.HTMLElement qualified as HTMLElement
import Web.HTML.HTMLInputElement qualified as HTMLInputElement

-- | What a test's object was asked to do.
data Asked = Call Text [JSVal] | Set Text JSVal

-- | A value holding 7 that answers for itself as an element of the tag
-- given, noting what it was asked; its document is a value holding 8.
recording :: Text -> IO (JSVal, IORef [Asked])
recording klass = do
  asked <- newIORef []
  let note a = atomicModifyIORef' asked (\as -> (as <> [a], ()))
      object =
        NativeObject
          { call = \name arguments -> note (Call name arguments)
          , set = \name value -> note (Set name value)
          , get = \name -> if name == "document" then Just (toJSInt 8) else Nothing
          , instanceOf = (`elem` ["Element", "HTMLElement", klass])
          }
  pure (withNativeObject object (toJSInt 7), asked)

names :: [Asked] -> [Text]
names = map $ \case
  Call name _ -> "call " <> name
  Set name value -> "set " <> name <> " " <> jsValText value

data Focus = Focus
  { field :: H.RefLabel
  , dialog :: H.RefLabel
  }

data Act a = Open a | Shut a

-- | A field it focuses in its @Initialize@, and a dialog it opens and
-- closes on its actions, through refs, as a component in a page does.
focusing :: forall m. (MonadIO m, MonadUUID m, MonadBrowserDOM m) => H.Component Act () Void m
focusing =
  H.component
    (\() -> Focus <$> H.newRefLabel "field" <*> H.newRefLabel "dialog")
    ( \s ->
        HH.div_
          [ HH.input [HP.ref s.field]
          , HH.element (ElemName "dialog") [HP.ref s.dialog] [HH.text "A dialog"]
          ]
          :: H.ComponentHTML () Empty m
    )
    [ H.onInitialize $ do
        s <- get
        H.getHTMLElementRef s.field >>= traverse_ (H.liftEffect . liftIO . HTMLElement.focus)
        doc <- H.liftEffect (DOM.window >>= DOM.document)
        H.liftEffect (liftIO (doc.title_set "Focused"))
    , H.onQuery $ \act -> do
        s <- get
        dialog <- (>>= HTMLDialogElement.fromElement) <$> H.getRef s.dialog
        for_ dialog $ \d -> H.liftEffect . liftIO $ case act of
          Open _ -> d.showModal
          Shut _ -> HTMLDialogElement.close d
        pure (Just (case act of Open a -> a; Shut a -> a))
    ]

spec :: Spec
spec = describe "values that answer for themselves, natively" $ do
  it "leaves an ordinary value's members inert" $ do
    let plain = Element (toJSInt 7)
    plain.scrollIntoView
    (coerce plain :: HTMLElement).focus
    isNothing (HTMLElement.fromElement plain) `shouldBe` True
    isNothing (HTMLDialogElement.fromElement plain) `shouldBe` True
    (coerce plain :: HTMLDocument).title_set "Notes"
    foreignToInt (coerce (coerce plain :: Window).document :: JSVal) `shouldBe` 0

  it "asks a value that answers for itself to call, set, give and cast" $ do
    (value, asked) <- recording "HTMLDialogElement"
    let element = Element value
        html = coerce element :: HTMLElement
    -- What it holds is read as before.
    foreignToInt value `shouldBe` 7
    element.scrollIntoView
    html.focus
    HTMLElement.blur html
    Element.scrollIntoViewBlock element Element.ToCenter
    HTMLElement.focusWithoutScrolling html
    (coerce element :: HTMLDocument).title_set "Notes"
    for_ (HTMLDialogElement.fromElement element) $ \d -> d.showModal >> HTMLDialogElement.close d
    readIORef asked >>= (`shouldBe` ["call scrollIntoView", "call focus", "call blur", "call scrollIntoView", "call focus", "set title Notes", "call showModal", "call close"]) . names
    -- The arguments of the two that have options, as JSON.
    readIORef asked >>= \as -> do
      [o] <- pure [a | Call "scrollIntoView" [a] <- as]
      readProp "block" (Just . foreignToString) o `shouldBe` Just "center"
      [p] <- pure [a | Call "focus" [a] <- as]
      readProp "preventScroll" (Just . foreignToBool) p `shouldBe` Just True
    -- A cast asks it; a class it is not is no cast.
    isJust (HTMLElement.fromElement element) `shouldBe` True
    isJust (HTMLDialogElement.fromElement element) `shouldBe` True
    isNothing (HTMLInputElement.fromElement element) `shouldBe` True
    -- An object it has, as a fixed member.
    foreignToInt (coerce (coerce element :: Window).document :: JSVal) `shouldBe` 8

  it "keeps a read and a method with a result inert" $ do
    (value, asked) <- recording "HTMLDialogElement"
    let element = Element value
    element.isConnected `shouldReturn` False
    element.hasAttribute "open" `shouldReturn` False
    (coerce element :: HTMLDocument).title `shouldReturn` ""
    readIORef asked >>= (`shouldBe` []) . names

  it "notes what a component did to the in-memory DOM: a focus in Initialize, a dialog opened and closed, the title" $ do
    container <- N.newElement Nothing (ElemName "div")
    H.HalogenSocket {H.query = ask, H.dispose = dispose} <- runMemDOM (VD.runUI focusing () (N.fromNative container))
    [root] <- N.childNodes container
    [input, dialog] <- N.childNodes root
    N.performedOn input `shouldReturn` [N.Called "focus"]
    _ <- runMemDOM (ask (H.mkTell Open))
    _ <- runMemDOM (ask (H.mkTell Shut))
    N.performedOn dialog `shouldReturn` [N.Called "showModal", N.Called "close"]
    doc <- runMemDOM (DOM.window >>= DOM.document)
    N.performedOn (N.toNative doc) >>= (`shouldBe` True) . elem (N.Assigned "title" "Focused")
    runMemDOM dispose

#endif
