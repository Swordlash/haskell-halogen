{-# LANGUAGE CPP #-}

-- | What a @ref@ holds when its element is replaced by another under the
-- same label. Among keyed children the reconciler builds the element under
-- a new key before it removes the one whose key went, so the old one's
-- removal is the last the driver hears of the label, and it must not take
-- away the element that replaced it. (An element whose tag changes is
-- removed before its successor is built; that case is here so that it
-- stays right whichever way round it happens.) Run against the in-memory
-- DOM, through the real VDom driver.
module Test.Refs (spec) where

import Prelude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "refs" $ pure ()

#else

import Control.Monad.State.Class (gets, modify)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.Driver qualified as VD
import Halogen.VDom.Types (ElemName (..))
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)
import Web.DOM.Element (Element)

data Field = Field
  { label :: H.RefLabel
  , key :: Int
  , bold :: Bool
  , shown :: Bool
  }

data Query a
  = Rekey a
  | Retag a
  | Hide a
  | Held (Maybe Element -> a)

-- | A keyed element under a ref, between two keyed siblings: a new key or
-- a new tag replaces it with another element under the same label.
field :: H.Component Query () Void MemDOM
field =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> do
          label <- H.newRefLabel "field"
          pure Field {label, key = 0, bold = False, shown = True}
      , render = \model ->
          HH.withKeys_
            HH.div_
            ( [("before", HH.span_ [])]
                <> [ ("field-" <> T.pack (show model.key), (if model.bold then HH.b else HH.i) [HP.ref model.label] [])
                   | model.shown
                   ]
                <> [("after", HH.span_ [])]
            )
          :: H.ComponentHTML () Empty MemDOM
      , eval =
          H.mkEval
            H.defaultEval
              { H.handleQuery = \case
                  Rekey a -> modify (\s -> s {key = s.key + 1} :: Field) >> pure (Just a)
                  Retag a -> modify (\s -> s {bold = not s.bold} :: Field) >> pure (Just a)
                  Hide a -> modify (\s -> s {shown = False} :: Field) >> pure (Just a)
                  Held reply -> Just . reply <$> (H.getRef =<< gets (.label))
              }
      }

-- | Mount 'field', run the step, and check that the ref holds the element
-- in the page afterwards, whose tag is the one given.
holdsAfter :: Query () -> Text -> IO ()
holdsAfter step tag = do
  container <- N.newElement Nothing (ElemName "div")
  H.HalogenSocket {H.query = ask, H.dispose = dispose} <- runMemDOM (VD.runUI field () (N.fromNative container))
  let held = runMemDOM (ask (H.mkRequest Held))
      inPage = do
        [root] <- N.childNodes container
        [_, element, _] <- N.childNodes root
        pure element
  before <- inPage
  held >>= assertWith "the ref holds the element as mounted" . (== Just (Just before)) . fmap (fmap N.toNative)
  _ <- runMemDOM (ask step)
  after <- inPage
  assertWith "the element was replaced" (after /= before)
  N.renderToText container >>= assertEqual "the new element is in the page" ("<div><div><span></span><" <> tag <> "></" <> tag <> "><span></span></div></div>")
  held >>= assertWith "the ref holds the element that replaced it" . (== Just (Just after)) . fmap (fmap N.toNative)
  runMemDOM dispose

-- | Its element removed with nothing in its place, the ref holds nothing.
clearedWhenRemoved :: IO ()
clearedWhenRemoved = do
  container <- N.newElement Nothing (ElemName "div")
  H.HalogenSocket {H.query = ask, H.dispose = dispose} <- runMemDOM (VD.runUI field () (N.fromNative container))
  _ <- runMemDOM (ask (H.mkTell Hide))
  N.renderToText container >>= assertEqual "the element is gone" "<div><div><span></span><span></span></div></div>"
  runMemDOM (ask (H.mkRequest Held)) >>= assertWith "the ref holds nothing" . (== Just True) . fmap null
  runMemDOM dispose

spec :: Spec
spec =
  describe "refs" $ do
    it "holds the element that replaced a keyed one with a new key" $ holdsAfter (Rekey ()) "i"
    it "holds the element that replaced one whose tag changed" $ holdsAfter (Retag ()) "b"
    it "holds nothing once its element is removed" clearedWhenRemoved

#endif
