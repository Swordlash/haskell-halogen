{-# LANGUAGE CPP #-}

-- | What a @ref@ holds when its element is replaced by another under the
-- same label. Among keyed children the reconciler builds the element under
-- a new key before it removes the one whose key went, so the old one's
-- removal is the last the driver hears of the label, and it must not take
-- away the element that replaced it. (An element whose tag changes is
-- removed before its successor is built; that case is here so that it
-- stays right whichever way round it happens.) And what a @ref@ holds when
-- its element stays and only the prop changes: the label moves to another,
-- the prop goes, or it comes. Run against the in-memory DOM, through the
-- real VDom driver.
module Test.Refs (spec) where

import Prelude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "refs" $ pure ()

#else

import Control.Monad.State.Class (gets, modify)
import Control.Monad.UUID (MonadUUID)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.VDom.DOM.Monad (runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.Driver qualified as VD
import Halogen.VDom.Types (ElemName (..))
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)
import Web.DOM.Element (Element)

data Field = Field
  { label :: H.RefLabel
  , spare :: H.RefLabel
  , key :: Int
  , bold :: Bool
  , shown :: Bool
  , refd :: Bool
  }

data Query a
  = Rekey a
  | Retag a
  | Hide a
  | -- | Put the element under the spare label, and the label it was under
    -- in reserve.
    Relabel a
  | Unref a
  | Reref a
  | Held (Maybe Element -> a)
  | HeldSpare (Maybe Element -> a)

-- | A keyed element under a ref, between two keyed siblings: a new key or
-- a new tag replaces it with another element under the same label, while
-- a new label, or the prop dropped or added, patches the element where it
-- is. Its input says whether the element starts with the prop. Nothing in
-- it is the in-memory DOM's; 'holdsAfter', 'clearedWhenRemoved' and
-- 'patched', which read that tree, are what run it there.
field :: forall m. (MonadUUID m) => H.Component Query Bool Void m
field =
  H.mkComponent
    H.ComponentSpec
      { initialState = \refd -> do
          label <- H.newRefLabel "field"
          spare <- H.newRefLabel "spare"
          pure Field {label, spare, key = 0, bold = False, shown = True, refd}
      , render = \model ->
          HH.withKeys_
            HH.div_
            ( [("before", HH.span_ [])]
                <> [ ("field-" <> T.pack (show model.key), (if model.bold then HH.b else HH.i) [HP.ref model.label | model.refd] [])
                   | model.shown
                   ]
                <> [("after", HH.span_ [])]
            )
          :: H.ComponentHTML () Empty m
      , eval =
          H.mkEval
            H.defaultEval
              { H.handleQuery = \case
                  Rekey a -> modify (\s -> s {key = s.key + 1} :: Field) >> pure (Just a)
                  Retag a -> modify (\s -> s {bold = not s.bold} :: Field) >> pure (Just a)
                  Hide a -> modify (\s -> s {shown = False} :: Field) >> pure (Just a)
                  Relabel a -> modify (\s -> s {label = s.spare, spare = s.label} :: Field) >> pure (Just a)
                  Unref a -> modify (\s -> s {refd = False} :: Field) >> pure (Just a)
                  Reref a -> modify (\s -> s {refd = True} :: Field) >> pure (Just a)
                  Held reply -> Just . reply <$> (H.getRef =<< gets (.label))
                  HeldSpare reply -> Just . reply <$> (H.getRef =<< gets (.spare))
              }
      }

-- | Mount 'field', run the step, and check that the ref holds the element
-- in the page afterwards, whose tag is the one given.
holdsAfter :: Query () -> Text -> IO ()
holdsAfter step tag = do
  container <- N.newElement Nothing (ElemName "div")
  H.HalogenSocket {H.query = ask, H.dispose = dispose} <- runMemDOM (VD.runUI field True (N.fromNative container))
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
  H.HalogenSocket {H.query = ask, H.dispose = dispose} <- runMemDOM (VD.runUI field True (N.fromNative container))
  _ <- runMemDOM (ask (H.mkTell Hide))
  N.renderToText container >>= assertEqual "the element is gone" "<div><div><span></span><span></span></div></div>"
  runMemDOM (ask (H.mkRequest Held)) >>= assertWith "the ref holds nothing" . (== Just True) . fmap null
  runMemDOM dispose

-- | Mount 'field', with the prop or without, and run the step, which
-- patches the element where it is. Then check what its label and the
-- spare one hold, against the element in the page; after a 'Relabel' they
-- have changed places.
patched :: Bool -> Query () -> (Maybe Element -> Maybe Element -> N.NativeNode -> IO ()) -> IO ()
patched refd step check = do
  container <- N.newElement Nothing (ElemName "div")
  H.HalogenSocket {H.query = ask, H.dispose = dispose} <- runMemDOM (VD.runUI field refd (N.fromNative container))
  let inPage = do
        [root] <- N.childNodes container
        [_, element, _] <- N.childNodes root
        pure element
  before <- inPage
  _ <- runMemDOM (ask step)
  after <- inPage
  assertWith "the element was patched where it is" (after == before)
  Just held <- runMemDOM (ask (H.mkRequest Held))
  Just spare <- runMemDOM (ask (H.mkRequest HeldSpare))
  check held spare after
  runMemDOM dispose

-- | Whether the ref holds this node of the in-memory tree.
holds :: Maybe Element -> N.NativeNode -> Bool
holds held node = fmap N.toNative held == Just node

spec :: Spec
spec =
  describe "refs" $ do
    it "holds the element that replaced a keyed one with a new key" $ holdsAfter (Rekey ()) "i"
    it "holds the element that replaced one whose tag changed" $ holdsAfter (Retag ()) "b"
    it "holds nothing once its element is removed" clearedWhenRemoved
    it "moves to the new label when an element's ref changes label" $ patched True (Relabel ()) $ \held old element -> do
      assertWith "the new label holds the element" (holds held element)
      assertWith "the old label holds nothing" (null old)
    it "holds nothing once an element's ref prop goes" $ patched True (Unref ()) $ \held spare _ -> do
      assertWith "the label holds nothing" (null held)
      assertWith "the spare label holds nothing" (null spare)
    it "holds an element once a ref prop is added to it" $ patched False (Reref ()) $ \held spare element -> do
      assertWith "the label holds the element" (holds held element)
      assertWith "the spare label holds nothing" (null spare)

#endif
