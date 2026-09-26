{-# LANGUAGE CPP #-}

-- | A render that fails part way has changed part of the page already. The
-- next render must bring the page to what it renders, not diff against a
-- record of the page that the failed one made untrue. Run against the
-- in-memory DOM, whose tree is what a browser's would be.
module Test.RenderRecovery (spec) where

import Prelude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "render recovery" $ pure ()

#else

import Control.Exception (ErrorCall (..), SomeException, throw, try)
import Control.Monad.State.Class (put)
import Data.Foldable (for_)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.VDom.DOM.Monad (MemDOM, appendChild, runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.Driver qualified as VD
import Halogen.VDom.Types (ElemName (..))
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)
import Web.HTML.Common (AttrName (..))

data Query a = Set Text Bool a

-- | A class on a first element, and after it a text that fails to render
-- when asked to.
flaky :: H.Component Query () Void MemDOM
flaky =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ("a", False)
      , render = \(cls, bad) ->
          HH.div_
            [ HH.span [HP.class_ (HH.ClassName cls), HP.attr (AttrName "data-tone") cls] []
            , HH.text (if bad then throw (ErrorCall "no text") else "ok")
            ]
          :: H.ComponentHTML () Empty MemDOM
      , eval = H.mkEval H.defaultEval {H.handleQuery = \(Set cls bad a) -> put (cls, bad) >> pure (Just a)}
      }

recovers :: IO ()
recovers = do
  container <- N.newElement Nothing (ElemName "div")
  H.HalogenSocket {H.query = ask, H.dispose = dispose} <- runMemDOM (VD.runUI flaky () (N.fromNative container))
  -- The markup, with the first element's properties (its class) after it.
  let page = do
        html <- N.renderToText container
        root : _ <- N.childNodes container
        first : _ <- N.childNodes root
        props <- N.propertyList first
        pure (html <> " " <> T.pack (show props))
      set cls bad = runMemDOM (ask (H.mkTell (Set cls bad)))
  before <- page
  assertWith ("rendered class a: " <> T.unpack before) ("\"a\"" `T.isInfixOf` before)
  -- The class is changed on the page, then the text fails.
  failed <- try (set "b" True)
  assertWith "the render failed" (either (\(_ :: SomeException) -> True) (const False) failed)
  page >>= \during -> assertWith ("the failed render changed the class: " <> T.unpack during) ("\"b\"" `T.isInfixOf` during)
  -- Back to what the last good render had: the page must say so too.
  _ <- set "a" False
  page >>= assertEqual "the page is what was rendered" before
  _ <- set "c" False
  page >>= \after -> assertWith ("and later renders patch it: " <> T.unpack after) ("\"c\"" `T.isInfixOf` after)
  runMemDOM dispose

data Shape a = Shape Text Text Bool a

-- | A root element of the given tag, holding a text that fails to render
-- when asked to.
shaped :: H.Component Shape () Void MemDOM
shaped =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ("div", "first", False)
      , render = \(tag, label, bad) ->
          (if tag == "section" then HH.section_ else HH.div_)
            [HH.text (if bad then throw (ErrorCall "no text") else label)]
          :: H.ComponentHTML () Empty MemDOM
      , eval = H.mkEval H.defaultEval {H.handleQuery = \(Shape tag label bad a) -> put (tag, label, bad) >> pure (Just a)}
      }

-- | Mount 'shaped' between two siblings, and run the steps: each changes
-- the shape, and says whether it fails.
betweenSiblings :: [(Text, Text, Bool)] -> IO Text
betweenSiblings steps = do
  container <- N.newElement Nothing (ElemName "div")
  before <- N.newElement Nothing (ElemName "b")
  after <- N.newElement Nothing (ElemName "i")
  runMemDOM (appendChild (N.fromNative before) (N.fromNative container))
  H.HalogenSocket {H.query = ask, H.dispose = dispose} <- runMemDOM (VD.runUI shaped () (N.fromNative container))
  runMemDOM (appendChild (N.fromNative after) (N.fromNative container))
  N.renderToText container >>= assertEqual "mounted between its siblings" "<div><b></b><div>first</div><i></i></div>"
  for_ steps $ \(tag, label, bad) -> do
    result <- try (runMemDOM (ask (H.mkTell (Shape tag label bad))))
    assertEqual ("the render of " <> T.unpack label <> " failed") bad (either (\(_ :: SomeException) -> True) (const False) result)
  page <- N.renderToText container
  runMemDOM dispose
  pure page

-- | The failed patch took the old root off the page before it failed.
replacedRoot :: IO ()
replacedRoot =
  betweenSiblings [("section", "broken", True), ("div", "back", False)]
    >>= assertEqual "the new root is where the old one was" "<div><b></b><div>back</div><i></i></div>"

-- | The rebuild after a failed patch fails too.
failedRebuild :: IO ()
failedRebuild = do
  betweenSiblings [("div", "broken", True), ("div", "still broken", True), ("div", "back", False)]
    >>= assertEqual "the rebuilt root is where the old one was" "<div><b></b><div>back</div><i></i></div>"
  betweenSiblings [("section", "broken", True), ("section", "still broken", True), ("div", "back", False), ("section", "patched", False)]
    >>= assertEqual "and is patched in place afterwards" "<div><b></b><section>patched</section><i></i></div>"

spec :: Spec
spec =
  describe "render recovery" $ do
    it "brings the page to what is rendered after a render that failed part way" recovers
    it "puts a root back where it was when the failed patch had taken it off the page" replacedRoot
    it "keeps the place through a rebuild that fails too" failedRebuild

#endif
