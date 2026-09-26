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
        [root] <- N.childNodes container
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

spec :: Spec
spec =
  describe "render recovery" $
    it "brings the page to what is rendered after a render that failed part way" recovers

#endif
