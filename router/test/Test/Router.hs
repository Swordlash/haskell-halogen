{-# LANGUAGE CPP #-}

-- | The router over the in-memory address it keeps natively. In a browser
-- it is the browser's address, which the tutorial's test
-- (toolchain/test-learn.mjs) drives. What a hash router writes is checked
-- on every backend.
module Test.Router (spec) where

import Halogen.Router qualified as Router
import Protolude hiding (Handler, optional)
import Test.Hspec (Spec, describe, it, shouldBe)

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

import Test.Hspec (xdescribe)

spec :: Spec
spec = describe "router" $ do
  addresses
  xdescribe "in memory" pass

#else

import Data.Foreign (toForeign)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Halogen.HTML qualified as HH
import Halogen.HTML.Core (HTML (..))
import Halogen.HTML.Properties qualified as HP
import Halogen.Router.Duplex
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM (VDomSpec (..), buildVDom)
import Halogen.VDom.DOM.Monad (runMemDOM, window)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.DOM.Prop (HandlerOptions (..), Prop (..), buildProp, handlerOptions)
import Halogen.VDom.Machine (extract)
import Test.Hspec (shouldReturn)
import Web.Event.Event (Event (..), EventType (..))

data Route = Home | Page Int
  deriving stock (Eq, Show)

route :: RouteDuplex' Route
route = root (lmap toPage (fromPage <$> optional (path "page" (int segment))))
  where
    fromPage = maybe Home Page
    toPage = \case
      Home -> Nothing
      Page n -> Just n

-- | Every route the router announces, in order.
recording :: Router.Router Route -> IO (IORef [Route])
recording router = do
  seen <- newIORef []
  void $ HS.subscribe (Router.changes router) $ \r -> modifyIORef' seen (<> [r])
  pure seen

-- | What the browser does going back or forward: the window's @popstate@
-- listeners run. Natively the window is the ambient document, which every
-- router made here listens on.
popstate :: IO ()
popstate = do
  target <- N.toNative <$> runMemDOM window
  listeners <- readIORef target.listeners
  for_ [l | l <- listeners, l.eventType == "popstate"] $ \l -> runMemDOM (l.fire (Event (toForeign ())))

spec :: Spec
spec = describe "router" $ do
  addresses
  describe "by path" $ do
    it "writes every address under its base" $ do
      router <- runMemDOM (Router.newRouter "haskell-halogen" Home route)
      Router.href router Home `shouldBe` "/haskell-halogen/"
      Router.href router (Page 2) `shouldBe` "/haskell-halogen/page/2"
    it "starts at the base, and goes where it is told" $ do
      router <- runMemDOM (Router.newRouter "/base/" Home route)
      seen <- recording router
      Router.current router `shouldReturn` Home
      Router.navigate router (Page 1)
      Router.current router `shouldReturn` Page 1
      Router.redirect router (Page 2)
      Router.current router `shouldReturn` Page 2
      readIORef seen `shouldReturn` [Page 1, Page 2]
    it "announces nothing when it goes where it already is" $ do
      router <- runMemDOM (Router.newRouter "/" Home route)
      seen <- recording router
      Router.navigate router Home
      readIORef seen `shouldReturn` []
    it "follows the history until it is stopped" $ do
      router <- runMemDOM (Router.newRouter "/" Home route)
      seen <- recording router
      popstate
      readIORef seen `shouldReturn` [Home]
      Router.stop router
      popstate
      Router.stop router
      readIORef seen `shouldReturn` [Home]
      Router.navigate router (Page 1)
      readIORef seen `shouldReturn` [Home, Page 1]

  describe "links" $ do
    it "keeps a link's href and click the router's, whatever the caller gives" $ do
      router <- runMemDOM (Router.newRouter "/base/" Home route)
      element <- built (Router.link router (const ()) (Page 2) [HP.href "elsewhere"] [])
      N.propertyList element `shouldReturn` [("href", "/base/page/2")]
      N.listenerTypes element `shouldReturn` ["click"]

    it "gives the properties of a link for an element written by hand" $ do
      router <- runMemDOM (Router.newRouter "/base/" Home route)
      let props = Router.linkProps router (const ()) (Page 2)
      element <- built (HH.a ([HP.href "elsewhere"] <> props) [HH.text "two"])
      N.propertyList element `shouldReturn` [("href", "/base/page/2")]
      N.listenerTypes element `shouldReturn` ["click"]
      -- The listener prevents the default of a click it hands over, which
      -- the in-memory DOM cannot show: the handler says so.
      [options | HP.IProp (Handler (EventType "click") options _) <- props]
        `shouldBe` [handlerOptions {preventDefault = True}]

  describe "by fragment" $ do
    it "is at the root with no fragment, and goes nowhere going there" $ do
      router <- runMemDOM (Router.newHashRouter Home route)
      seen <- recording router
      Router.current router `shouldReturn` Home
      Router.navigate router Home
      readIORef seen `shouldReturn` []
      Router.navigate router (Page 1)
      Router.navigate router Home
      readIORef seen `shouldReturn` [Page 1, Home]

    it "writes routes after a #, on the page's own path" $ do
      router <- runMemDOM (Router.newHashRouter Home route)
      Router.href router (Page 3) `shouldBe` "/#/page/3"
      seen <- recording router
      Router.navigate router (Page 3)
      Router.current router `shouldReturn` Page 3
      readIORef seen `shouldReturn` [Page 3]

-- | An element as the in-memory DOM builds it.
built :: HTML Void () -> IO N.NativeNode
built (HTML vdom) = do
  doc <- N.newDocument
  let vspec =
        VDomSpec
          { buildWidget = \_ -> absurd
          , hydrateWidget = \_ _ -> absurd
          , buildAttributes = buildProp (\_ -> pure ())
          , document = N.fromNative doc
          }
  N.toNative . extract <$> runMemDOM (buildVDom vspec vdom)

#endif

-- A bare "#/about" resolves against the page's <base href>, not its
-- address: under a base of "/" a lesson at "/06-router/" would link and
-- push "/#/about", leaving the lesson.
addresses :: Spec
addresses = describe "a hash router's address" $ do
  it "is the whole address, so that a <base href> does not move it" $ do
    Router.fragmentAddress "/06-router/" "#/about" `shouldBe` "/06-router/#/about"
    Router.fragmentAddress "/06-router/?x=1#/post/2" "#/about" `shouldBe` "/06-router/?x=1#/about"
    Router.fragmentAddress "/a/b#" "#/" `shouldBe` "/a/b#/"
