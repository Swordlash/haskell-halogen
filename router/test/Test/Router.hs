{-# LANGUAGE CPP #-}

-- | The router over the in-memory address it keeps natively. In a browser
-- it is the browser's address, which the examples gallery's test drives.
module Test.Router (spec) where

import Protolude hiding (optional)

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "router" pass

#else

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Halogen.HTML.Core (HTML (..))
import Halogen.HTML.Properties qualified as HP
import Halogen.Router qualified as Router
import Halogen.Router.Duplex
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM (VDomSpec (..), buildVDom)
import Halogen.VDom.DOM.Monad (runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.DOM.Prop (buildProp)
import Halogen.VDom.Machine (extract)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

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

spec :: Spec
spec = describe "router" $ do
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

  describe "links" $
    it "keeps a link's href and click the router's, whatever the caller gives" $ do
      router <- runMemDOM (Router.newRouter "/base/" Home route)
      doc <- N.newDocument
      let vspec =
            VDomSpec
              { buildWidget = \_ -> absurd
              , buildAttributes = buildProp (\_ -> pure ())
              , document = N.fromNative doc
              }
          HTML vdom = Router.link router (const ()) (Page 2) [HP.href "elsewhere"] [] :: HTML Void ()
      element <- N.toNative . extract <$> runMemDOM (buildVDom vspec vdom)
      N.propertyList element `shouldReturn` [("href", "/base/page/2")]
      N.listenerTypes element `shouldReturn` ["click"]

  describe "by fragment" $ do
    it "writes routes after a #" $ do
      router <- runMemDOM (Router.newHashRouter Home route)
      Router.href router (Page 3) `shouldBe` "#/page/3"
      seen <- recording router
      Router.navigate router (Page 3)
      Router.current router `shouldReturn` Page 3
      readIORef seen `shouldReturn` [Page 3]

#endif
