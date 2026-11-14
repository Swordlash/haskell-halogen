{-# LANGUAGE CPP #-}

-- | The router over the in-memory address it keeps natively, and over a
-- window that answers for its location and history, as a page across a wire
-- does (haskell-halogen-remote's tests drive the real one). In a browser it
-- is the browser's address, which the tutorial's test
-- (examples/learn/browser-test) drives. What a hash router writes is checked
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

import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Trans.Class (MonadTrans)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Foreign (toForeign)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import Halogen.JSBits (JSVal, NativeObject (..), isNull, jsValText, textValue, toJSInt, withNativeObject)
import Halogen.VDom.DOM.Monad (MonadAttributes, MonadBrowserDOM (..), MonadDOM (..))
import UnliftIO (MonadUnliftIO)
import Web.DOM.Internal.Types (Window (..))
import Halogen.HTML qualified as HH
import Halogen.HTML.Core (HTML (..))
import Halogen.HTML.Properties qualified as HP
import Halogen.Router.Duplex
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM (VDomSpec (..), buildVDom)
import Halogen.VDom.DOM.Monad (runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.DOM.Prop (EventMatch (..), HandlerOptions (..), Prop (..), buildProp, handlerOptions)
import Halogen.VDom.Machine (extract)
import Test.Hspec (shouldReturn)
import Web.Event.Event (Event (..), EventType (..))
import Web.UIEvent.MouseEvent (MouseEvent (..))

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

-- | A DOM monad whose window is the one it is given, over the in-memory
-- DOM: what a backend whose window answers for itself looks like to the
-- router.
newtype Windowed m a = Windowed (ReaderT Window m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadTrans)

runWindowed :: Window -> Windowed m a -> m a
runWindowed w (Windowed program) = runReaderT program w

instance (PrimMonad m) => PrimMonad (Windowed m) where
  type PrimState (Windowed m) = PrimState m
  primitive = lift . primitive

instance (MonadDOM m) => MonadDOM (Windowed m) where
  type DomNode (Windowed m) = DomNode m
  type DomElement (Windowed m) = DomElement m
  type DomDocument (Windowed m) = DomDocument m
  type DomEventListener (Windowed m) = DomEventListener m
  type DomEventTarget (Windowed m) = DomEventTarget m
  mkEventListener f = Windowed (ReaderT (\w -> mkEventListener (runWindowed w . f)))
  setListenerOptions ty options listener target = lift (setListenerOptions ty options listener target)

instance (MonadAttributes m) => MonadAttributes (Windowed m)

instance (MonadBrowserDOM m) => MonadBrowserDOM (Windowed m) where
  window = Windowed ask

-- | A page's address and its history, as a window that answers for itself
-- keeps them: the address (@pathname + search + hash@), and what was pushed
-- and replaced, in order.
data Answering = Answering
  { at :: IORef Text
  , written :: IORef [Text]
  , window_ :: Window
  }

-- | The in-memory window, answering for a location and a history kept in
-- the test, from an address. Its listeners are the in-memory window's, so
-- 'popstate' reaches a router made over it.
pageAt :: Text -> IO Answering
pageAt start = do
  at <- newIORef start
  written <- newIORef []
  Window ambient <- runMemDOM window
  let answering get' read' call' =
        NativeObject {call = call', set = \_ _ -> pass, get = get', live = read', instanceOf = const False}
      part = \case
        "pathname" -> Just (T.takeWhile (`notElem` ['?', '#']))
        "search" -> Just (T.dropWhile (/= '?') . T.takeWhile (/= '#'))
        "hash" -> Just (T.dropWhile (/= '#'))
        _ -> Nothing
      location = withNativeObject (answering (const Nothing) (\name -> for (part name) (\f -> textValue . f <$> readIORef at)) (\_ _ -> pass)) (toJSInt 0)
      move how = \case
        [_, _, target] | not (isNull target) -> do
          writeIORef at (jsValText target)
          modifyIORef' written (<> [how <> " " <> jsValText target])
        _ -> pass
      history = withNativeObject (answering (const Nothing) (const (pure Nothing)) (\method arguments -> case method of
        "pushState" -> move "push" arguments
        "replaceState" -> move "replace" arguments
        _ -> pass)) (toJSInt 0)
      windowObject = answering (\case "location" -> Just location; "history" -> Just history; _ -> Nothing) (const (pure Nothing)) (\_ _ -> pass)
  pure Answering {at, written, window_ = Window (withNativeObject windowObject (ambient :: JSVal))}

spec :: Spec
spec = describe "router" $ do
  addresses
  describe "over a window that answers for its address" $ do
    it "starts where the page is, writes the page's history, and hears its back and forward" $ do
      page <- pageAt "/base/page/3?x=1#top"
      router <- runMemDOM (runWindowed page.window_ (Router.newRouter "/base/" Home route))
      Router.current router `shouldReturn` Page 3
      seen <- recording router
      Router.navigate router (Page 1)
      Router.current router `shouldReturn` Page 1
      Router.redirect router Home
      Router.current router `shouldReturn` Home
      readIORef page.written `shouldReturn` ["push /base/page/1", "replace /base/"]
      -- Going back: the page is at the address of the entry it went to by
      -- the time its popstate is dispatched.
      writeIORef page.at "/base/page/3"
      popstate
      readIORef seen `shouldReturn` [Page 1, Home, Page 3]
      Router.stop router
    it "keeps a hash router's route in the page's fragment, on the page's path" $ do
      page <- pageAt "/lesson/?q=1#/page/2"
      router <- runMemDOM (runWindowed page.window_ (Router.newHashRouter Home route))
      Router.current router `shouldReturn` Page 2
      Router.href router (Page 4) `shouldBe` "/lesson/?q=1#/page/4"
      Router.navigate router (Page 4)
      readIORef page.written `shouldReturn` ["push /lesson/?q=1#/page/4"]
      Router.current router `shouldReturn` Page 4
      Router.stop router
    it "keeps the address in memory over a window that does not answer, the in-memory one" $ do
      router <- runMemDOM (Router.newRouter "/base/" Home route)
      Router.current router `shouldReturn` Home
      Router.navigate router (Page 1)
      Router.current router `shouldReturn` Page 1
      Router.stop router
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
      -- The listener prevents the default of a plain click, the only one
      -- it hands over, which the in-memory DOM cannot show: the handler
      -- says so.
      [options | HP.IProp (Handler (EventType "click") options _) <- props]
        `shouldBe` [handlerOptions {preventDefault = True, matching = Just PlainLeftClick}]

  describe "a plain left click, read from an event's JSON" $ do
    let plain = ["button" .= (0 :: Int), "ctrlKey" .= False, "metaKey" .= False, "shiftKey" .= False, "altKey" .= False]
        clickOf fields = Router.isPlainLeftClick (MouseEvent (Event (toForeign (Aeson.object fields))))
        onLink anchor = plain <> ["currentTarget" .= Aeson.object anchor]
    it "is one with the main button and no modifier key" $ do
      clickOf plain `shouldBe` True
      clickOf ["button" .= (0 :: Int)] `shouldBe` True
    it "is none with a modifier key held, or with another button" $ do
      for_ ["ctrlKey", "metaKey", "shiftKey", "altKey"] $ \modifier ->
        (modifier, clickOf (plain <> [Key.fromText modifier .= True])) `shouldBe` (modifier, False)
      clickOf ["button" .= (1 :: Int)] `shouldBe` False
    it "is none once something has prevented its default" $
      clickOf (plain <> ["defaultPrevented" .= True]) `shouldBe` False
    it "is none on a link to another target, or to a download" $ do
      clickOf (onLink ["tagName" .= ("A" :: Text), "target" .= ("_blank" :: Text)]) `shouldBe` False
      clickOf (onLink ["tagName" .= ("A" :: Text), "target" .= ("_SELF" :: Text)]) `shouldBe` True
      clickOf (onLink ["tagName" .= ("A" :: Text), "target" .= ("" :: Text)]) `shouldBe` True
      clickOf (onLink ["tagName" .= ("A" :: Text), "download" .= True]) `shouldBe` False
      clickOf (onLink ["tagName" .= ("A" :: Text)]) `shouldBe` True
    it "is none for an event that says nothing of its button" $ do
      clickOf [] `shouldBe` False
      Router.isPlainLeftClick (MouseEvent (Event (toForeign ()))) `shouldBe` False

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
