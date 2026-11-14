{-# LANGUAGE CPP #-}

-- | The server over a real WebSocket: a client connects, gets the first
-- render, clicks, gets the patch, and leaves, which ends its session.
module Test.Server (spec) where

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

import Protolude
import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "the server (native only)" pass

#else

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Data.List (lookup)
import Halogen.Activity (Activity (..))
import Data.Map.Strict qualified as Map
import Halogen.Remote.Protocol (Batch (..), FromPage (..), FromProgram (..), Hello (..), Op (..), decodeFromProgram, encodeFromPage, encodeProbeQuestion)
import Halogen.Router qualified as Router
import Halogen.Remote.Session qualified as Session
import Test.Components (Spot (..), counter, echo, spots)
import Halogen.Remote.Server qualified as Server
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import Protolude
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy)
import Test.Session (clickOn, elementWithId, listenerOn)
import Data.Row (Empty)
import Halogen.HTML.Properties qualified as HP
import System.Timeout (timeout)

-- | A counter that says when it is finalized, which happens when its
-- session ends.
watched :: forall m. (MonadIO m) => MVar () -> H.Component H.VoidF () Void m
watched finalized =
  H.component
    (\() -> pure (0 :: Int))
    render
    [H.onAction (\() -> modify (+ 1)), H.onFinalize (liftIO (putMVar finalized ()))]
  where
    render :: Int -> H.ComponentHTML () Empty m
    render n = HH.button [HP.id "increment", HE.onClick (const ())] [HH.text (show n)]

-- | A client of the server's at a path, which says first where its page is,
-- as the real client does when it connects: at the root, with empty
-- stores.
talking :: Int -> [Char] -> (WS.Connection -> IO a) -> IO a
talking port path body = WS.runClient "127.0.0.1" port path $ \connection -> do
  WS.sendTextData connection (encodeFromPage (PageHello (Hello "http://127.0.0.1/" Nothing Map.empty Map.empty)))
  body connection

receiveBatch :: WS.Connection -> IO Batch
receiveBatch connection =
  timeout 5000000 (WS.receiveData connection) >>= \case
    Nothing -> expectationFailure "the server sent nothing" >> panic "unreachable"
    Just text -> maybe (expectationFailure ("not a batch: " <> toS text) >> panic "unreachable") pure (Aeson.decodeStrictText text)

spec :: Spec
spec = describe "the server" $ do
  it "serves a session per connection, and ends it when the connection closes" $ do
    finalized <- newEmptyMVar
    let app = Server.application Server.defaultSettings (watched finalized) ()
    Warp.testWithApplication (pure app) $ \port ->
      talking port "/live" $ \connection -> do
        opening <- receiveBatch connection
        opening.sequence `shouldBe` 1
        head opening.ops `shouldBe` Just Clear
        let ops = opening.ops
        button <- maybe (panic "no button") pure (elementWithId ops "increment")
        listener <- maybe (panic "no listener") pure (listenerOn ops button "click")
        WS.sendTextData connection (clickOn listener button)
        patch <- receiveBatch connection
        patch.sequence `shouldBe` 2
        [t | SetText _ t <- patch.ops] `shouldBe` ["1"]
        WS.sendClose connection ("bye" :: Text)
        -- Read on until the server closes too.
        _ <- timeout 5000000 (try (forever (WS.receiveDataMessage connection)) :: IO (Either WS.ConnectionException ()))
        pass
    timeout 5000000 (takeMVar finalized) >>= (`shouldBe` Just ())

  it "calls a function per connection, which mounts what the connection asked for" $ do
    seen <- newMVar []
    let connect request session = do
          modifyMVar_ seen (pure . (<> [(request.path, request.query)]))
          case join (lookup "which" request.query) of
            Just "counter" -> void (Session.mount session counter ())
            Just "echo" -> void (Session.mount session echo ())
            _ -> pass
        app = Server.applicationWith Server.defaultSettings connect
    Warp.testWithApplication (pure app) $ \port -> do
      let firstOps path = talking port path $ \connection -> do
            opening <- receiveBatch connection
            WS.sendClose connection ("bye" :: Text)
            pure opening.ops
      counted <- firstOps "/live?which=counter"
      echoed <- firstOps "/live?which=echo&x"
      elementWithId counted "increment" `shouldSatisfy` isJust
      elementWithId counted "field" `shouldBe` Nothing
      elementWithId echoed "field" `shouldSatisfy` isJust
      readMVar seen >>= (`shouldBe` [("/live", [("which", Just "counter")]), ("/live", [("which", Just "echo"), ("x", Nothing)])])
      -- A connection the function mounts nothing for is closed, with why.
      closed <- talking port "/live?which=nothing" $ \connection ->
        timeout 5000000 (try (WS.receiveDataMessage connection))
      case closed of
        Just (Left (WS.CloseRequest _ reason)) -> reason `shouldBe` "nothing was mounted for this connection"
        other -> expectationFailure ("not closed with a reason: " <> show (fmap (fmap (const ())) other))

  it "ends only the session whose function throws, with why" $ do
    let connect request session = case join (lookup "which" request.query) of
          Just "broken" -> throwIO (ErrorCall "no component for this one")
          _ -> void (Session.mount session counter ())
        app = Server.applicationWith Server.defaultSettings connect
    Warp.testWithApplication (pure app) $ \port -> do
      closed <- talking port "/live?which=broken" $ \connection ->
        timeout 5000000 (try (WS.receiveDataMessage connection))
      case closed of
        Just (Left (WS.CloseRequest _ reason)) -> reason `shouldBe` "the session ended"
        other -> expectationFailure ("not closed with a reason: " <> show (fmap (fmap (const ())) other))
      -- The server goes on: the next connection gets its component.
      talking port "/live" $ \connection -> do
        opening <- receiveBatch connection
        elementWithId opening.ops "increment" `shouldSatisfy` isJust
        WS.sendClose connection ("bye" :: Text)

  it "answers the probe's questions over the socket, after the batches" $ do
    let app = Server.application Server.defaultSettings counter ()
    Warp.testWithApplication (pure app) $ \port ->
      talking port "/live" $ \connection -> do
        opening <- receiveBatch connection
        button <- maybe (panic "no button") pure (elementWithId opening.ops "increment")
        listener <- maybe (panic "no listener") pure (listenerOn opening.ops button "click")
        WS.sendTextData connection (clickOn listener button)
        WS.sendTextData connection (encodeProbeQuestion 1)
        patch <- receiveBatch connection
        [t | SetText _ t <- patch.ops] `shouldBe` ["1"]
        answer <- timeout 5000000 (WS.receiveData connection)
        (answer >>= decodeFromProgram) `shouldBe` Just (ProbeAnswer 1 Activity {trees = 1, running = 0, waiting = 0})
        WS.sendClose connection ("bye" :: Text)

  it "hands the page's first message to the session before it calls the function, and takes anything else first as any message" $ do
    seen <- newMVar []
    let connect _ session = do
          place <- Session.run session $ do
            base <- Router.pageBase
            router <- Router.newRouter base Front spots
            spot <- Router.current router
            Router.stop router
            pure (base, spot)
          modifyMVar_ seen (pure . (<> [place]))
          void (Session.mount session counter ())
        app = Server.applicationWith Server.defaultSettings connect
        at = Hello "http://127.0.0.1/site/blog/post/3" (Just "http://127.0.0.1/site/") Map.empty Map.empty
    Warp.testWithApplication (pure app) $ \port -> do
      WS.runClient "127.0.0.1" port "/live" $ \connection -> do
        WS.sendTextData connection (encodeFromPage (PageHello at))
        _ <- receiveBatch connection
        WS.sendClose connection ("bye" :: Text)
      -- A client that asks the probe first: answered (what the reading is
      -- depends on whether the mount has begun when it is sent), and given
      -- a session at the root.
      WS.runClient "127.0.0.1" port "/live" $ \connection -> do
        WS.sendTextData connection (encodeProbeQuestion 1)
        answer <- timeout 5000000 (WS.receiveData connection)
        [n | Just (ProbeAnswer n _) <- [answer >>= decodeFromProgram]] `shouldBe` [1]
        _ <- receiveBatch connection
        WS.sendClose connection ("bye" :: Text)
      readMVar seen `shouldReturn` [("/site/", Post 3), ("/", Front)]

  it "has the client to serve" $ do
    Server.clientScript `shouldSatisfy` (not . null . Text.breakOnAll "function halogen_remote_connect" . decodeUtf8)
#endif
