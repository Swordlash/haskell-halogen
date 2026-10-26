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
import Halogen.Remote.Protocol (Batch (..), Op (..))
import Halogen.Remote.Server qualified as Server
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import Protolude
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
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
      WS.runClient "127.0.0.1" port "/live" $ \connection -> do
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

  it "has the client to serve" $ do
    Server.clientScript `shouldSatisfy` (not . null . Text.breakOnAll "function halogen_remote_connect" . decodeUtf8)
#endif
