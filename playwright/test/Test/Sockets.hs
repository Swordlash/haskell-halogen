-- | A page's WebSocket frames, kept as they cross, and a wait for one: a
-- small version of smoke-test-server.mjs's live comments, where a second
-- tab must have subscribed on the server's channel before the first one
-- comments, or the comment is never pushed to it.
module Test.Sockets (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (handle, try)
import Control.Monad (forM_, forever)
import Data.Aeson (Value (..), decodeStrict, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS

-- | A channel like the wiki's: a socket that sends @{"subscribe": page}@ is
-- told @{"type": "subscribed"}@ a moment later, once it is on the list, and
-- from then on gets every comment. The body is given the channel's address
-- and how to comment.
withChannel :: (Text -> (Text -> IO ()) -> IO a) -> IO a
withChannel body = do
  subscribers <- newMVar []
  let channel request = do
        connection <- WS.acceptRequest request
        -- The page closing its socket (it went away) ends the connection:
        -- nothing to report, and Warp's test server would rethrow it.
        handle (\(_ :: WS.ConnectionException) -> pure ()) $ WS.withPingThread connection 30 (pure ()) $ forever $ do
          message <- WS.receiveData connection
          case decodeStrict message of
            Just (Object o) | Just (String _) <- KeyMap.lookup "subscribe" o -> do
              -- The server takes a moment, as a real one does: a comment
              -- made before this would not reach the socket.
              threadDelay 200_000
              modifyMVar_ subscribers (pure . (connection :))
              WS.sendTextData connection (encode (object ["type" .= ("subscribed" :: Text)]))
            _ -> pure ()
      comment text = do
        connections <- readMVar subscribers
        forM_ connections $ \c -> WS.sendTextData c (encode (object ["type" .= ("comment" :: Text), "text" .= text]))
      plain _ respond = respond (Wai.responseLBS HTTP.status426 [] "a WebSocket only")
  Warp.testWithApplication (pure (websocketsOr WS.defaultConnectionOptions channel plain)) $ \port ->
    body ("ws://127.0.0.1:" <> Text.pack (show port) <> "/api/v1/events") comment

spec :: Text -> SpecWith Playwright
spec base = describe "WebSockets" $ do
  let at path = base <> path

  it "keeps a page's frames, and waits for the one that says it subscribed" $ \pw -> withChannel $ \address comment -> withPage pw $ \page -> do
    goto page (at "socket.html?socket=" <> address)
    -- The page's subscription crosses as it loads, perhaps before this
    -- call: frames are kept from the page's start, so it cannot be missed.
    subscribed <- waitForWebSocket page (\f -> not f.sent && "\"subscribed\"" `Text.isInfixOf` f.payload)
    subscribed.url `shouldBe` address
    subscribed.binary `shouldBe` False
    -- Now a comment reaches it.
    comment "Live from the smoke test."
    locator page "#pushed li" `shouldHaveText` "Live from the smoke test."
    frames' <- webSocketFrames page
    map (\f -> (f.sent, f.payload)) frames'
      `shouldBe` [ (True, "{\"subscribe\":\"first-page\"}")
                 , (False, "{\"type\":\"subscribed\"}")
                 , (False, "{\"text\":\"Live from the smoke test.\",\"type\":\"comment\"}")
                 ]
    -- A frame already there is found at once.
    sentFirst <- waitForWebSocket (withTimeout 300 page) (\f -> f.sent)
    Just sentFirst `shouldBe` case frames' of
      f : _ -> Just f
      [] -> Nothing

  it "fails a wait for a frame that never crosses, listing the frames that did" $ \pw -> withChannel $ \address _ -> withPage pw $ \page -> do
    goto page (at "socket.html?socket=" <> address)
    _ <- waitForWebSocket page (\f -> "subscribed" `Text.isInfixOf` f.payload)
    try (waitForWebSocket (withTimeout 300 page) (\f -> "never" `Text.isInfixOf` f.payload)) >>= \case
      Left (e :: PlaywrightError) -> do
        e.name `shouldBe` "TimeoutError"
        Text.unpack e.message `shouldContain` "subscribed"
      Right f -> expectationFailure ("a frame satisfied it: " <> show f)
  it "says no frame crossed on a page with no socket" $ \pw -> withPage pw $ \page -> do
    goto page (at "plain.html")
    try (waitForWebSocket (withTimeout 300 page) (const True)) >>= \case
      Left (e :: PlaywrightError) -> Text.unpack e.message `shouldContain` "no frame crossed"
      Right f -> expectationFailure ("a frame crossed: " <> show f)
