-- | Requests made and waited for directly, against the runner's file server
-- and the network double (test/web/network-double.js).
module Test.Fetch (spec) where

import Control.Concurrent (ThreadId, forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (unless, void)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen.Net.Fetch
import System.Timeout (timeout)
import Test.Hspec
import Test.Network (aborted, fixtureText, release, started)

spec :: Spec
spec = do
  describe "a file" $ do
    it "is a response with its status, address and headers" $ do
      r <- responseOf =<< fetch (request GET "fixture.txt")
      r.status `shouldBe` 200
      r.ok `shouldBe` True
      r.statusText `shouldBe` "OK"
      r.url `shouldSatisfy` ("/fixture.txt" `Text.isSuffixOf`)
      lookup "content-type" r.headers `shouldBe` Just "application/octet-stream"

    it "has a body to read as text, decoded as UTF-8" $ do
      r <- responseOf =<< fetch (request GET "fixture.txt")
      text r `shouldReturn` Right fixtureText

    it "has a body to read as bytes" $ do
      r <- responseOf =<< fetch (request GET "fixture.bin")
      bytes r `shouldReturn` Right (ByteString.pack [0 .. 255])

    it "has a body that can be read once" $ do
      r <- responseOf =<< fetch (request GET "fixture.txt")
      text r `shouldReturn` Right fixtureText
      rejectedWith "TypeError" =<< text r
      rejectedWith "TypeError" =<< bytes r

  describe "an error status" $
    it "is a response that is not ok" $ do
      r <- responseOf =<< fetch (request GET "no-such-file.txt")
      r.status `shouldBe` 404
      r.ok `shouldBe` False

  describe "a request that gets no response" $ do
    it "is a TypeError for an address that is no address" $
      rejectedWith "TypeError" =<< fetch (request GET "http://[::1")
    it "is a TypeError for a host that does not answer" $
      rejectedWith "TypeError" =<< fetch (request GET "http://127.0.0.1:1/")
    it "is a TypeError for a header name fetch refuses" $
      rejectedWith "TypeError" =<< fetch (request GET "fixture.txt") {headers = [("no spaces", "here")]}
    it "is a TypeError for a GET with a body" $
      rejectedWith "TypeError" =<< fetch (request GET "fixture.txt") {body = Just (TextBody "a body")}
    it "is a TypeError for another origin in SameOriginMode" $
      rejectedWith "TypeError" =<< fetch (request GET "http://127.0.0.1:1/") {mode = SameOriginMode}

  describe "what a request says" $ do
    it "reaches fetch: the method, the headers, a text body, credentials and mode" $ do
      r <-
        responseOf
          =<< fetch
            (request POST "echo")
              { headers = [("x-one", "1"), ("content-type", "application/json"), ("x-two", "a"), ("x-two", "b")]
              , body = Just (TextBody "{\"zażółć\": 1}")
              , credentials = OmitCredentials
              , mode = SameOriginMode
              }
      r.statusText `shouldBe` "Echoed"
      echoed r "method" `shouldBe` Just "POST"
      echoed r "x-one" `shouldBe` Just "1"
      echoed r "content-type" `shouldBe` Just "application/json"
      echoed r "x-two" `shouldBe` Just "a, b"
      echoed r "credentials" `shouldBe` Just "omit"
      echoed r "mode" `shouldBe` Just "same-origin"
      text r `shouldReturn` Right "{\"zażółć\": 1}"

    it "sends a text body as text/plain unless it says otherwise" $ do
      r <- responseOf =<< fetch (request PUT "echo") {body = Just (TextBody "plain")}
      echoed r "method" `shouldBe` Just "PUT"
      echoed r "content-type" `shouldBe` Just "text/plain;charset=UTF-8"
      echoed r "credentials" `shouldBe` Just "same-origin"
      echoed r "mode" `shouldBe` Just "cors"

    it "sends bytes as they are" $ do
      let sent = ByteString.pack [0, 255, 10, 13, 0xc3]
      r <- responseOf =<< fetch (request (OtherMethod "PROPFIND") "echo") {body = Just (BytesBody sent)}
      echoed r "method" `shouldBe` Just "PROPFIND"
      echoed r "content-type" `shouldBe` Nothing
      bytes r `shouldReturn` Right sent

  describe "abort" $ do
    it "before the response arrives gives an AbortError" $ do
      f <- start (request GET "fixture.txt")
      abort f
      rejectedWith "AbortError" =<< response f

    it "aborts a request on its way" $ do
      f <- start (request GET "held/aborted/fixture.txt")
      started "aborted" `shouldReturn` True
      abort f
      rejectedWith "AbortError" =<< response f
      aborted "aborted" `shouldReturn` True

    it "after the body has been read does nothing" $ do
      f <- start (request GET "fixture.txt")
      r <- responseOf =<< response f
      text r `shouldReturn` Right fixtureText
      abort f
      (.status) <$> (responseOf =<< response f) `shouldReturn` 200

  describe "a wait that is killed" $ do
    it "in fetch aborts the request" $ do
      t <- forkIO (void (fetch (request GET "held/killed/fixture.txt")))
      waitUntil "the request is made" (started "killed")
      killedPromptly t
      waitUntil "the request is aborted" (aborted "killed")

    it "for the body of fetch's response aborts the request" $ do
      r <- responseOf =<< fetch (request GET "stream/killed-body")
      t <- waiting (void (text r))
      killedPromptly t
      waitUntil "the request is aborted" (aborted "killed-body")

    it "in response only stops waiting" $ do
      f <- start (request GET "held/kept/fixture.txt")
      t <- waiting (void (response f))
      killedPromptly t
      threadDelay 100_000
      aborted "kept" `shouldReturn` False
      release "kept"
      r <- responseOf =<< response f
      text r `shouldReturn` Right fixtureText

    it "for the body of response's response only stops waiting" $ do
      f <- start (request GET "stream/kept-body")
      r <- responseOf =<< response f
      t <- waiting (void (text r))
      killedPromptly t
      threadDelay 100_000
      aborted "kept-body" `shouldReturn` False
      abort f
  where
    echoed r name = lookup ("x-echo-" <> name) r.headers

-- | The response, or the test fails with the rejection.
responseOf :: Either JSError Response -> IO Response
responseOf = either (\e -> fail ("expected a response, got " <> show e)) pure

rejectedWith :: Text -> Either JSError a -> Expectation
rejectedWith name = \case
  Left e -> e.name `shouldBe` name
  Right _ -> expectationFailure ("expected " <> Text.unpack name <> ", got a value")

-- | Run the wait on a thread of its own, and return once it is waiting: the
-- thread runs until it blocks, and this one goes on only then.
waiting :: IO () -> IO ThreadId
waiting wait = do
  ready <- newEmptyMVar
  t <- forkIO (putMVar ready () >> wait)
  takeMVar ready
  threadDelay 20_000
  pure t

-- | Kill a thread blocked in a wait, failing the test if the kill is not
-- delivered within a second: 'killThread' returns only once it is.
killedPromptly :: ThreadId -> Expectation
killedPromptly t = timeout 1_000_000 (killThread t) `shouldReturn` Just ()

waitUntil :: String -> IO Bool -> Expectation
waitUntil what check = go (200 :: Int)
  where
    go 0 = expectationFailure ("timed out waiting until " <> what)
    go n = check >>= \ok -> unless ok (threadDelay 10_000 >> go (n - 1))
