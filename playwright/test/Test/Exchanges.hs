-- | What crossed the network, seen whole: a route that fetches the
-- server's response to look at or change, every header a request was sent
-- with (its cookies too), the exchanges a context records, and routes and
-- gates that take only some kinds of request. A server smoke test's needs:
-- "the page POSTed to the API and got 200 with this JSON", "the session's
-- cookie went with it", and an API held while the page's images load.
module Test.Exchanges (spec) where

import Control.Exception (try)
import Data.Aeson (Value, decodeStrict, object, (.=))
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Test.Server (standInImage, withServer)

-- | What a request from the page got: its status and body, or why it
-- failed.
fetchFrom :: Page -> Text -> Text -> Text -> IO Text
fetchFrom page method address body =
  evaluateWith
    page
    "async ([method, address, body]) => {\
    \  try {\
    \    const r = await fetch(address, method === 'GET' ? {cache: 'no-store'} : {method, body, cache: 'no-store'});\
    \    return r.status + ' ' + await r.text();\
    \  } catch (e) { return 'failed'; }\
    \}"
    (method, address, body)

spec :: SpecWith Playwright
spec = describe "what crossed the network" $ do
  it "fetches the server's response in a route's handler, which changes it" $ \pw -> withServer $ \base -> withPage pw $ \page -> do
    goto page (base <> "basics.html")
    seen <- newIORef []
    _ <- route page "**/api/data" $ \request -> do
      r <- fetchResponse request
      atomicModifyIORef' seen (\rs -> (rs <> [r], ()))
      pure (Fulfill r {body = "{\"data\":\"changed by the test\"}"})
    fetchFrom page "GET" "api/data" "" `shouldReturn` "200 {\"data\":\"changed by the test\"}"
    [r] <- readIORef seen
    r.status `shouldBe` 200
    r.body `shouldBe` "{\"data\":\"from the server\"}"
    lookup "content-type" r.headers `shouldBe` Just "application/json"
    -- What described the bytes on the wire is left out: the body changed.
    lookup "content-length" r.headers `shouldBe` Nothing

  it "looks at a request and its response and lets the page have them as they came" $ \pw -> withServer $ \base -> withPage pw $ \page -> do
    goto page (base <> "basics.html")
    seen <- newIORef []
    _ <- route page "**/api/echo" $ passThrough $ \request response ->
      atomicModifyIORef' seen (\rs -> (rs <> [(request.method, request.postData, response.status, response.body)], ()))
    answer <- fetchFrom page "POST" "api/echo" "the body"
    answer `shouldSatisfy` Text.isPrefixOf "200 "
    readIORef seen >>= \case
      [(method, postData, status, body)] -> do
        (method, postData, status) `shouldBe` ("POST", Just "the body", 200)
        decodeStrict body `shouldBe` Just (object ["method" .= ("POST" :: Text), "body" .= ("the body" :: Text), "cookie" .= (Nothing :: Maybe Text)])
      other -> expectationFailure ("seen: " <> show other)
    -- Both of the server's cookies reached the page: a header that came
    -- twice went on twice.
    evaluate page "document.cookie" `shouldReturn` ("a=1; b=2" :: Text)

  it "gives a route's handler every header the browser sends, the cookies too" $ \pw -> withServer $ \base -> withContext pw defaultContextOptions $ \ctx -> do
    addCookies ctx [cookie "session" "s3cret" base]
    page <- newPage ctx
    goto page (base <> "basics.html")
    seen <- newIORef []
    _ <- route page "**/api/echo" $ \request -> atomicModifyIORef' seen (\rs -> (rs <> [request], ())) >> pure Continue
    answer <- fetchFrom page "GET" "api/echo" ""
    -- The server had it too.
    Text.unpack answer `shouldContain` "\"cookie\":\"session=s3cret\""
    [request] <- readIORef seen
    lookup "cookie" request.allHeaders `shouldBe` Just "session=s3cret"

  describe "a context's exchanges" $ do
    let recording = defaultContextOptions {recordExchanges = Just 100}

    it "keeps what each request sent and what came back, cookies and bodies too" $ \pw -> withServer $ \base -> withContext pw recording $ \ctx -> do
      addCookies ctx [cookie "session" "s3cret" base]
      page <- newPage ctx
      goto page (base <> "basics.html")
      _ <- fetchFrom page "POST" "api/echo" "{\"title\":\"Home\"}"
      saved <- waitForExchange ctx (\e -> e.method == "POST" && "/api/echo" `Text.isSuffixOf` e.url)
      saved.resourceType `shouldBe` "fetch"
      saved.status `shouldBe` Just 200
      saved.failure `shouldBe` Nothing
      saved.postData `shouldBe` Just "{\"title\":\"Home\"}"
      lookup "cookie" saved.requestHeaders `shouldBe` Just "session=s3cret"
      (decodeStrict =<< saved.responseBody) `shouldBe` Just (object ["method" .= ("POST" :: Text), "body" .= ("{\"title\":\"Home\"}" :: Text), "cookie" .= ("session=s3cret" :: Text)])
      [v | (k, v) <- saved.responseHeaders, k == "set-cookie"] `shouldBe` ["a=1; Path=/", "b=2; Path=/"]
      -- The page's own load is there too, oldest first.
      map (.url) <$> exchanges ctx >>= (`shouldSatisfy` ((base <> "basics.html") `elem`))

    it "keeps a body up to the limit, and says why there is no response" $ \pw -> withServer $ \base -> withContext pw recording $ \ctx -> do
      page <- newPage ctx
      goto page (base <> "basics.html")
      _ <- fetchFrom page "GET" "api/big" ""
      big <- waitForExchange ctx (\e -> "/api/big" `Text.isSuffixOf` e.url)
      big.status `shouldBe` Just 200
      big.responseBody `shouldBe` Nothing
      _ <- fetchFrom page "GET" "api/data" ""
      small <- waitForExchange ctx (\e -> "/api/data" `Text.isSuffixOf` e.url)
      small.responseBody `shouldBe` Just "{\"data\":\"from the server\"}"
      _ <- route page "**/api/refused" (\_ -> pure (Abort "connectionrefused"))
      fetchFrom page "GET" "api/refused" "" `shouldReturn` "failed"
      refused <- waitForExchange ctx (\e -> "/api/refused" `Text.isSuffixOf` e.url)
      refused.status `shouldBe` Nothing
      refused.failure `shouldBe` Just "net::ERR_CONNECTION_REFUSED"

    it "keeps none unless the context's options say so, and the wait says so" $ \pw -> withServer $ \base -> withContext pw defaultContextOptions $ \ctx -> do
      page <- newPage ctx
      goto page (base <> "basics.html")
      _ <- fetchFrom page "GET" "api/data" ""
      exchanges ctx `shouldReturn` []
      -- Its own requests are still listed.
      map (.url) <$> requests ctx >>= (`shouldSatisfy` ((base <> "api/data") `elem`))
      try (waitForExchange ctx (const True)) >>= \case
        Left (e :: PlaywrightError) -> do
          e.name `shouldBe` "TimeoutError"
          Text.unpack e.message `shouldContain` "recordExchanges"
        Right e -> expectationFailure ("an exchange: " <> show e)

  describe "a filter" $ do
    -- The wiki's smoke test: the API held while the page loads, its images
    -- let through, or the page's load never comes.
    it "holds an API's fetch requests and lets its images through" $ \pw -> withServer $ \base -> withPage pw $ \page -> do
      gate <- holdWith page (routeFilter "**/api/**") {resourceTypes = ["fetch"]}
      goto page (base <> "api.html")
      waitHeld gate
      heldCount gate `shouldReturn` 1
      textContent (locator page "#data") `shouldReturn` "waiting"
      -- The image under the same glob loaded.
      evaluate page "document.getElementById('pic').naturalWidth" `shouldReturn` (32 :: Int)
      release gate
      locator page "#data" `shouldHaveText` "from the server"

    it "routes the methods it names, and never asks about the rest" $ \pw -> withServer $ \base -> withPage pw $ \page -> do
      goto page (base <> "basics.html")
      asked <- newIORef (0 :: Int)
      _ <- routeWith page (routeFilter "**/api/echo") {methods = ["post"]} $ \_ -> do
        atomicModifyIORef' asked (\n -> (n + 1, ()))
        pure (fulfill 503 (Just "text/plain") "routed")
      fetchFrom page "POST" "api/echo" "x" `shouldReturn` "503 routed"
      fetchFrom page "GET" "api/echo" "" >>= (`shouldSatisfy` Text.isPrefixOf "200 ")
      readIORef asked `shouldReturn` 1
      -- By resource type too: the page's image is not a fetch.
      images <- newIORef (0 :: Int)
      _ <- routeWith page (routeFilter "**/api/pic.png*") {resourceTypes = ["image"]} $ \_ -> do
        atomicModifyIORef' images (\n -> (n + 1, ()))
        pure (fulfill 200 (Just "image/png") standInImage)
      fetchFrom page "GET" "api/pic.png" "" >>= (`shouldSatisfy` Text.isPrefixOf "200 ")
      readIORef images `shouldReturn` 0
      _ :: Value <- evaluate page "new Promise((done) => { const i = new Image(); i.onload = i.onerror = () => done(null); i.src = 'api/pic.png?again'; })"
      readIORef images `shouldReturn` 1
