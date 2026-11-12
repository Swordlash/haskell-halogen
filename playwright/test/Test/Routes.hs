-- | Requests answered by the test: routes, and the gate built on them.
-- Each is a small version of what a real smoke test does with them (named
-- beside it), and the rest is what must never happen: a handler that
-- blocks holding up anything else, a handler's failure lost, and anything
-- left waiting when a route, a page, a context or the driver goes while
-- requests are in its hands.
module Test.Routes (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.MVar
import Control.Exception (onException, throwIO, try)
import Control.Monad (forM_, void)
import Data.Aeson (Value)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.IORef
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Clock (getMonotonicTime)
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Halogen.Playwright.Internal (Context (..), Page (..), Playwright (..), routeFailures)
import System.Timeout (timeout)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))

-- | Start a request in the page without waiting for it, kept under a
-- name; 'fetched' waits for what it got: its status and body, or why it
-- failed.
startFetch :: Page -> Text -> Text -> IO ()
startFetch page name address =
  void
    ( evaluateWith
        page
        "([name, address]) => {\
        \  window[name] = fetch(address, {cache: 'no-store'}).then(async (r) => r.status + ' ' + await r.text(), (e) => 'failed');\
        \  return null;\
        \}"
        (name, address)
        :: IO Value
    )

fetched :: Page -> Text -> IO Text
fetched page name = evaluateWith page "(name) => window[name]" name

-- | What a request got, asked and waited for at once (kept under its
-- address, so that several threads may ask at once).
fetchNow :: Page -> Text -> IO Text
fetchNow page address = startFetch page address address >> fetched page address

-- | Whether an MVar is filled within a few seconds.
filledSoon :: MVar () -> IO Bool
filledSoon m = maybe False (const True) <$> timeout 5_000_000 (readMVar m)

-- | A handler that holds its request until it is killed, and says so.
holdUntilKilled :: MVar () -> MVar () -> RoutedRequest -> IO Answer
holdUntilKilled asked killed _ = do
  _ <- tryPutMVar asked ()
  (threadDelay 60_000_000 >> pure Continue) `onException` tryPutMVar killed ()

spec :: Text -> SpecWith Playwright
spec base = describe "routes" $ do
  let at path = base <> path

  -- smoke-test.mjs: the demo's photograph and video from Wikimedia
  -- Commons answered by the test, so that it needs no network.
  it "answers a request in another site's place" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    answered <- newIORef []
    _ <- route page "https://upload.wikimedia.org/**" $ \request -> do
      atomicModifyIORef' answered (\as -> (as <> [request.url], ()))
      pure $
        if ".webm" `Text.isSuffixOf` request.url
          then fulfill 204 Nothing ""
          else fulfill 200 (Just "image/png") standInImage
    evaluateWith
      page
      "(src) => new Promise((done) => { const i = new Image(); i.onload = () => done(i.naturalWidth); i.onerror = () => done(-1); i.src = src; })"
      ("https://upload.wikimedia.org/commons/photograph.png" :: Text)
      `shouldReturn` (32 :: Int)
    -- Another site's answer is opaque to the page; that it came at all is
    -- the route's doing (there is no network here to give it).
    evaluate page "fetch('https://upload.wikimedia.org/commons/film.webm', {mode: 'no-cors'}).then((r) => r.type)" `shouldReturn` ("opaque" :: Text)
    readIORef answered `shouldReturn` ["https://upload.wikimedia.org/commons/photograph.png", "https://upload.wikimedia.org/commons/film.webm"]
    -- Headers of the test's own reach the page.
    _ <- route page "**/headers" $ \_ -> pure (Fulfill Response {status = 200, headers = [("x-stand-in", "yes")], contentType = Just "text/plain", body = "h"})
    evaluate page "fetch('/headers').then((r) => r.headers.get('x-stand-in'))" `shouldReturn` ("yes" :: Text)

  -- smoke-test-server.mjs: the server out of reach (a VPN gone down), its
  -- requests refused by the context's route, and back when it is removed.
  it "fails a context's requests, and lets them through once the route is gone" $ \pw ->
    withContext pw defaultContextOptions $ \ctx -> do
      page <- newPage ctx
      goto page (at "basics.html")
      unreachable <- route ctx (base <> "**") (\_ -> pure (Abort "connectionrefused"))
      fetchNow page (at "second.html") `shouldReturn` "failed"
      -- A page opened after the route was added is under it too.
      other <- newPage ctx
      try (goto other (at "second.html")) >>= \case
        Left (e :: PlaywrightError) -> Text.unpack e.message `shouldContain` "ERR_CONNECTION_REFUSED"
        Right () -> expectationFailure "the page loaded"
      unroute unreachable
      fetchNow page (at "second.html") >>= (`shouldSatisfy` Text.isPrefixOf "200 ")

  -- smoke-test-server.mjs: what the program asks the API for, watched
  -- while it goes through (page.apiAsked).
  it "shows a handler the request, which goes on" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    asked <- newIORef []
    _ <- route page "**/second.html" $ \request -> do
      atomicModifyIORef' asked (\rs -> (rs <> [request], ()))
      pure Continue
    _ :: Value <-
      evaluate
        page
        "fetch('second.html', {method: 'POST', headers: {'x-asking': 'the test'}, body: 'the body'}).then(() => null)"
    reqs <- readIORef asked
    map (\r -> (r.method, r.resourceType, r.postData)) reqs `shouldBe` [("POST", "fetch", Just "the body")]
    map (.url) reqs `shouldBe` [at "second.html"]
    map (lookup "x-asking" . (.headers)) reqs `shouldBe` [Just "the test"]
    -- And it went on, to the server.
    fetchNow page (at "second.html") >>= (`shouldSatisfy` Text.isInfixOf "<h1>Second</h1>")

  it "asks the route added last first, whose Continue passes the request to the one before" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    _ <- route page "**/layered" $ \_ -> pure (fulfill 200 Nothing "the first route's")
    seen <- newIORef (0 :: Int)
    _ <- route page "**/layered" $ \_ -> atomicModifyIORef' seen (\n -> (n + 1, ())) >> pure Continue
    fetchNow page (at "layered") `shouldReturn` "200 the first route's"
    readIORef seen `shouldReturn` 1

  -- The reader thread runs no handler: one that blocks holds its request
  -- and nothing else, not another route's request, not a call.
  it "lets a handler block without holding up anything else" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    letGo <- newEmptyMVar
    asked <- newEmptyMVar
    _ <- route page "**/slow" $ \_ -> putMVar asked () >> readMVar letGo >> pure (fulfill 200 Nothing "slow")
    _ <- route page "**/quick" $ \_ -> pure (fulfill 200 Nothing "quick")
    startFetch page "slow" (at "slow")
    filledSoon asked `shouldReturn` True
    fetchNow page (at "quick") `shouldReturn` "200 quick"
    click (locator page "#count")
    textContent (locator page "#count") `shouldReturn` "1"
    putMVar letGo ()
    fetched page "slow" `shouldReturn` "200 slow"

  it "runs the handlers of requests made at once at the same time" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    _ <- route page "**/each/*" $ \request -> do
      threadDelay 300_000
      pure (fulfill 200 Nothing (Text.encodeUtf8 (Text.takeWhileEnd (/= '/') request.url)))
    started <- getMonotonicTime
    got <- evaluate page "Promise.all([...Array(8).keys()].map((i) => fetch('each/' + i).then((r) => r.text())))"
    took <- subtract started <$> getMonotonicTime
    got `shouldBe` map (Text.pack . show) [0 :: Int .. 7]
    -- Eight handlers of 300 ms one after another would take 2.4 s.
    took `shouldSatisfy` (< 1.5)
    -- And from several threads of the test's at once.
    answers <- forConcurrently [1 :: Int .. 4] $ \i -> fetchNow page (at ("each/" <> Text.pack (show i)))
    answers `shouldBe` map (\i -> "200 " <> Text.pack (show i)) [1 :: Int .. 4]

  it "aborts the request of a handler that throws, and keeps why for the failure" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    _ <- route page "**/broken" $ \_ -> throwIO (userError "the handler broke")
    fetchNow page (at "broken") `shouldReturn` "failed"
    -- So does one whose answer cannot be made.
    _ <- route page "**/lazy" $ \_ -> pure (fulfill 200 Nothing (error "a body that is not there"))
    fetchNow page (at "lazy") `shouldReturn` "failed"
    -- The page still works.
    fetchNow page (at "second.html") >>= (`shouldSatisfy` Text.isPrefixOf "200 ")
    kept <- routeFailures pw (pageContext page).handle
    map (Text.isInfixOf "the handler broke") (foldr (:) [] kept) `shouldBe` [True, False]
    any (Text.isInfixOf (at "broken")) kept `shouldBe` True
    any (Text.isInfixOf "a body that is not there") kept `shouldBe` True
    -- A failure says so.
    failure <- try (withFailureContext page (locator page "#count" `shouldHaveText` "7"))
    case failure of
      Left (HUnitFailure _ (ExpectedButGot (Just preface) _ _)) -> do
        preface `shouldSatisfy` ("Route handlers that failed:" `isInfixOf`)
        preface `shouldSatisfy` ("the handler broke" `isInfixOf`)
        preface `shouldSatisfy` ("Requests that failed:" `isInfixOf`)
        preface `shouldSatisfy` ((Text.unpack (at "broken") <> ": net::ERR_FAILED") `isInfixOf`)
      other -> expectationFailure ("another outcome: " <> show (either Just (const Nothing) other))

  it "stops the handlers in flight when the route is taken away, and lets their requests go on" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    asked <- newEmptyMVar
    killed <- newEmptyMVar
    r <- route page "**/second.html" (holdUntilKilled asked killed)
    startFetch page "held" (at "second.html")
    filledSoon asked `shouldReturn` True
    unroute r
    filledSoon killed `shouldReturn` True
    -- The request went to the server, as if the route had not been there.
    fetched page "held" >>= (`shouldSatisfy` Text.isInfixOf "<h1>Second</h1>")
    -- And the next is not routed at all.
    fetchNow page (at "second.html") >>= (`shouldSatisfy` Text.isPrefixOf "200 ")
    -- Taking it away again does nothing.
    unroute r

  it "stops the handlers in flight when their page closes, and the context's routes go on" $ \pw ->
    withContext pw defaultContextOptions $ \ctx -> do
      page <- newPage ctx
      goto page (at "basics.html")
      asked <- newEmptyMVar
      killed <- newEmptyMVar
      r <- route page "**/second.html" (holdUntilKilled asked killed)
      _ <- route ctx "**/context-wide" (\_ -> pure (fulfill 200 Nothing "still here"))
      startFetch page "held" (at "second.html")
      filledSoon asked `shouldReturn` True
      closePage page
      filledSoon killed `shouldReturn` True
      -- The route went with its page.
      unroute r
      other <- newPage ctx
      goto other (at "basics.html")
      fetchNow other (at "context-wide") `shouldReturn` "200 still here"

  it "stops the handlers in flight when their context closes" $ \pw -> do
    asked <- newEmptyMVar
    killed <- newEmptyMVar
    withContext pw defaultContextOptions $ \ctx -> do
      page <- newPage ctx
      goto page (at "basics.html")
      _ <- route ctx "**/second.html" (holdUntilKilled asked killed)
      startFetch page "held" (at "second.html")
      filledSoon asked `shouldReturn` True
    filledSoon killed `shouldReturn` True
    -- The driver goes on.
    withPage pw $ \page -> do
      goto page (at "basics.html")
      title page `shouldReturn` "Basics"

  it "stops the handlers in flight when the driver ends, and ends all the same" $ \_ -> do
    options <- optionsFromEnvironment
    asked <- newEmptyMVar
    killed <- newEmptyMVar
    started <- getMonotonicTime
    outcome <- timeout 20_000_000 $ withPlaywright options $ \pw -> withPage pw $ \page -> do
      goto page (at "basics.html")
      _ <- route page "**/second.html" (holdUntilKilled asked killed)
      startFetch page "held" (at "second.html")
      filledSoon asked `shouldReturn` True
    outcome `shouldBe` Just ()
    filledSoon killed `shouldReturn` True
    took <- subtract started <$> getMonotonicTime
    took `shouldSatisfy` (< 15)

  describe "a gate" $ do
    -- smoke-test.mjs, smoke-test-server.mjs and test-learn.mjs: app.wasm
    -- held, so that the prerendered page is seen before the program runs,
    -- then let go. gated.html's module script waits at its top level for
    -- what is held, as a program's loader waits for its binary.
    it "holds a page's requests until it is released, and lets the later ones through" $ \pw -> withPage pw $ \page -> do
      gate <- hold page "**/second.html"
      heldCount gate `shouldReturn` 0
      -- The page loads, its script waiting.
      goto page (at "gated.html")
      waitHeld gate
      heldCount gate `shouldReturn` 1
      textContent (locator page "#loaded") `shouldReturn` "waiting"
      locator page "#prerendered" `shouldHaveText` "as the server sent it"
      release gate
      locator page "#loaded" `shouldHaveText` "loaded"
      heldCount gate `shouldReturn` 0
      -- Released for good: a reload goes straight through.
      reload page
      locator page "#loaded" `shouldHaveText` "loaded"

    -- smoke-test.mjs: a page whose program is held for good, its gate
    -- never released, closed with the request still held.
    it "lets a page whose requests it holds close" $ \pw -> withContext pw defaultContextOptions $ \ctx -> do
      page <- newPage ctx
      gate <- hold page "**/second.html"
      goto page (at "gated.html")
      waitHeld gate
      closePage page
      eventuallyWithin 5000 (heldCount gate `shouldReturn` 0)

    -- smoke-test.mjs: one gate for a walk that holds app.wasm on every
    -- load, re-armed before each rather than made anew, and taken away at
    -- the end, so that released routes do not pile up.
    it "is armed again for the next load, and taken away" $ \pw -> withPage pw $ \page -> do
      routesBefore <- length <$> readIORef page.playwright.routes
      gate <- hold page "**/second.html"
      forM_ [1 :: Int .. 3] $ \_ -> do
        rearm gate
        goto page (at "gated.html")
        waitHeld gate
        textContent (locator page "#loaded") `shouldReturn` "waiting"
        release gate
        locator page "#loaded" `shouldHaveText` "loaded"
      -- One route for the three loads.
      length <$> readIORef page.playwright.routes `shouldReturn` routesBefore + 1
      -- Armed, holding a request, and taken away: the request goes on.
      rearm gate
      goto page (at "gated.html")
      waitHeld gate
      closeGate gate
      locator page "#loaded" `shouldHaveText` "loaded"
      length <$> readIORef page.playwright.routes `shouldReturn` routesBefore
      -- Gone for good: armed again, it holds nothing.
      rearm gate
      reload page
      locator page "#loaded" `shouldHaveText` "loaded"
      heldCount gate `shouldReturn` 0

    it "fails a wait for a request it never holds, after the timeout" $ \pw -> withPage pw $ \page -> do
      gate <- hold (withTimeout 300 page) "**/never"
      goto page (at "basics.html")
      started <- getMonotonicTime
      try (waitHeld gate) >>= \case
        Left (e :: PlaywrightError) -> do
          e.name `shouldBe` "TimeoutError"
          Text.unpack e.message `shouldContain` "**/never"
        Right () -> expectationFailure "the wait returned"
      took <- subtract started <$> getMonotonicTime
      took `shouldSatisfy` (\t -> t >= 0.29 && t < 1.5)

-- | smoke-test.mjs's stand-in for the demo's photograph: a 32 by 32 PNG.
standInImage :: ByteString.ByteString
standInImage =
  either error id . Base64.decode $
    "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAIAAAD8GO2jAAAASklEQVR42mP48OUXTRHDCLDAJHUJHjRqwagFI8MCZFTR1ANENMzJoxbQyQKIKeShwWQBqeX+sLeAvPKAGF2jFoxaMHQsGG384kYAzNSeSHKJ2CAAAAAASUVORK5CYII="
