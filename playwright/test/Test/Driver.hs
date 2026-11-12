-- | Every function of the driver against plain pages, and what it does
-- when things go wrong: timeouts, closed pages, a driver that died, a
-- caller that was killed.
module Test.Driver (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async (concurrently, replicateConcurrently_)
import Control.Concurrent.MVar
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (filterM, forM_, replicateM_, unless)
import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTime)
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Halogen.Playwright.Internal (descendants, driverProcessId, killDriver, readProcFile, statFields)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Types (ProcessID)
import System.Timeout (timeout)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))

spec :: Text -> SpecWith Playwright
spec base = describe "the driver" $ do
  let at path = base <> path

  it "acts on elements and reads them, every action settled" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    title page `shouldReturn` "Basics"
    click (locator page "#count")
    -- Settled: the handler has run by the time click returns.
    textContent (locator page "#count") `shouldReturn` "1"
    doubleClick (locator page "#double")
    locator page "#double" `shouldHaveText` "doubled"
    hover (locator page "#hover")
    locator page "#hover" `shouldHaveText` "hovered"
    fill (byLabel page "Name") "Ada"
    locator page "#echo" `shouldHaveText` "Ada"
    inputValue (locator page "#name") `shouldReturn` "Ada"
    locator page "#name" `shouldHaveValue` "Ada"
    clear (locator page "#name")
    locator page "#echo" `shouldHaveText` ""
    typeText (locator page "#keys") "ab"
    locator page "#keys" `shouldHaveValue` "ab"
    pressOn (locator page "#keys") "Enter"
    getAttribute (locator page "#keys") "data-last" `shouldReturn` Just "Enter"
    press page "x"
    getAttribute (locator page "#keys") "data-last" `shouldReturn` Just "x"
    check (locator page "#agree")
    isChecked (locator page "#agree") `shouldReturn` True
    uncheck (locator page "#agree")
    isChecked (locator page "#agree") `shouldReturn` False
    selectOption (locator page "#colour") ["green"]
    inputValue (locator page "#colour") `shouldReturn` "green"
    focus (locator page "#focusable")
    locator page "#focusable" `shouldHaveAttribute` "data-focused" $ Just "yes"
    blur (locator page "#focusable")
    locator page "#focusable" `shouldHaveAttribute` "data-focused" $ Just "no"
    isEnabled (locator page "#disabled") `shouldReturn` False
    isEnabled (locator page "#count") `shouldReturn` True
    count (locator page ".item") `shouldReturn` 3
    locator page ".item" `shouldHaveCount` 3
    textContent (nth 1 (locator page ".item")) `shouldReturn` "two"
    textContent (first (locator page ".item")) `shouldReturn` "one"
    textContent (withText "thr" (locator page ".item")) `shouldReturn` "three"
    textContent (locator (locator page "ul") ".special") `shouldReturn` "three"
    textContent (byRoleNamed page "button" "log") `shouldReturn` "log"
    count (byRole page "listitem") `shouldReturn` 3
    count (byRole (locator page "ul") "listitem") `shouldReturn` 3
    innerText (byText page "hover") `shouldReturn` "hovered"
    outerHTML (locator page "#disabled") `shouldReturn` "<button id=\"disabled\" disabled=\"\">off</button>"
    isVisible (locator page "#hidden") `shouldReturn` False
    shouldBeHidden (locator page "#hidden")
    shouldBeVisible (locator page "#count")
    waitFor (locator page "#hidden") Attached
    waitFor (locator page "#hidden") Hidden
    waitFor (locator page "#nothing-here") Detached
    dragTo (locator page "#source") (locator page "#target")
    locator page "#target" `shouldHaveText` "dropped"
    locator page "h1" `shouldContainText` "asic"
    page' <- content page
    Text.isInfixOf "<h1>Basics</h1>" page' `shouldBe` True
    evaluate page "1 + 1" `shouldReturn` (2 :: Int)
    evaluate page "Promise.resolve('later')" `shouldReturn` ("later" :: Text)
    evaluate page "undefined" `shouldReturn` (Nothing :: Maybe Int)
    evaluateWith page "(n) => n + 1" (41 :: Int) `shouldReturn` (42 :: Int)
    evaluateWith page "({a, b}) => a + b" (Pair 1 2) `shouldReturn` (3 :: Int)
    -- What the page does on a timer is not the program's: eventually.
    click (locator page "#later")
    textContent (locator page "#later") `shouldReturn` "soon"
    eventually page (textContent (locator page "#later") `shouldReturn` "later")
    withSystemTempDirectory "playwright-shot" $ \dir -> do
      screenshot page (dir </> "page.png")
      doesFileExist (dir </> "page.png") `shouldReturn` True
    url page `shouldReturn` at "basics.html"
    shouldHaveURL page (at "basics.html")

  -- screenshots.mjs: a picture of the window, 1280 by 800, not of the
  -- whole page; the smoke tests' failures: of the whole page.
  it "takes a picture of the window, or of the whole page" $ \pw ->
    withContext pw defaultContextOptions {viewport = Just (640, 400)} $ \ctx -> do
      page <- newPage ctx
      goto page (at "long.html")
      withSystemTempDirectory "playwright-shot" $ \dir -> do
        screenshotWith page defaultScreenshotOptions {fullPage = False} (dir </> "window.png")
        pngSize (dir </> "window.png") `shouldReturn` (640, 400)
        screenshot page (dir </> "page.png")
        (width, height) <- pngSize (dir </> "page.png")
        width `shouldBe` 640
        height `shouldSatisfy` (>= 3000)

  -- smoke-test-server.mjs: the status a page was served with, for every
  -- address of the walk (a 404 page among them, a page a visitor may not
  -- see), and with JavaScript off.
  it "gives the status a navigation's response came with" $ \pw -> do
    withPage pw $ \page -> do
      gotoStatus page (at "basics.html") `shouldReturn` Just 200
      gotoStatus page (at "no-such-page.html") `shouldReturn` Just 404
      _ <- route page "**/private.html" (\_ -> pure (fulfill 401 (Just "text/html") "<p>Not yours</p>"))
      gotoStatus page (at "private.html") `shouldReturn` Just 401
      locator page "p" `shouldHaveText` "Not yours"
      -- Within the document: no response.
      gotoStatus page (at "private.html#further") `shouldReturn` Nothing
    withContext pw defaultContextOptions {javaScriptEnabled = False} $ \ctx -> do
      page <- newPage ctx
      gotoStatus page (at "basics.html") `shouldReturn` Just 200

  -- smoke-test.mjs and smoke-test-server.mjs: a version picked by its whole
  -- name, the last diff line, every item's text, and a control disabled
  -- until a choice is kept.
  it "filters by a whole text, reads every match's text, takes the last, and says what is disabled" $ \pw -> withPage pw $ \page -> do
    goto page (at "versions.html")
    let versions = locator page ".version-list li > a"
    count (withText "Version 2" versions) `shouldReturn` 2
    count (withTextExact "Version 2" versions) `shouldReturn` 1
    count (withTextExact "version 2" versions) `shouldReturn` 0
    click (withTextExact "Version 2" versions)
    locator page "#shown" `shouldHaveText` "Version 2"
    click (withTextExact "Version 12" versions)
    locator page "#shown" `shouldHaveText` "Version 12"
    allTextContents versions `shouldReturn` ["Version 12 (current)", "Version 12", "Version 2 (current)", "Version 2", "Version 1"]
    allTextContents (locator page ".nothing") `shouldReturn` []
    textContent (lastOf (locator page ".diff-text")) `shouldReturn` "A third line."
    isDisabled (locator page "#effort") `shouldReturn` True
    click (locator page "#keep")
    -- The page's own timer, not a program's.
    eventually page (isDisabled (locator page "#effort") `shouldReturn` False)

  -- smoke-test.mjs and screenshots.mjs: JavaScript whose promise may never
  -- settle (a cache in a worker that broke) fails after the timeout rather
  -- than holding the test for ever, in every form.
  it "fails evaluated JavaScript that never settles, after the timeout" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    let quick = withTimeout 300 page
        never = "new Promise(() => {})"
        timesOutQuickly :: IO Value -> IO ()
        timesOutQuickly io = do
          started <- getMonotonicTime
          try io >>= \case
            Left (e :: PlaywrightError) -> do
              e.name `shouldBe` "TimeoutError"
              Text.unpack e.message `shouldContain` "300ms"
            Right v -> expectationFailure ("it answered " <> show v)
          took <- subtract started <$> getMonotonicTime
          took `shouldSatisfy` (\t -> t >= 0.29 && t < 1.5)
    timesOutQuickly (evaluate quick never)
    timesOutQuickly (evaluateWith quick "() => new Promise(() => {})" ())
    timesOutQuickly (evaluateOn (locator quick "#count") "() => new Promise(() => {})" ())
    timesOutQuickly (evaluateAll (locator quick ".item") "() => new Promise(() => {})" ())
    -- One that settles in time is answered, and the page still is.
    evaluate quick "new Promise((r) => setTimeout(() => r(7), 50))" `shouldReturn` (7 :: Int)
    textContent (locator page "#count") `shouldReturn` "0"

  it "fails a retrying expectation with what it last read" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    result <- try (locator page "#count" `shouldHaveText` "7")
    case result of
      Left (HUnitFailure _ (ExpectedButGot what expected actual)) -> do
        what `shouldBe` Just "the text of css \"#count\""
        expected `shouldBe` "\"7\""
        actual `shouldBe` "\"0\""
      Left other -> expectationFailure ("another failure: " <> show other)
      Right () -> expectationFailure "the expectation held"

  it "times out with Playwright's message, which names the locator" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    try (click (locator page "#missing")) >>= \case
      Left (e :: PlaywrightError) -> do
        e.method `shouldBe` "click"
        e.name `shouldBe` "TimeoutError"
        Text.unpack e.message `shouldContain` "#missing"
      Right () -> expectationFailure "the click found something"

  it "gives one page handle a timeout of its own, for its calls and its locators'" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    let quick = withTimeout 300 page
    started <- getMonotonicTime
    try (click (locator quick "#missing")) >>= \case
      Left (e :: PlaywrightError) -> e.name `shouldBe` "TimeoutError"
      Right () -> expectationFailure "the click found something"
    took <- subtract started <$> getMonotonicTime
    -- Well under the suite's two seconds.
    took `shouldSatisfy` (< 1.5)
    -- The retrying expectations read it too.
    started' <- getMonotonicTime
    try (locator quick "#count" `shouldHaveText` "7") >>= \case
      Left (_ :: HUnitFailure) -> pure ()
      Right () -> expectationFailure "the expectation held"
    took' <- subtract started' <$> getMonotonicTime
    took' `shouldSatisfy` (< 1.5)
    -- And a longer one lets a call wait past the suite's.
    _ :: Maybe Value <- evaluate page "(setTimeout(() => { document.getElementById('late').textContent = 'here'; }, 2500), null)"
    waitFor (locator (withTimeout 6000 page) "#late:text-is('here')") Attached
    -- The original handle is unchanged.
    try (waitFor (locator page "#late:text-is('never')") Attached) >>= \case
      Left (e :: PlaywrightError) -> Text.unpack e.message `shouldContain` "2000ms"
      Right () -> expectationFailure "the wait returned"

  it "waits for a JavaScript expression to hold, and runs a function on a locator's elements" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    click (locator page "#later")
    -- The page's own timer, not a program's: settle did not wait for it.
    textContent (locator page "#later") `shouldReturn` "soon"
    waitForFunction page "document.getElementById('later').textContent === 'later'"
    textContent (locator page "#later") `shouldReturn` "later"
    try (waitForFunction (withTimeout 300 page) "false") >>= \case
      Left (e :: PlaywrightError) -> e.name `shouldBe` "TimeoutError"
      Right () -> expectationFailure "waitForFunction returned"
    evaluateAll (locator page ".item") "(items, suffix) => items.map((item) => item.textContent + suffix)" ("!" :: Text)
      `shouldReturn` ["one!", "two!", "three!" :: Text]
    evaluateAll (locator page ".nothing") "(items) => items.length" () `shouldReturn` (0 :: Int)

  it "runs an init script of the test's own in every page of a context, after the probe" $ \pw -> do
    withContext pw defaultContextOptions $ \ctx -> do
      addInitScript ctx "window.seenProbe = typeof globalThis.__halogenProbe; window.Worker = undefined;"
      page <- newPage ctx
      goto page (at "basics.html")
      evaluate page "window.seenProbe" `shouldReturn` ("object" :: Text)
      evaluate page "typeof Worker" `shouldReturn` ("undefined" :: Text)
      other <- newPage ctx
      goto other (at "plain.html")
      evaluate other "window.seenProbe" `shouldReturn` ("object" :: Text)
    withPage pw $ \page -> do
      goto page (at "basics.html")
      evaluate page "typeof window.seenProbe" `shouldReturn` ("undefined" :: Text)

  it "keeps a context's requests, its workers' among them, and counts a page's workers" $ \pw ->
    withContext pw defaultContextOptions $ \ctx -> do
      page <- newPage ctx
      goto page (at "workers.html")
      workers page `shouldReturn` 0
      click (locator page "#start")
      -- The worker is the page's, but not its program's: settle did not
      -- wait for it to start or to answer.
      locator page "#said" `shouldHaveText` "fetched"
      workers page `shouldReturn` 1
      asked <- requests ctx
      let request path = filter (\r -> r.url == at path) asked
      map (\r -> (r.method, r.resourceType)) (request "workers.html") `shouldBe` [("GET", "document")]
      map (.resourceType) (request "worker.js") `shouldBe` ["script"]
      -- The worker's own fetch, which no page made.
      map (.resourceType) (request "plain.html") `shouldBe` ["fetch"]
      click (locator page "#stop")
      eventually page (workers page `shouldReturn` 0)
      -- Another context's requests are its own.
      withContext pw defaultContextOptions $ \other -> do
        page' <- newPage other
        goto page' (at "plain.html")
        map (.url) <$> requests other `shouldReturn` [at "plain.html"]

  -- What the playground's test prints for the tutorial's measurements: the
  -- renderers' memory, which a page cannot read from inside.
  it "lists the browser's processes, a renderer among them, with their memory" $ \pw -> withPage pw $ \page -> do
    hasProc <- doesDirectoryExist "/proc"
    unless hasProc $ pendingWith "no /proc here, where the processes are read from"
    goto page (at "basics.html")
    processes <- browserProcesses page
    map (.kind) processes `shouldContain` ["browser"]
    let renderers = filter (\p -> p.kind == "renderer") processes
    renderers `shouldSatisfy` (not . null)
    -- A renderer that has run a page holds megabytes, and never more now
    -- than at its peak.
    forM_ renderers $ \r -> do
      r.rss `shouldSatisfy` (> 1_000_000)
      r.peak `shouldSatisfy` (>= r.rss)

  it "keeps two contexts' storage apart" $ \pw ->
    withPage pw $ \one -> withPage pw $ \two -> do
      goto one (at "basics.html")
      () <- evaluate one "localStorage.setItem('mark', 'one') ?? null" >>= \(_ :: Maybe Value) -> pure ()
      reload one
      locator one "#storage" `shouldHaveText` "one"
      goto two (at "basics.html")
      locator two "#storage" `shouldHaveText` "none"

  it "drives two pages of one context at once, from two threads" $ \pw ->
    withContext pw defaultContextOptions {baseURL = Just base} $ \ctx -> do
      one <- newPage ctx
      two <- newPage ctx
      goto one "/basics.html"
      goto two "/basics.html"
      _ <- concurrently (replicateM_ 10 (click (locator one "#count"))) (replicateM_ 15 (click (locator two "#count")))
      locator one "#count" `shouldHaveText` "10"
      locator two "#count" `shouldHaveText` "15"
      -- One context: one storage.
      () <- evaluate one "localStorage.setItem('mark', 'shared') ?? null" >>= \(_ :: Maybe Value) -> pure ()
      reload two
      locator two "#storage" `shouldHaveText` "shared"
      -- Many calls in flight on one page.
      replicateConcurrently_ 8 (textContent (locator one "#count") `shouldReturn` "10")

  it "keeps what a page writes to its console and what it throws" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    click (locator page "#log")
    eventually page $ do
      messages <- consoleMessages page
      map (\m -> (m.kind, m.text)) messages `shouldContain` [("log", "logged"), ("error", "an error")]
    click (locator page "#throw")
    eventually page $ do
      errors <- pageErrors page
      any (Text.isInfixOf "thrown on purpose") errors `shouldBe` True

  it "goes back and forward" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    click (locator page "#next")
    shouldHaveURL page (at "second.html")
    locator page "h1" `shouldHaveText` "Second"
    goBack page
    url page `shouldReturn` at "basics.html"
    goForward page
    url page `shouldReturn` at "second.html"

  it "sets the viewport, the colour scheme, JavaScript, the network and motion" $ \pw -> do
    let options = defaultContextOptions {baseURL = Just base, viewport = Just (500, 400), colorScheme = Just Dark}
    withContext pw options $ \ctx -> do
      page <- newPage ctx
      goto page "/basics.html"
      evaluate page "innerWidth" `shouldReturn` (500 :: Int)
      locator page "#scheme" `shouldHaveText` "dark"
      setViewport page 300 200
      evaluate page "innerWidth" `shouldReturn` (300 :: Int)
    withContext pw defaultContextOptions {baseURL = Just base, colorScheme = Just Light, javaScriptEnabled = False} $ \ctx -> do
      page <- newPage ctx
      goto page "/basics.html"
      -- The page's script did not run.
      locator page "#scheme" `shouldHaveText` ""
    withContext pw defaultContextOptions {baseURL = Just base} $ \ctx -> do
      page <- newPage ctx
      goto page "/basics.html"
      evaluate page "navigator.onLine" `shouldReturn` True
    withContext pw defaultContextOptions {offline = True} $ \ctx -> do
      page <- newPage ctx
      evaluate page "navigator.onLine" `shouldReturn` False
    -- screenshots.mjs: motion reduced, so the pictures show what animates
    -- at its end.
    withContext pw defaultContextOptions {reducedMotion = True} $ \ctx -> do
      page <- newPage ctx
      goto page (at "basics.html")
      evaluate page "matchMedia('(prefers-reduced-motion: reduce)').matches" `shouldReturn` True
    withPage pw $ \page -> do
      goto page (at "basics.html")
      evaluate page "matchMedia('(prefers-reduced-motion: reduce)').matches" `shouldReturn` False

  it "fails an action on a closed page" $ \pw -> withContext pw defaultContextOptions $ \ctx -> do
    page <- newPage ctx
    goto page (at "basics.html")
    closePage page
    try (click (locator page "#count")) >>= \case
      Left (e :: PlaywrightError) -> e.name `shouldBe` "ClosedError"
      Right () -> expectationFailure "the click went through"

  it "fails, rather than hangs, once the driver has died" $ \_ -> do
    options <- optionsFromEnvironment
    withPlaywright options $ \pw -> withPage pw $ \page -> do
      goto page (at "basics.html")
      killDriver pw
      outcome <- timeout 10_000_000 (try (click (locator page "#count")))
      case outcome of
        Nothing -> expectationFailure "the call hung"
        Just (Left (e :: PlaywrightError)) -> e.name `shouldBe` "DriverGone"
        Just (Right ()) -> expectationFailure "the call went through"
    -- And so does a call after withPlaywright has ended.
    escaped <- withPlaywright options pure
    try (newContext escaped defaultContextOptions) >>= \case
      Left (e :: PlaywrightError) -> e.name `shouldBe` "DriverGone"
      Right _ -> expectationFailure "the call went through"

  -- With what can keep a process busy in flight when it is killed: a
  -- route's handler holding a request, and a download under way, whose
  -- server has sent part of it and waits.
  it "leaves no node and no Chromium behind when the caller is killed, routes and a download in flight" $ \_ -> withSlowDownload $ \slow streaming -> do
    options <- optionsFromEnvironment
    pids <- newEmptyMVar
    held <- newEmptyMVar
    finished <- newEmptyMVar
    caller <- forkIO $ do
      _ <- try @SomeException $ withPlaywright options $ \pw -> withPage pw $ \page -> do
        goto page (at "basics.html")
        _ <- route page "**/second.html" $ \_ -> putMVar held () >> threadDelay 60_000_000 >> pure Continue
        _ :: Value <- evaluate page "(fetch('second.html'), null)"
        takeMVar held
        node <- maybe (throwIO (userError "no node")) pure =<< driverProcessId pw
        family <- descendants node
        putMVar pids (node : family)
        -- Waits for the download to end, which it does not.
        _ <- download (withTimeout 60_000 page) (goto page slow)
        threadDelay 60_000_000
      putMVar finished ()
    family <- maybe (expectationFailure "the driver did not start" >> pure []) pure =<< timeout 30_000_000 (takeMVar pids)
    -- Chromium is among them: node's only children are the browser's.
    names <- catMaybes <$> mapM commandOf family
    any ("headless" `isPrefixOf`) names || any ("chrom" `isPrefixOf`) names `shouldBe` True
    timeout 30_000_000 (readMVar streaming) `shouldReturn` Just ()
    killThread caller
    timeout 30_000_000 (takeMVar finished) `shouldReturn` Just ()
    let waitGone tries = do
          alive <- filterM isAlive family
          unless (null alive || tries <= (0 :: Int)) $ threadDelay 100_000 >> waitGone (tries - 1)
    waitGone 50
    filterM isAlive family `shouldReturn` []

-- | A server whose one address is a download that never ends: it sends a
-- part, says so, and waits. The body gets the address and the signal.
withSlowDownload :: (Text -> MVar () -> IO a) -> IO a
withSlowDownload body = do
  streaming <- newEmptyMVar
  let app _ respond =
        respond $ Wai.responseStream HTTP.status200 [("Content-Type", "application/octet-stream"), ("Content-Disposition", "attachment; filename=slow.bin"), ("Content-Length", "10000000")] $ \write flush -> do
          write (Builder.byteString (ByteString.replicate 65536 0))
          flush
          _ <- tryPutMVar streaming ()
          threadDelay 60_000_000
  Warp.testWithApplication (pure app) $ \port -> body ("http://127.0.0.1:" <> Text.pack (show port) <> "/slow.bin") streaming

-- | A PNG's width and height, from its header.
pngSize :: FilePath -> IO (Int, Int)
pngSize path = do
  bytes <- ByteString.readFile path
  let word32 at = foldl (\n i -> n * 256 + fromIntegral (ByteString.index bytes (at + i))) 0 [0 .. 3]
  pure (word32 16, word32 20)

-- | A record to send as an argument.
data Pair = Pair Int Int

instance ToJSON Pair where
  toJSON (Pair a b) = object ["a" .= a, "b" .= b]

commandOf :: ProcessID -> IO (Maybe String)
commandOf pid = either (const Nothing) (Just . takeWhile (/= '\n')) <$> try @SomeException (readProcFile pid "comm")

-- | Whether the process exists and is not a zombie waiting to be reaped.
isAlive :: ProcessID -> IO Bool
isAlive pid =
  statFields pid >>= \case
    state : _ -> pure (state /= "Z" && state /= "X")
    [] -> pure False
