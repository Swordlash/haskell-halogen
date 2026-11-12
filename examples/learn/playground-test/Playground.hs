-- | The in-browser playground (learn/) in headless Chromium, on the rootfs
-- toolchain/build-learn-rootfs.sh built: GHC loads in the page, compiles the
-- default program (a Halogen counter), and the counter it mounts counts; a
-- second Run replaces it, and so does a third of an edited program. Then,
-- for each layer the build has (rootfs.json), the page loads it and runs a
-- program that uses it (playground-test/programs/<layer>.hs), which loads
-- its libraries and calls their JavaScript: a hooks counter, a router link,
-- a fetch from this server, a custom element defined over the page's HTML,
-- the DevTools panel, a Material button with its CSS, a Pixi scene on a
-- canvas, a Three.js scene, an hh quote from haskell-halogen-th, an
-- hspec-halogen spec, whose report is the program's output, a store, jobs
-- of haskell-halogen-worker, run in a Web Worker that is a second compiler
-- with the program compiled in it, a component from a library linked while
-- the program runs, and one sent over the live layer's wire to a client in
-- the page.
--
-- > npm run test-playground                        # after: npm run build-rootfs
-- > LEARN_ONLY=router,net npm run test-playground  # the base runs, and only those layers
--
-- It builds nothing. It serves the rootfs (@dist-newstyle/wasm/learn@ from
-- the repository's root, or the directory @LEARN_ROOTFS_DIR@ names), as a
-- static host does, and fails on any error in the page. @LEARN_TIMEOUT@ is
-- the seconds loading the compiler, a run or a layer may take (300: loading
-- takes seconds and the first run tens of them on a fast machine; this is
-- generous, for slow CI runners), and @LEARN_QUERY@ the page's query, for
-- trying other RTS options (@LEARN_QUERY='rts=-c -H64m'@).
--
-- It prints what the page measured (fetch, extract, loading the compiler,
-- each layer, each run), the page's JS heap after each run, and how much
-- memory the page's renderer process used, now and at its peak: these
-- numbers go into learn/README.md's tables. The pixi and three layers load
-- PixiJS and Three.js from jsDelivr, so those runs need the network; if
-- jsDelivr cannot be reached, they are pending and say why. Everything else
-- the page loads is the rootfs'.
--
-- How it waits. Compiling is the page's own work, in its JavaScript, not a
-- program's: a run is waited for on the page's signal, @window.learn.runs@,
-- with the long timeout. What a run mounts is a Halogen program, linked
-- against the rootfs' own haskell-halogen-core, which answers the driver's
-- probe (the probe's object is there from the page's start, and the
-- program's runtime finds it when it mounts its first tree, much later,
-- inside GHC's dynamic linker). So once a run has ended, the program is
-- settled (or quiet, where it waits in a @liftIO@: a fetch, a library
-- linked, a job in a worker), a click on what it mounted is settled when
-- 'click' returns, and what it shows is read once, with no retry. What is
-- neither (PixiJS and Three.js arriving from the CDN) is waited for as a
-- condition with a deadline, saying why.
--
-- The examples share one page ('onOnePage'), in order: each goes on from
-- the state the one before left (the compiler loaded, the last run's tree
-- for the next run to end), so one of them cannot be run alone with
-- @--match@; @LEARN_ONLY@ chooses the layers instead, and the compiler's
-- load and the three runs of the counter always come first. Without the
-- rootfs the examples are pending, with the command that builds it, unless
-- @HALOGEN_EXAMPLE_REQUIRED=1@ (as @npm run test-playground@ sets it), when
-- they fail.
module Main (main) where

import Control.Monad (forM_, unless, when)
import Data.Aeson (FromJSON (..), Value, withObject, (.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (isDigit)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import GHC.Clock (getMonotonicTime)
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Halogen.Playwright.Serve (defaultServeOptions, staticApplication)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Printf (printf)
import Text.Read (readMaybe)

main :: IO ()
main = do
  root <- fromMaybe "../../dist-newstyle/wasm/learn" <$> lookupEnv "LEARN_ROOTFS_DIR"
  required <- (== Just "1") <$> lookupEnv "HALOGEN_EXAMPLE_REQUIRED"
  built <- doesFileExist (root </> "rootfs.json")
  if built
    then do
      manifest :: Manifest <- either (\e -> fail ("rootfs.json: " <> e)) pure =<< Aeson.eitherDecodeFileStrict (root </> "rootfs.json")
      only <- maybe Nothing (Just . filter (not . Text.null) . Text.splitOn "," . Text.pack) <$> lookupEnv "LEARN_ONLY"
      seconds <- maybe 300 (fromMaybe 300 . readMaybe) <$> lookupEnv "LEARN_TIMEOUT"
      query <- maybe "" Text.pack <$> lookupEnv "LEARN_QUERY"
      walk <- newWalk (seconds * 1000) query
      let chosen = [l | l <- manifest.layers, maybe True (l.name `elem`) only]
      serve root $ \base -> hspec (spec walk {base} chosen)
    else
      hspec
        $ describe "the playground (learn/), on the rootfs"
        $ it "is served"
        $ if required
          then expectationFailure (root </> "rootfs.json" <> " is missing: build the rootfs with npm run build-rootfs")
          else pendingWith ("no " <> root </> "rootfs.json" <> ": build the rootfs with npm run build-rootfs, or run npm run test-playground")

-- | The rootfs served as a static host serves it, and nothing else: no
-- COOP/COEP headers, so the page is not cross-origin isolated and has no
-- SharedArrayBuffer. It also answers learn-test/hello.txt, which the net
-- layer's program fetches: a fetch from the page's own server.
serve :: FilePath -> (Text -> IO a) -> IO a
serve root body = Warp.testWithApplication (pure app) $ \port ->
  body ("http://127.0.0.1:" <> Text.pack (show port) <> "/")
  where
    app request respond
      | Wai.pathInfo request == ["learn-test", "hello.txt"] =
          respond (Wai.responseLBS status200 [(hContentType, "text/plain")] (LazyByteString.fromStrict (Text.encodeUtf8 hello)))
      | otherwise = staticApplication defaultServeOptions root request respond

-- | What the test server says at learn-test/hello.txt.
hello :: Text
hello = "Hello from the test server."

----------------------------------------------------------------------
-- The rootfs' manifest

-- | What the test reads of rootfs.json: the layers, in the order the page
-- shows them.
newtype Manifest = Manifest {layers :: [Layer]}

instance FromJSON Manifest where
  parseJSON = withObject "rootfs.json" $ \o -> Manifest <$> o .: "layers"

-- | A layer: its name, and what it loads from another site while a program
-- runs (its @external@ entries' addresses).
data Layer = Layer {name :: Text, external :: [Text]}

instance FromJSON Layer where
  parseJSON = withObject "layer" $ \o -> do
    name <- o .: "name"
    entries <- o .:? "external" .!= []
    external <- traverse (withObject "external" (.: "url")) entries
    pure Layer {name, external}

----------------------------------------------------------------------
-- The walk's state, kept across its examples

data Walk = Walk
  { base :: Text
  , timeout :: Int
  -- ^ What loading the compiler, a run or a layer may take, in ms.
  , query :: Text
  , loaded :: IORef Bool
  -- ^ Whether the compiler loaded: nothing after it can run otherwise.
  , runs :: IORef Int
  , heaps :: IORef (Map Text Double)
  -- ^ The page's JS heap after each run, as DevTools counts it.
  , renderers :: IORef [(Text, [BrowserProcess])]
  -- ^ The renderers' memory at the same moments.
  , layerTimes :: IORef [(Text, Int, Maybe Int)]
  -- ^ Each layer's load, and its first run, in ms.
  , relayed :: IORef Int
  -- ^ How many of the page's console messages have been looked at for the
  -- page's own lines (@learn: ...@), which are printed as they come.
  }

newWalk :: Int -> Text -> IO Walk
newWalk timeout query = do
  loaded <- newIORef False
  runs <- newIORef 0
  heaps <- newIORef Map.empty
  renderers <- newIORef []
  layerTimes <- newIORef []
  relayed <- newIORef 0
  pure Walk {base = "", timeout, query, loaded, runs, heaps, renderers, layerTimes, relayed}

-- | The page with the walk's long timeout, for what the compiler does.
long :: Walk -> Page -> Page
long walk = withTimeout walk.timeout

-- | An example of the walk. Its page's calls have 30 s, not the driver's
-- five: the programs run as bytecode in GHC's interpreter, slower than
-- compiled ones, on a CI runner slower still, and every action on them
-- settles. Nothing here waits that long unless something is wrong.
step :: String -> (Page -> IO ()) -> SpecWith Page
step name body = it name (body . withTimeout 30000)

-- | Nothing after the compiler's load can run if it did not load, and that
-- example has failed already.
requireLoaded :: Walk -> IO ()
requireLoaded walk = do
  ok <- readIORef walk.loaded
  unless ok $ pendingWith "the compiler did not load (see the first example)"

-- | A line of the report.
report :: Text -> IO ()
report line = Text.putStrLn ("    ok  " <> line)

-- | Print the page's own lines (@learn: fetch ...: 267 ms@) that it has
-- written since the last time, as the page measured each step.
relay :: Walk -> Page -> IO ()
relay walk page = do
  messages <- consoleMessages page
  seen <- readIORef walk.relayed
  writeIORef walk.relayed (length messages)
  forM_ (drop seen messages) $ \m ->
    when ("learn:" `Text.isPrefixOf` m.text) $ Text.putStrLn ("    " <> m.text)

-- | Nothing logged as an error, nothing thrown, so far.
noErrors :: Page -> Text -> IO ()
noErrors page when' = do
  logged <- filter (\m -> m.kind == "error") <$> consoleMessages page
  thrown <- pageErrors page
  unless (null logged && null thrown) $
    expectationFailure ("errors in the page " <> Text.unpack when' <> ": " <> show (map (.text) logged <> thrown))

-- | The page's JS heap, as DevTools counts it (performance.memory, which
-- the page reports, is rounded and lags), after a collection: what the page
-- keeps. The extracted archive's files are on it. And the renderers'
-- memory, which no page can read from inside.
measure :: Walk -> Page -> Text -> IO ()
measure walk page when' = do
  rs <- filter (\p -> p.kind == "renderer") <$> browserProcesses page
  unless (null rs) $ modifyIORef' walk.renderers (<> [(when', rs)])
  _ :: Value <- cdp page "HeapProfiler.collectGarbage" ()
  Metrics metrics <- cdp page "Performance.getMetrics" ()
  forM_ (lookup "JSHeapUsedSize" metrics) $ \heap -> modifyIORef' walk.heaps (Map.insert when' heap)

newtype Metrics = Metrics [(Text, Double)]

instance FromJSON Metrics where
  parseJSON = withObject "metrics" $ \o -> do
    ms <- o .: "metrics"
    Metrics <$> traverse (withObject "metric" (\m -> (,) <$> m .: "name" <*> m .: "value")) ms

----------------------------------------------------------------------
-- A run

-- | Press Run and wait for the page to say the run is over: compiling is
-- the page's work, so this waits on its count of runs, with the long
-- timeout. The run must have gone through (the page records its time),
-- with nothing on GHC's stderr and nothing wrong in the page. Its number.
run :: Walk -> Page -> IO Int
run walk page = do
  n <- atomicModifyIORef' walk.runs (\k -> (k + 1, k + 1))
  -- The click's settle has the long timeout too: what the page does on a
  -- run holds its thread for seconds at a time.
  click (locator (long walk page) "#run:enabled")
  waitForFunctionWith (long walk page) "(n) => window.learn.runs >= n" n
  (timings :: Map Text Int, stderr :: Text) <- evaluate page "[window.learn.timings, document.getElementById('stderr').textContent]"
  let name = "run " <> tshow n
  unless (Map.member name timings) $ do
    status <- textContent (locator page "#status")
    expectationFailure (Text.unpack (name <> " failed: " <> status <> "\n" <> stderr))
  unless (Text.null stderr) $ expectationFailure (Text.unpack (name <> ": GHC wrote to stderr:\n" <> stderr))
  noErrors page ("in " <> name)
  measure walk page name
  relay walk page
  pure n

-- | What the program now in the page says, which it answers through
-- haskell-halogen-core's probe: this many trees, idle.
answers :: Page -> Int -> IO ()
answers page trees = do
  a <- activity page
  case a of
    Just reading | reading.trees == trees && reading.running == 0 -> pure ()
    _ -> expectationFailure ("the program compiled in the page answers the probe with " <> show trees <> " tree(s), idle: " <> show a)

-- | An element's whole text, read once: the program has finished what it
-- was doing when this is called.
hasText :: Locator -> Text -> IO ()
hasText l expected = do
  actual <- textContent l
  unless (actual == expected) $ expectationFailure ("the element's text, read once the program had finished, is " <> show actual <> ", not " <> show expected)

-- | What #app shows has this in it, read once.
appShows :: Page -> Text -> IO ()
appShows page expected = do
  actual <- textContent (locator page "#app")
  unless (expected `Text.isInfixOf` actual) $
    expectationFailure ("#app, read once the program had finished, shows " <> show actual <> ", without " <> show expected)

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

----------------------------------------------------------------------

spec :: Walk -> [Layer] -> Spec
spec walk layers = playwright $ describe "the playground (learn/), on the rootfs" $ onOnePage defaultContextOptions (\_ -> pure ()) $ do
  step "loads the compiler, without cross-origin isolation" $ \page -> do
    -- The page's query as the browser writes one (LEARN_QUERY='rts=-c
    -- -H64m' is ?rts=-c+-H64m).
    query <- if Text.null walk.query then pure "" else evaluateWith page "(q) => '?' + new URLSearchParams(q)" walk.query
    goto (long walk page) (walk.base <> query)
    _ :: Value <- cdp page "Performance.enable" ()
    -- Loading is the page's: fetching and extracting the archive, and
    -- dyld loading GHC. No program runs in the page yet.
    waitForFunction (long walk page) "window.learn?.ready || window.learn?.error"
    relay walk page
    failed :: Maybe Text <- evaluate page "window.learn.error"
    forM_ failed $ \e -> expectationFailure ("the page failed to load the compiler: " <> Text.unpack e)
    noErrors page "while loading"
    isolated <- evaluate page "crossOriginIsolated || typeof SharedArrayBuffer !== 'undefined'"
    unless (not isolated) $ expectationFailure "the page is cross-origin isolated"
    report "the compiler loaded, without cross-origin isolation"
    measure walk page "loaded"
    writeIORef walk.loaded True

  step "run 1 compiles the counter and mounts it, which answers the probe" $ \page -> do
    requireLoaded walk
    _ <- run walk page
    -- The program's main has mounted the counter by the time the run ends,
    -- and its runtime has found the probe the driver set before the page's
    -- scripts ran: it answers, with its one tree, idle.
    answers page 1
    hasText (locator page "#app button") "Clicked 0 times"
    report "run 1 compiled the counter and mounted it, and the program answers the probe"

  step "the interpreted counter counts clicks" $ \page -> do
    requireLoaded walk
    -- Each click is settled: the program's handler has run and rendered.
    click (locator page "#app button")
    click (locator page "#app button")
    hasText (locator page "#app button") "Clicked 2 times"
    report "the interpreted counter counts clicks"

  -- A second Run ends the first run's tree (Halogen.IO.Util.endPreviousRun)
  -- before it mounts its own, so there is one counter, a new one, at 0.
  step "run 2 replaces the counter with a new one, which counts" $ \page -> do
    requireLoaded walk
    _ :: Value <- evaluateOn (locator page "#app button") "(button) => { button.dataset.first = '1'; return null; }" ()
    _ <- run walk page
    -- The first tree was disposed of: the program answers with one.
    answers page 1
    hasText (locator page "#app button") "Clicked 0 times"
    count (locator page "#app button") `shouldReturn` 1
    count (locator page "#app button[data-first]") `shouldReturn` 0
    click (locator page "#app button")
    hasText (locator page "#app button") "Clicked 1 times"
    report "run 2 replaced the counter with a new one, which counts"

  -- Run 2 compiled the same source again, which GHC finds unchanged; an
  -- edit is what a reader does, and has Main compiled anew.
  step "run 3 compiles the edited program and replaces the counter" $ \page -> do
    requireLoaded walk
    _ :: Value <- evaluateOn (locator page "#source") "(area) => { area.value = area.value.replace('\"Clicked \"', '\"Pressed \"'); return null; }" ()
    _ <- run walk page
    answers page 1
    hasText (locator page "#app button") "Pressed 0 times"
    count (locator page "#app button") `shouldReturn` 1
    report "run 3 compiled the edited program and replaced the counter"

  -- The layers, each in an archive and a package DB of its own, which the
  -- page extracts on top of the base one when asked to and passes to GHC.
  forM_ layers $ \layer -> layerStep walk layer (find (\p -> p.layer == layer.name) programs)

  step "said nothing wrong" $ \page -> do
    requireLoaded walk
    noErrors page "at the end"
    report "no errors in the page"

  step "prints what the page measured" $ \page -> do
    requireLoaded walk
    printMeasurements walk page

-- | A layer's example: load it, by its button, and wait for the page to
-- have it (the page's work: fetching, extracting, adding its assets); then
-- run its program and check what it shows.
layerStep :: Walk -> Layer -> Maybe Program -> SpecWith Page
layerStep walk layer program =
  step (Text.unpack ("the " <> layer.name <> " layer: " <> maybe "only loaded, with no program" (.what) program)) $ \page -> do
    requireLoaded walk
    -- A layer whose program loads a module from a CDN itself cannot run
    -- where the CDN cannot be reached.
    forM_ (program >>= (.needs)) $ \lib -> forM_ (take 1 layer.external) $ \cdn -> do
      reachable <- canReach page cdn
      unless reachable $
        pendingWith (Text.unpack (cdn <> " cannot be reached from the browser, and the " <> layer.name <> " layer loads " <> lib <> " from there"))
    click (locator page ("#layer-" <> layer.name <> ":enabled"))
    waitForFunctionWith (long walk page) "(name) => window.learn.layers[name]" layer.name
    relay walk page
    noErrors page ("while loading the " <> layer.name <> " layer")
    loadMs :: Int <- evaluateWith page "(name) => window.learn.layers[name].ms" layer.name
    case program of
      Nothing -> do
        modifyIORef' walk.layerTimes (<> [(layer.name, loadMs, Nothing)])
        pendingWith (Text.unpack ("no program for the " <> layer.name <> " layer: only loaded it"))
      Just p -> do
        source <- Text.readFile ("playground-test/programs" </> Text.unpack layer.name <> ".hs")
        fill (locator page "#source") source
        n <- run walk page
        p.verify walk page
        noErrors page ("in the " <> layer.name <> " layer's run")
        runMs :: Int <- evaluateWith page "(n) => window.learn.timings['run ' + n]" n
        modifyIORef' walk.layerTimes (<> [(layer.name, loadMs, Just runMs)])
        report ("run " <> tshow n <> ": " <> p.what)

-- | Whether the browser can fetch an address of another site. Asked from a
-- page of its own: a fetch that fails logs a console error, which would
-- count as an error of the walk's page.
canReach :: Page -> Text -> IO Bool
canReach page address = do
  probe <- newPage (pageContext page)
  reachable <- evaluateWith probe "(address) => fetch(address).then((r) => r.ok, () => false)" address
  closePage probe
  pure reachable

----------------------------------------------------------------------
-- The layers' programs

-- | A layer's program (in playground-test/programs/<layer>.hs), and what
-- shows that it worked: something that loads the layer's libraries and
-- calls into their JavaScript. Each replaces the last run's tree, as the
-- counter does.
data Program = Program
  { layer :: Text
  , what :: Text
  , needs :: Maybe Text
  -- ^ What the program loads from the layer's CDN, if it cannot run
  -- without it.
  , verify :: Walk -> Page -> IO ()
  }

programs :: [Program]
programs =
  [ -- A counter written with haskell-halogen-hooks.
    Program "hooks" "a hooks counter from the hooks layer, which counts" Nothing $ \_ page -> do
      settle page
      hasText (locator page "#app button") "Hooked 0 times"
      click (locator page "#app button")
      hasText (locator page "#app button") "Hooked 1 times"
  , -- A hash router: where the page is, and a link that goes elsewhere.
    Program "router" "a router link from the router layer, followed in the page" Nothing $ \_ page -> do
      settle page
      appShows page "route: Home"
      click (withText "About" (locator page "#app a"))
      appShows page "route: About"
      (hash, path) :: (Text, Text) <- evaluate page "[location.hash, location.pathname]"
      unless (hash == "#/about" && path == "/") $ expectationFailure ("the link went to " <> show (hash, path))
  , -- A fetch of a file this server serves. The program's fiber waits in a
    -- liftIO for the response: quiet is when it has come back and been
    -- rendered.
    Program "net" "a fetch from the net layer, its text shown" Nothing $ \_ page -> do
      waitQuiet page
      appShows page ("fetched: " <> hello)
  , -- A custom element: plain HTML put into #app, its element defined from
    -- a component afterwards (so that it is upgraded where it is), and an
    -- attribute the page sets heard as the component's new input. The run
    -- ends with the HTML taken out again, for the next run's #app.
    Program
      "elements"
      "a custom element from the elements layer, defined over HTML in the page, its attribute set from JavaScript"
      Nothing
      $ \_ page -> do
        let element = locator page "#app x-playground-greeting"
        settle page
        -- Playwright's selectors go into open shadow roots.
        hasText (locator element "p") "an element greets Ada"
        _ :: Value <- evaluateOn element "(e) => { e.setAttribute('name', 'Grace'); return null; }" ()
        -- The element hears the change in a callback into the program,
        -- whose new input is the program's work: settled.
        settle page
        hasText (locator element "p") "an element greets Grace"
        inside :: Text <- evaluateOn element "(e) => e.innerHTML" ()
        unless (Text.null inside) $
          expectationFailure ("the component renders in the shadow root, not among the page's children: " <> show inside)
  , -- Halogen.DevTools' panel, opened over the page before a counter is
    -- mounted in #app: it lists the counter under its name, and counts its
    -- click. The run ends with the panel closed and its element gone.
    --
    -- The panel takes what the trace reported a tenth of a second after the
    -- first event of a batch, on a timer of its own (Halogen.DevTools'
    -- listener), so what it shows is not the program's yet when the
    -- program is settled: the row, and its count after the click, are
    -- waited for as conditions, for the page's timeout. The counter's
    -- own text is the program's, read once.
    Program "devtools" "the DevTools panel from the devtools layer, over the page, counting a click" Nothing $ \_ page -> do
      let row = locator page ".halogen-devtools [data-label=\"Counter\"]"
      settle page
      waitFor row Attached
      click (withText "Counted 0" (locator page "#app button"))
      hasText (locator page "#app button") "Counted 1"
      locator row ".halogen-devtools-actions" `shouldHaveText` "1"
  , -- A Material button, whose ripple is material.js's and whose look is
    -- material.css's.
    Program "material" "a Material button from the material layer, with its CSS and JavaScript, clicked" Nothing $ \_ page -> do
      let button = locator page "#app button.mdc-button"
      settle page
      count button `shouldReturn` 1
      -- MDCRipple, from material.js, marks the button it took.
      count (locator page "#app button.mdc-ripple-upgraded") `shouldReturn` 1
      background :: Text <- evaluateOn button "(b) => getComputedStyle(b).backgroundColor" ()
      unless (background == "rgb(98, 0, 238)") $ expectationFailure ("the button's background is " <> show background <> ": no material.css?")
      click button
      appShows page "material clicks: 1"
  , -- A rectangle on a Pixi canvas, tapped.
    Program "pixi" "a Pixi scene from the pixi layer, on a canvas, tapped" (Just "PixiJS") $ \_ page ->
      -- Below the scene's origin (the canvas's centre) and inside the
      -- rectangle wherever its corner is.
      tapUntilCounted page "pixi taps: " (\box -> (box.x + box.width / 2 + 40, box.y + box.height / 2 + 40)) "no tap reached the Pixi scene"
  , -- A Three.js scene, one box at the origin the camera looks at,
    -- clicked: the click is raycast into the scene and handed to the box's
    -- handler.
    Program "three" "a Three.js scene from the three layer, on a canvas, clicked" (Just "Three.js") $ \_ page ->
      -- The canvas's centre, where the camera looks at the box.
      tapUntilCounted page "three clicks: " (\box -> (box.x + box.width / 2, box.y + box.height / 2)) "no click reached the Three.js scene"
  , -- An hh quote, which runs haskell-halogen-th's quasi-quoter (and so
    -- GHC's own parser, the base archive's libHSghc, for the Haskell in it)
    -- in the page's interpreter.
    Program "th" "an hh quote from the th layer, with Haskell inside" Nothing $ \_ page -> do
      settle page
      hasText (locator page "#app p.quoted") "hello from hh, 42"
  , -- An hspec-halogen spec, run in the page: with no Playwright here, its
    -- clicks and keystrokes are synthetic events the harness dispatches
    -- itself, its tests' pages are in #app, and hspec's report is the
    -- program's stdout. The program's main returns once hspec has printed
    -- the report, and the run is over only then, so it is all there; and
    -- each test took down what it mounted, so no tree is left.
    Program "hspec" "an hspec-halogen spec from the hspec layer, run in the page, its report on stdout" Nothing $ \_ page -> do
      settle page
      stdout <- textContent (locator page "#stdout")
      let lines' = Text.lines stdout
      forM_ ["  clicks [✔]", "  types [✔]", "2 examples, 0 failures"] $ \line ->
        unless (line `elem` lines') $ expectationFailure ("hspec's report has no line " <> show line <> ":\n" <> Text.unpack stdout)
      inside :: Text <- evaluateOn (locator page "#app") "(e) => e.innerHTML" ()
      unless (Text.null inside) $ expectationFailure ("each test took down what it mounted, but #app has " <> show inside)
  , -- A store: a button that updates it, and a component connected to it.
    Program "store" "a store from the store layer, updated by a click and shown by a connected component" Nothing $ \_ page -> do
      settle page
      appShows page "stored 0"
      click (withText "bump" (locator page "#app button"))
      appShows page "stored 1"
  , -- Halogen.Worker in the interpreter, which has no binary for a worker
    -- to load: the page names its worker.js as halogenWorker.script before
    -- the run, and the pool's worker, started at the first job, starts a
    -- compiler of its own, compiles the same source and runs its main,
    -- which serves. The jobs, static pointers the worker finds by the
    -- page's keys, run there (whichWorker says Just 0), one mapped with
    -- start, three in runPar's tasks, and a job's exception comes back as a
    -- WorkerError. The first job waits for that worker to start: the page's
    -- own start-up, and this program's first compile, again.
    Program
      "worker"
      "static jobs from the worker layer, run in a worker that is a second compiler with the program compiled in it, one mapped with start and in tasks with runPar, a failure as a WorkerError"
      Nothing
      $ \walk page -> do
        -- The component's Initialize waits for each job in a liftIO, the
        -- first for the worker's compiler to start and compile the
        -- program: tens of seconds on a slow runner. Quiet is when the
        -- last has come back and been rendered. What it took, from the end
        -- of the run (the component mounted, its jobs given), and the
        -- renderer's memory with the worker up, are printed.
        started <- getMonotonicTime
        waitQuiet (long walk page)
        elapsed <- subtract started <$> getMonotonicTime
        said <- textContent (locator page "#app")
        unless (workerSaid said) $ expectationFailure ("the jobs' results, read once the program was quiet: " <> show said)
        rs <- filter (\p -> p.kind == "renderer") <$> browserProcesses page
        report $
          "the worker started and answered in "
            <> tshow (round (elapsed * 1000) :: Int)
            <> " ms; renderer RSS with it up "
            <> Text.intercalate ", " (map memory rs)
  , -- A component that arrives later: Halogen.Lazy's placeholder, while
    -- Halogen.Lazy.Dyld fetches the layer's demo library and has dyld link
    -- it into the program, then the component its entry hands over, which
    -- waves on a click and when its parent tells it to, and raises it each
    -- time.
    Program "lazy" "a component from the lazy layer's demo library, fetched and linked while the program runs" Nothing $ \_ page -> do
      -- The loader runs in the wrapper's fiber, in a liftIO: quiet is when
      -- the library is linked and its component rendered.
      waitQuiet page
      appShows page "Hello, playground!"
      click (withText "Wave" (locator page "#app button"))
      appShows page "Waved 1 time."
      appShows page "heard 1"
      click (withText "tell it" (locator page "#app button"))
      appShows page "Waved 2 times."
      appShows page "heard 2"
  , -- A component in RemoteDOM, through Halogen.Remote.Loopback: its DOM
    -- operations go as JSON to the layer's client in #app, which builds the
    -- button there, and the button's click comes back the same way to the
    -- component's handler, whose patch the client applies.
    Program "live" "a component sent over the live layer's wire to a client in the page, clicked through it" Nothing $ \_ page -> do
      settle page
      count (locator page "#app[data-halogen-remote=\"attached\"]") `shouldReturn` 1
      appShows page "remote clicks: 0"
      click (withText "remote clicks" (locator page "#app button"))
      appShows page "remote clicks: 1"
  ]

-- | Tap a canvas, at a point of its box, until the program counts a tap
-- (what #app shows after the prefix is not 0), for a minute. PixiJS and
-- Three.js arrive from the CDN and initialise asynchronously, in their
-- own JavaScript and not in a fiber of the program, so neither settling
-- nor quiet covers them; until they have, a tap goes nowhere. Each tap is
-- settled, so a tap that reached the scene is counted when it returns.
tapUntilCounted :: Page -> Text -> (Box -> (Double, Double)) -> String -> IO ()
tapUntilCounted page prefix at failure = do
  let canvas = locator page "#app canvas"
  waitFor canvas Attached
  eventuallyWithin 60000 $ do
    box <- boundingBox canvas
    forM_ box $ \b -> when (b.width > 0) $ uncurry (mouseClick page) (at b)
    said <- textContent (locator page "#app")
    case Text.breakOn prefix said of
      (_, rest) | Just n <- readMaybe (Text.unpack (Text.takeWhile isDigit (Text.drop (Text.length prefix) rest))), n >= (1 :: Int) -> pure ()
      _ -> expectationFailure (failure <> ": #app shows " <> show said)

-- | What the worker layer's program shows when every job came back as it
-- should: the failing job's message names where its job is (Main's line
-- and column).
workerSaid :: Text -> Bool
workerSaid said = case Text.breakOn prefix said of
  (_, rest)
    | Just afterMain <- Text.stripPrefix prefix rest
    , (line, r1) <- Text.span isDigit afterMain
    , Just r2 <- Text.stripPrefix ":" r1
    , (column, r3) <- Text.span isDigit r2 ->
        not (Text.null line) && not (Text.null column) && ": user error (no luck)" `Text.isPrefixOf` r3
  _ -> False
  where
    prefix = "available True; ran in Just 0; doubled 21 to 42; doubled 1, 2, 3 to [2,4,6]; in tasks 4, 5, 6 to [8,10,12]; Main:"

----------------------------------------------------------------------
-- What the page measured

mb :: (Real a) => a -> Text
mb bytes = Text.pack (printf "%.0f MB" (realToFrac bytes / 1e6 :: Double))

-- | A renderer's memory: resident now, and at its peak.
memory :: BrowserProcess -> Text
memory p = mb p.rss <> " (peak " <> mb p.peak <> ")"

-- | The page's own measurements (window.learn: each step's time, the JS
-- heap and the wasm memory after each), the JS heap as DevTools counts it,
-- and the renderers' memory, as tables to copy into learn/README.md.
printMeasurements :: Walk -> Page -> IO ()
printMeasurements walk page = do
  times <- readIORef walk.layerTimes
  unless (null times) $ do
    putStrLn "\n    Layers (ms; loading is fetching, extracting and adding assets; the first run loads its libraries):"
    forM_ times $ \(name, load, firstRun) ->
      printf "      %-10s load %6d  first run %6s\n" name load (maybe "-" show firstRun)
    printf "      %-10s load %6d\n" ("all" :: Text) (sum [l | (_, l, _) <- times])
  Measured {timings, memories} <- evaluate page "({timings: Object.entries(window.learn.timings), memory: Object.entries(window.learn.memory)})"
  putStrLn "\n    Timings (ms, as the page measured them):"
  forM_ timings $ \(name, ms) -> printf "      %-28s %7d\n" name ms
  putStrLn "    Memory:"
  heaps <- readIORef walk.heaps
  forM_ memories $ \(when', wasm) ->
    printf "      after %-22s JS heap %s, wasm memory %s\n" when' (maybe "?" mb (Map.lookup when' heaps)) (maybe "?" mb wasm)
  rs <- readIORef walk.renderers
  forM_ rs $ \(when', processes) ->
    printf "      renderer RSS %-16s %s\n" when' (Text.intercalate ", " (map memory processes))

-- | window.learn's timings and memory, in the page's order.
data Measured = Measured {timings :: [(Text, Int)], memories :: [(Text, Maybe Double)]}

instance FromJSON Measured where
  parseJSON = withObject "learn" $ \o -> do
    timings <- o .: "timings"
    entries :: [(Text, Value)] <- o .: "memory"
    memories <- traverse (\(k, v) -> (k,) <$> withObject "memory" (.:? "wasm") v) entries
    pure Measured {timings, memories}
