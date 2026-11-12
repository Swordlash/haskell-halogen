-- | The worker example (examples/worker), built for WebAssembly, in
-- headless Chromium: jobs run in Web Workers that are copies of the page's
-- own app.wasm, while the page goes on drawing.
--
-- > npm run test-worker      # after: sh toolchain/build-wasm.sh worker
--
-- It builds nothing. It serves the built example
-- (@dist-newstyle/wasm/public/worker@ from the repository's root, or the
-- directory @HALOGEN_EXAMPLE_DIR@ names) and checks: the pool says workers
-- are available; counting in a worker gives the right count while the
-- component's spinner, which it turns a frame at a time, goes on turning
-- (the frames the page showed meanwhile are counted from the page itself, a
-- sample at each of its own animation frames), in each of the pool's two
-- workers as it starts and in one already running; counting on the page
-- gives the same count, and the component draws no frame until it is done;
-- the range counted in four parts with Worker.start and mapConcurrently
-- comes back in order, from both of the pool's workers, while the page
-- draws, and on the page where no worker can be made; the range counted in
-- sixteen tasks with Halogen.Worker.Par's runPar comes back in order, each
-- part's count right, from both workers; a slow count that checkpoints
-- stops soon after Cancel (its log of chunks, which the workers' console
-- relays to the page's, stops), a count that does not is stopped at once by
-- Kill, which ends its worker, and the pool's places still run jobs
-- afterwards; a race between a quick count and the slow one cancels the
-- slow one; a job that throws is reported with its message; terminating the
-- pool ends its workers, and the next job starts one again; a page whose
-- loader names a worker script of its own (@?workers=script@) counts in a
-- worker and in sixteen tasks over both through it; a page whose @Worker@
-- refuses to start fails the job with that; nothing is logged as an error,
-- and nothing is loaded from another site. @WORKER_DEBUG=1@ prints what the
-- page and its workers log, as it arrives (it sets
-- @HALOGEN_PLAYWRIGHT_CONSOLE@, which the driver reads).
--
-- How it waits. A job's result is the component's: its fiber waits in a
-- @liftIO@ until the job's promise settles, and then renders it. So after
-- the click that starts a job the test waits for the program to be quiet
-- ('waitQuiet': idle, and no fiber waiting), and reads the result once,
-- with no retry. What is not the program's is waited for as a condition
-- with a deadline, each saying why: the page's own animation frames, the
-- chunks a worker logs, a worker ending.
--
-- The examples of a group share one page ('onOnePage'), in order: each
-- goes on from the state the one before left (the pool's workers, a killed
-- worker, a terminated pool), so one of them cannot be run alone with
-- @--match@.
-- Without the build the examples are pending, with the command that makes
-- it, unless @HALOGEN_EXAMPLE_REQUIRED=1@ (as @npm run test-worker@ sets
-- it), when they fail.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless, when)
import Data.Aeson (Value)
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, (!))
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTime)
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Halogen.Playwright.Serve (serveDirectory)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, setEnv)
import System.FilePath ((</>))
import Text.Read (readMaybe)

main :: IO ()
main = do
  dir <- fromMaybe "../../dist-newstyle/wasm/public/worker" <$> lookupEnv "HALOGEN_EXAMPLE_DIR"
  required <- (== Just "1") <$> lookupEnv "HALOGEN_EXAMPLE_REQUIRED"
  built <- doesFileExist (dir </> "app.wasm")
  debug <- maybe False (not . null) <$> lookupEnv "WORKER_DEBUG"
  when debug $ setEnv "HALOGEN_PLAYWRIGHT_CONSOLE" "1"
  if built
    then serveDirectory dir (hspec . spec)
    else
      hspec
        $ describe "the worker example, built for WebAssembly"
        $ it "is served"
        $ if required
          then expectationFailure (dir </> "app.wasm" <> " is missing: build it with sh toolchain/build-wasm.sh worker")
          else pendingWith ("no " <> dir <> "/app.wasm: build it with sh toolchain/build-wasm.sh worker, or run npm run test-worker")

----------------------------------------------------------------------
-- What the example counts

-- | The primes below two million, which the example counts.
expected :: Int
expected = 148933

-- | Where the example counts up to.
size :: Int
size = 2000000

-- | Each quarter of that range, which the example counts in parts: its
-- start, its end and its count.
quarters :: [(Int, Int, Int)]
quarters =
  [ (0, 500000, 41538)
  , (500000, 1000000, 36960)
  , (1000000, 1500000, 35657)
  , (1500000, 2000000, 34778)
  ]

-- | Which numbers below 'size' are not prime, by a sieve: the sixteen
-- tasks' counts are checked against it rather than against the example's
-- own trial division.
composite :: UArray Int Bool
composite = runSTUArray $ do
  marks <- newArray (0, size - 1) False
  forM_ (takeWhile (\i -> i * i < size) [2 ..]) $ \i -> do
    marked <- readArray marks i
    unless marked $ forM_ [i * i, i * i + i .. size - 1] $ \j -> writeArray marks j True
  pure marks

-- | The primes from @from@ up to @to@, @to@ left out.
primesIn :: Int -> Int -> Int
primesIn from to = length [k | k <- [max 2 from .. to - 1], not (composite ! k)]

----------------------------------------------------------------------
-- A page shared by a group's examples

-- | A group of examples on one page, in a context of its own (so that what
-- the context asked for is the group's), with an init script of the test's
-- own if one is given.
group :: String -> Maybe Text -> SpecWith Page -> SpecWith Playwright
group name initScript =
  describe name . onOnePage defaultContextOptions (\page -> mapM_ (addInitScript (pageContext page)) initScript)

-- | A line of the report, with what was measured.
report :: Text -> IO ()
report line = putStrLn ("    ok  " <> Text.unpack line)

ms :: Double -> Text
ms seconds = tshow (round (seconds * 1000) :: Int) <> " ms"

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

-- | Nothing logged as an error, nothing thrown, so far: the workers'
-- console is the page's too.
noErrors :: Page -> Text -> IO ()
noErrors page when' = do
  logged <- filter (\m -> m.kind == "error") <$> consoleMessages page
  thrown <- pageErrors page
  unless (null logged && null thrown) $
    expectationFailure ("errors in the page " <> Text.unpack when' <> ": " <> show (map (.text) logged <> thrown))

----------------------------------------------------------------------
-- Starting

-- | Open the page and wait for the program: loading app.wasm (two
-- megabytes, compiled as it streams) may take far longer than an action,
-- so these calls have 30 s. 'visit' waits for the program to have mounted
-- and run its Initialize's synchronous part; 'waitQuiet' then for the rest
-- of it, which asks Worker.available in a liftIO: until that has answered
-- the page says no Web Worker can be made, whatever the answer will be.
start :: Text -> Page -> Text -> IO ()
start base page query = do
  let loading = withTimeout 30000 page
  visit loading (base <> query)
  waitQuiet loading

-- | What the page says about Web Workers, read once the program is quiet.
available :: Page -> Text -> IO ()
available page prefix = do
  said <- textContent (locator page "#available")
  unless (prefix `Text.isPrefixOf` said) $
    expectationFailure ("#available says " <> show said <> ", not " <> show prefix)

----------------------------------------------------------------------
-- A job

-- | What a job's run showed: the result, how many different frame counts
-- the page showed while the component said it was counting and in how
-- many of the page's own frames, and how long from the click to the
-- program being quiet.
data Run = Run
  { text :: Text
  , shown :: Int
  , samples :: Int
  , elapsed :: Double
  }

-- | Click a button, then sample the frame count the page shows at each of
-- the page's own frames while the component says it is counting: how many
-- different counts the reader saw while the job ran, in how many frames of
-- the page's (none, when the page's thread was busy throughout).
--
-- A job, whether in a worker or on the page, is waited for in the
-- component's fiber, in a liftIO (Worker.start's promise, waitCatch), and
-- the component renders the result when it comes back. So 'waitQuiet'
-- returns once the result is rendered, and it is read once, with no
-- retry: a wait that returned early would read the empty result the
-- component shows while the job runs, and fail. A job takes seconds (on
-- the page it holds the page's thread for them, the settle after the
-- click included), so the click and the wait have a minute.
runJob :: Page -> Text -> Text -> IO Run
runJob page button place = do
  let job = withTimeout 60000 page
  _ :: Maybe Value <-
    evaluate
      page
      "(() => {\
      \  window.seen = new Set();\
      \  window.samples = 0;\
      \  const sample = () => {\
      \    if (document.getElementById('status').textContent.startsWith('Counting')) {\
      \      window.seen.add(document.getElementById('frames').textContent);\
      \      window.samples += 1;\
      \    }\
      \    window.sampling = requestAnimationFrame(sample);\
      \  };\
      \  sample();\
      \  return null;\
      \})()"
  started <- getMonotonicTime
  click (locator job ("#" <> button))
  waitQuiet job
  elapsed <- subtract started <$> getMonotonicTime
  (shown, samples) <- evaluate page "(cancelAnimationFrame(window.sampling), [window.seen.size, window.samples])"
  text <- textContent (locator page "#result")
  unless ((place <> ":") `Text.isPrefixOf` text) $
    expectationFailure ("the result, read once the program was quiet, is not " <> show place <> "'s: " <> show text)
  pure Run {text, shown, samples, elapsed}

-- | A count's result: the count, the milliseconds the component measured
-- and the frames it drew meanwhile.
data Counted = Counted {count :: Int, millis :: Int, drew :: Int}

counted :: Text -> Text -> IO Counted
counted place text =
  case Text.words <$> Text.stripPrefix (place <> ": ") text of
    Just [n, "primes", "below", _, "in", m, "ms,", "while", "the", "page", "drew", f, "frames"]
      | Just c <- readText n, Just t <- readText m, Just d <- readText f -> pure Counted {count = c, millis = t, drew = d}
    _ -> expectationFailure ("not a count: " <> show text) >> pure Counted {count = 0, millis = 0, drew = 0}
  where
    readText = readMaybe . Text.unpack

-- | The lines of the parts a count in parts or in tasks showed: each one's
-- text and the worker that counted it (@page@ for the page).
partsShown :: Page -> IO [(Text, Text)]
partsShown page = evaluateAll (locator page "#parts li") "(items) => items.map((item) => [item.textContent, item.dataset.worker])" ()

-- | A count in a worker: the right count, a worker running, and the page
-- drawing meanwhile.
inWorker :: Page -> Text -> IO ()
inWorker page what = do
  run <- runJob page "worker" "in a worker"
  c <- counted "in a worker" run.text
  c.count `shouldBe` expected
  alive <- workers page
  alive `shouldSatisfy` (>= 1)
  -- At 60 frames a second a job of a second or more shows tens of them;
  -- ten is a floor a loaded machine still clears.
  unless (run.shown >= 10) $
    expectationFailure ("the spinner turned while the worker counted: " <> show run.shown <> " different counts shown in " <> Text.unpack (ms run.elapsed))
  unless (c.drew >= 10) $ expectationFailure ("the component drew while the worker counted: " <> show c.drew <> " frames")
  report $
    "in a worker, "
      <> what
      <> ": "
      <> tshow c.count
      <> " primes in "
      <> tshow c.millis
      <> " ms, the component drew "
      <> tshow c.drew
      <> " frames, the page showed "
      <> tshow run.shown
      <> " different counts in "
      <> tshow run.samples
      <> " of its frames"
  noErrors page ("counting in a worker, " <> what)

-- | The range in four parts, each a Worker.start, waited for together
-- with mapConcurrently: the parts come back in the range's order,
-- whichever finished first, and were counted where expected (the pool's
-- two workers took turns with them, or the page).
inParts :: Page -> Text -> [Text] -> IO (Counted, Run, [(Text, Text)])
inParts page place used = do
  run <- runJob page "spread" place
  c <- counted place run.text
  c.count `shouldBe` expected
  parts <- partsShown page
  unless (length parts == length quarters) $ expectationFailure ("a line for each part: " <> show parts)
  forM_ (zip3 [0 :: Int ..] parts quarters) $ \(index, (text, _), (from, to, n)) ->
    unless ((tshow n <> " primes from " <> tshow from <> " up to " <> tshow to <> ", counted ") `Text.isPrefixOf` text) $
      expectationFailure ("part " <> show index <> " in its place, with its count: " <> show text)
  unless (sort (nub (map snd parts)) == used) $
    expectationFailure ("the parts were counted " <> show used <> ": " <> show parts)
  pure (c, run, parts)

-- | The range in sixteen tasks, which runPar sends in batches to whichever
-- worker is free: each part's count, against a sieve's, in the range's
-- order, and both workers used.
inTasks :: Page -> Text -> IO ()
inTasks page what = do
  let sixteenth = size `div` 16
  run <- runJob page "tasks" "in tasks"
  c <- counted "in tasks" run.text
  c.count `shouldBe` expected
  parts <- partsShown page
  unless (length parts == 16) $ expectationFailure ("a line for each task: " <> show parts)
  forM_ (zip [0 ..] parts) $ \(index, (text, _)) -> do
    let from = index * sixteenth
        to = from + sixteenth
    unless ((tshow (primesIn from to) <> " primes from " <> tshow from <> " up to " <> tshow to <> ", counted ") `Text.isPrefixOf` text) $
      expectationFailure ("task " <> show index <> " in its place, with its count: " <> show text)
  unless (sort (nub (map snd parts)) == ["0", "1"]) $
    expectationFailure ("the tasks were counted in both workers: " <> show parts)
  report $
    "runPar"
      <> what
      <> ": "
      <> tshow c.count
      <> " primes in 16 tasks in "
      <> tshow c.millis
      <> " ms, in order, from workers "
      <> Text.concat (map snd parts)
      <> ", the component drew "
      <> tshow c.drew
      <> " frames"
  noErrors page ("counting in tasks" <> what)

-- | How many chunks the slow count has logged on this page so far: it logs
-- each chunk it counts, and checkpoints after each.
progress :: Page -> IO Int
progress page = length . filter (Text.isInfixOf "the slow count has counted up to" . (.text)) <$> consoleMessages page

-- | Requests to another site than the test server's, which serves
-- everything the example needs: the WASI shim is bundled beside the page.
offSite :: Text -> Page -> IO [Text]
offSite base page = do
  asked <- map (.url) <$> requests (pageContext page)
  pure [u | u <- asked, any (`Text.isPrefixOf` u) ["http:", "https:"], not (Text.isPrefixOf (origin base) u)]
  where
    origin = Text.intercalate "/" . take 3 . Text.splitOn "/"

-- | The checks every group ends with.
clean :: Text -> Page -> IO ()
clean base page = do
  noErrors page "at the end"
  offSite base page `shouldReturn` []
  report "no errors in the page, and nothing loaded from another site"

----------------------------------------------------------------------

spec :: Text -> Spec
spec base = playwright $ describe "the worker example, built for WebAssembly" $ do
  group "with the pool's two workers" Nothing $ do
    it "starts: workers available, the spinner turning, none started" $ \page -> do
      start base page ""
      available page "Web Workers are available"
      -- The component counts the page's animation frames: they are the
      -- page's own, which the program follows rather than makes, so this
      -- waits for the count itself, for ten seconds.
      firstFrame <- textContent (locator page "#frames")
      waitForFunction (withTimeout 10000 page) ("Number(document.getElementById('frames').textContent) > " <> firstFrame <> " + 5")
      workers page `shouldReturn` 0
      report "the page is up, the spinner turning, workers available and none started"

    -- The pool has two workers, which take turns: the first two jobs start
    -- one each, and the third finds the first already running.
    it "counts in a worker, the first, starting it" $ \page -> inWorker page "the first, starting it"
    it "counts in a worker, the second, starting it" $ \page -> inWorker page "the second, starting it"
    it "counts in a worker, the first again, already running" $ \page -> inWorker page "the first again, already running"

    it "counts in four parts with Worker.start, in both workers, in order" $ \page -> do
      (c, run, parts) <- inParts page "in both workers" ["0", "1"]
      unless (c.drew >= 10) $ expectationFailure ("the component drew while the workers counted: " <> show c.drew <> " frames")
      report $
        "in both workers, a part each in turn: "
          <> tshow c.count
          <> " primes in "
          <> tshow c.millis
          <> " ms, the parts in order from workers "
          <> Text.intercalate ", " (map snd parts)
          <> ", the component drew "
          <> tshow c.drew
          <> " frames, the page showed "
          <> tshow run.shown
          <> " different counts"
      noErrors page "counting in parts"

    it "counts in sixteen tasks with runPar, in both workers, in order" $ \page -> inTasks page ""

    it "stops a slow count at its next checkpoint on Cancel" $ \page -> do
      started <- progress page
      click (locator page "#slow")
      -- The chunks are logged by the worker's console, relayed to the
      -- page's: not the program's, so waited for as they come, for thirty
      -- seconds.
      eventuallyWithin 30000 $ do
        logged <- progress page
        unless (logged >= started + 3) $ expectationFailure ("the slow count has logged " <> show (logged - started) <> " chunks, not three")
      clicked <- getMonotonicTime
      click (locator page "#cancel")
      -- Cancel rejects the job's promise at once, and the component's fiber
      -- waiting for it comes back with the failure and renders it.
      waitQuiet page
      rejectedIn <- subtract clicked <$> getMonotonicTime
      result <- textContent (locator page "#result")
      unless ("the slow count: failed: " `Text.isPrefixOf` result && "the job was cancelled" `Text.isInfixOf` result) $
        expectationFailure ("the slow count, cancelled: " <> show result)
      -- The chunk under way when the cancel came is finished, then the
      -- checkpoint after it throws: half a second is several chunks' time.
      -- These two pauses let time pass to show that nothing more happens.
      threadDelay 500_000
      atCancel <- progress page
      threadDelay 2_000_000
      progress page `shouldReturn` atCancel
      report $
        "cancel: the promise was rejected "
          <> ms rejectedIn
          <> " after the click, and the slow count stopped at its next checkpoint, after "
          <> tshow (atCancel - started)
          <> " chunks"
      noErrors page "cancelling the slow count"

    it "ends a count's worker on Kill, and both of the pool's places run jobs afterwards" $ \page -> do
      before <- workers page
      click (locator page "#worker")
      -- Rendered as the click's handler ran, before its first wait.
      status <- textContent (locator page "#status")
      unless ("Counting in a worker" `Text.isPrefixOf` status) $ expectationFailure ("the status: " <> show status)
      -- Into the count, so that Kill stops a job that is running. (Kill's
      -- button is enabled once the job has started, which the click waits
      -- for.)
      threadDelay 300_000
      clicked <- getMonotonicTime
      click (locator page "#kill")
      -- Kill rejects the promise at once too.
      waitQuiet page
      rejectedIn <- subtract clicked <$> getMonotonicTime
      result <- textContent (locator page "#result")
      unless ("in a worker: failed: " `Text.isPrefixOf` result && "killed" `Text.isInfixOf` result) $
        expectationFailure ("the count, killed: " <> show result)
      -- A worker's ending is the browser's, not the program's: waited for
      -- as it happens, for five seconds.
      eventuallyWithin 5000 $ do
        alive <- workers page
        unless (alive < before) $ expectationFailure ("the killed job's worker has not ended: " <> show alive <> " workers, " <> show before <> " before")
      after <- workers page
      report $
        "kill: the promise was rejected "
          <> ms rejectedIn
          <> " after the click, and the worker that counted ended ("
          <> tshow before
          <> " workers before, "
          <> tshow after
          <> " after)"
      noErrors page "killing a count"
      (c, _, parts) <- inParts page "in both workers" ["0", "1"]
      report $
        "after kill, both of the pool's places run jobs: "
          <> tshow c.count
          <> " primes, the parts from workers "
          <> Text.intercalate ", " (map snd parts)

    it "cancels the slow count when it loses a race" $ \page -> do
      started <- progress page
      run <- runJob page "race" "the race"
      c <- counted "the race" run.text
      c.count `shouldBe` 78498
      atEnd <- progress page
      unless (atEnd > started) $ expectationFailure "the slow count did not run during the race"
      -- Time passing, to show the slow count logs no more: at most the
      -- chunk under way when the race ended.
      threadDelay 2_000_000
      afterwards <- progress page
      unless (afterwards <= atEnd + 1) $
        expectationFailure ("the slow count stopped when it lost: " <> show (afterwards - atEnd) <> " chunks after the race")
      report $
        "race: the quick count won in "
          <> tshow c.millis
          <> " ms, and the slow one, after "
          <> tshow (atEnd - started)
          <> " chunks, was cancelled and stopped"
      noErrors page "in the race"

    it "counts on the page, drawing nothing meanwhile" $ \page -> do
      run <- runJob page "here" "here"
      c <- counted "here" run.text
      c.count `shouldBe` expected
      c.drew `shouldBe` 0
      unless (run.shown <= 1) $ expectationFailure ("the spinner stopped while the page counted: " <> show run.shown <> " different counts shown")
      report $
        "on the page: "
          <> tshow c.count
          <> " primes in "
          <> tshow c.millis
          <> " ms, the component drew "
          <> tshow c.drew
          <> " frames, the page showed "
          <> tshow run.shown
          <> " different counts in "
          <> tshow run.samples
          <> " of its frames"
      noErrors page "counting on the page"

    it "reports a job that throws, with its message" $ \page -> do
      run <- runJob page "fail" "the failing job"
      unless ("the failing job: failed: " `Text.isPrefixOf` run.text && "this job always fails" `Text.isInfixOf` run.text) $
        expectationFailure ("the job's exception, with its message: " <> show run.text)
      report ("a job that throws is reported: " <> run.text)
      noErrors page "in the failing job"

    it "terminates the pool's workers, and starts one again for the next job" $ \page -> do
      before <- workers page
      click (locator page "#terminate")
      waitQuiet page
      status <- textContent (locator page "#status")
      unless ("terminated" `Text.isInfixOf` status) $ expectationFailure ("the status: " <> show status)
      -- The workers' ending is the browser's: waited for, for ten seconds.
      eventuallyWithin 10000 $ do
        alive <- workers page
        unless (alive == 0) $ expectationFailure ("terminate ends the workers: " <> show alive <> " left of " <> show before)
      report ("terminate ended the pool's " <> tshow before <> " workers")
      inWorker page "started again after terminate"

    it "said nothing wrong and loaded nothing from another site" $ \page -> do
      clean base page
      -- The workers' requests are among those checked: each worker the
      -- pool started loaded app.wasm itself, the page's own load besides.
      loads <- filter (Text.isSuffixOf "/app.wasm" . (.url)) <$> requests (pageContext page)
      unless (length loads >= 3) $ expectationFailure ("app.wasm loaded " <> show (length loads) <> " times: the workers' requests are missing")

  -- The page with ?workers=none, whose loader leaves halogenWorker unset, as
  -- wherever no worker can be made: Worker.start counts on the page, on a
  -- thread it forks, which leaves the component's loop free, and the page's
  -- thread is shared between them.
  group "where no worker can be made (?workers=none)" Nothing $ do
    it "starts, saying so" $ \page -> do
      start base page "?workers=none"
      available page "No Web Worker can be made here"

    it "counts with Worker.start, on the page" $ \page -> do
      run <- runJob page "worker" "with Worker.start"
      c <- counted "with Worker.start" run.text
      c.count `shouldBe` expected
      workers page `shouldReturn` 0
      report $
        "with Worker.start and no worker: "
          <> tshow c.count
          <> " primes in "
          <> tshow c.millis
          <> " ms, the component drew "
          <> tshow c.drew
          <> " frames, the page showed "
          <> tshow run.shown
          <> " different counts in "
          <> tshow run.samples
          <> " of its frames"
      noErrors page "in the fallback"

    it "counts in four parts with Worker.start, all on the page, in order" $ \page -> do
      (c, _, parts) <- inParts page "in parts with Worker.start" ["page"]
      workers page `shouldReturn` 0
      report $
        "in parts with no worker: "
          <> tshow c.count
          <> " primes in "
          <> tshow c.millis
          <> " ms, all "
          <> tshow (length parts)
          <> " parts counted on the page, in order"
      noErrors page "counting in parts in the fallback"

    it "said nothing wrong and loaded nothing from another site" (clean base)

  -- The page with ?workers=script, whose loader names a worker script of its
  -- own (web/worker-script.js) rather than the program's files, as a program
  -- that is not a binary does: each worker imports it, and it loads app.wasm
  -- and runs main itself. The count in a worker while the page draws, and
  -- the sixteen tasks over both workers.
  group "with a worker script of the page's own (?workers=script)" Nothing $ do
    it "starts: workers available, none started" $ \page -> do
      start base page "?workers=script"
      available page "Web Workers are available"
      workers page `shouldReturn` 0

    it "counts in a worker" $ \page -> inWorker page "from the page's own script, starting it"

    it "counts in sixteen tasks with runPar, in both workers, in order" $ \page -> inTasks page ", from the page's own script"

    it "said nothing wrong and loaded nothing from another site" (clean base)

  -- A page whose Worker constructor throws, as a browser refusing a Blob
  -- worker does: available still says yes, and the failure to start must
  -- reach the program as the job's WorkerError, not as an exception out of
  -- the binding.
  group "where the browser refuses to start a worker" (Just refusing) $ do
    it "starts: workers available" $ \page -> do
      start base page ""
      available page "Web Workers are available"

    it "fails the job, with why" $ \page -> do
      run <- runJob page "worker" "in a worker"
      case Text.breakOn "failed: the worker did not start: " run.text of
        (_, rest) | not (Text.null rest), "workers are refused here" `Text.isInfixOf` rest -> pure ()
        _ -> expectationFailure ("the start's failure, as the job's: " <> show run.text)
      workers page `shouldReturn` 0
      report ("a worker that cannot be made fails the job: " <> run.text)
      noErrors page "with a worker that cannot start"

    it "said nothing wrong and loaded nothing from another site" (clean base)
  where
    refusing =
      "globalThis.Worker = class {\
      \  constructor() {\
      \    throw new DOMException('workers are refused here', 'SecurityError');\
      \  }\
      \};"
