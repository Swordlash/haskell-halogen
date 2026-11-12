-- | Every lesson: its start compiles (warnings allowed, since an exercise
-- may leave something unused), and its solution compiles with no
-- diagnostic at all and shows something: a component in the result, or
-- what the program printed, for a lesson whose program runs a spec. Some
-- lessons' pages are checked to do what the lesson says, for each. This is
-- how a new lesson is checked: a failing one is named, with what GHC said.
module Tutorial.Lessons (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Exception (SomeException, displayException, fromException, try)
import Control.Monad (forM_, unless, when)
import Data.Aeson (Value)
import Data.Char (isDigit)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import GHC.Clock (getMonotonicTime)
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Halogen.Playwright.Internal (Page (..))
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Tutorial.Site
import Tutorial.Walk

-- | A compile of a lesson's code, as the page showed it.
data Run = Run
  { took :: Double
  , checked :: Maybe Text
  -- ^ Why the lesson's page did not do what the lesson says, if it did not.
  , outcome :: Text
  , errors :: [Text]
  , diagnostics :: [Text]
  , result :: Text
  , stdout :: Text
  }

shown :: Run -> Bool
shown r = not (Text.null r.result) || not (Text.null r.stdout)

-- | Load a lesson's code by its button (Reset, Solution), compile it, read
-- what the page shows, and run the lesson's own check of its page.
runLesson :: Site -> Page -> Text -> Maybe (Page -> IO ()) -> IO Run
runLesson site page button pageCheck = do
  click (locator page button)
  ((), took) <- timed (compiled site page)
  let result = locator page "#learn-result"
  inside <- evaluateOn result "(e) => e.innerHTML" ()
  stdout <- Text.concat <$> allTextContents (locator page ".learn-stdout")
  checked <- case pageCheck of
    Nothing -> pure Nothing
    Just checkPage -> either (Just . firstLine) (const Nothing) <$> try @SomeException (checkPage page)
  said <- outcomeOf page
  errors <- allTextContents (locator page ".learn-diagnostics li.learn-error")
  diagnostics <- allTextContents (locator page ".learn-diagnostics li")
  pure Run {took, checked, outcome = said, errors, diagnostics, result = inside, stdout}
  where
    firstLine e = Text.takeWhile (/= '\n') $ case fromException e of
      Just (HUnitFailure _ (Reason r)) -> Text.pack r
      Just (HUnitFailure _ (ExpectedButGot p expected actual)) -> Text.pack (fromMaybe "" p <> " expected " <> expected <> ", got " <> actual)
      Nothing -> Text.pack (displayException e)
    outcomeOf p = textContent (locator p ".learn-outcome")

-- | What a lesson whose program runs an hspec-halogen spec in the page,
-- rather than mount the component, must report: hspec's summary and lines
-- for Start.hs and for Solution.hs. Each test takes down what it mounted,
-- so the result is empty once the run is over, and the second compile runs
-- on what the first left, nothing.
specLessons :: [(Text, ([Text], [Text]))]
specLessons =
  [
    ( "13-testing"
    ,
      ( ["2 examples, 1 failure", "  greets by name [✔]", "  counts clicks [✘]", "No element matches \"button.increment\""]
      , ["3 examples, 0 failures", "  counts clicks [✔]", "  greets a new name once the old one is cleared [✔]"]
      )
    )
  ]

-- | What is wrong with a spec lesson's run: a line its report lacks (the
-- summary and the examples as lines of their own, the failure's message
-- anywhere), or something left mounted.
reportProblems :: Run -> [Text] -> Text -> Text
reportProblems run wanted file =
  let lines' = Text.lines run.stdout
      present w = if "No element" `Text.isPrefixOf` w then w `Text.isInfixOf` run.stdout else w `elem` lines'
      missing = filter (not . present) wanted
   in Text.concat
        [ if null missing then "" else "; " <> file <> "'s report lacks " <> Text.intercalate ", " missing <> ":\n" <> run.stdout
        , if Text.null run.result then "" else "; " <> file <> " left something mounted: " <> Text.take 200 run.result
        ]

-- | The element under the result whose text has this in it, read once.
sees :: Locator -> Text -> Text -> IO ()
sees result css text = do
  n <- count (withText text (locator result css))
  unless (n >= 1) $ do
    whole <- textContent result
    expectationFailure (Text.unpack ("no " <> css <> " says " <> tshow text <> "; the result says " <> tshow whole))

-- | The lessons whose program puts a page of its own into the result
-- rather than mount the component there, or whose component does more than
-- show something: what the page does after each compile, Start.hs's and
-- Solution.hs's, given the result's element (on whichever page the lesson
-- compiled).
pageLessons :: Site -> [(Text, (Page -> IO (), Page -> IO ()))]
pageLessons site =
  [ -- Lesson 21's component is mounted in RemoteDOM through the loopback:
    -- the client, not the app, fills its element (data-halogen-remote says
    -- it is attached), and the wire under it shows each message as it
    -- crosses. In Start.hs the input has no handler, so what is typed
    -- reaches nothing and Add adds nothing; in Solution.hs each key sends an
    -- input event carrying the value, Add adds the item, and Clear sends a
    -- remove for each one. The client applies what crosses the wire in its
    -- own JavaScript, as it arrives, so what it shows and what the wire
    -- shows are waited for as conditions.

    ( "21-live"
    ,
      ( onResult $ \page result -> do
          let remote = locator result ".lesson-remote[data-halogen-remote=\"attached\"]"
              crossed text = waitFor (withText text (locator result ".lesson-wire")) Attached
          waitFor (withText "bread" (locator remote "li")) Attached
          crossed "[\"create\",2,null,\"div\"]"
          fill (locator remote "input") "eggs"
          click (withText "Add" (locator remote "button"))
          crossed "\"type\":\"click\""
          -- Nothing should arrive: a wrong item would come through the
          -- client's own JavaScript, so it is given half a second to.
          waitQuiet page
          threadDelay 500000
          items <- allTextContents (locator remote "li")
          unless (items == ["milk", "bread"]) $ expectationFailure ("Start.hs's Add finds the draft empty, but the list is " <> show items)
      , onResult $ \_ result -> do
          let remote = locator result ".lesson-remote[data-halogen-remote=\"attached\"]"
              crossed text = waitFor (withText text (locator result ".lesson-wire")) Attached
          waitFor (withText "bread" (locator remote "li")) Attached
          fill (locator remote "input") "eggs"
          crossed "\"value\":\"eggs\""
          click (withText "Add" (locator remote "button"))
          waitFor (withText "eggs" (locator remote "li")) Attached
          click (withText "Clear" (locator remote "button"))
          crossed "[\"remove\","
          locator remote "li" `shouldHaveCount` 0
      )
    )
  , -- Lesson 20's component counts and renames; Start.hs's holes are only
    -- reached by a click, so it is not clicked.

    ( "20-holes"
    ,
      ( onResult $ \_ result -> sees result "p" "Ada has counted to 0."
      , onResult $ \_ result -> do
          click (withText "+1" (locator result "button"))
          sees result "p" "Ada has counted to 1."
          fill (locator result "input") "Grace"
          sees result "p" "Grace has counted to 1."
      )
    )
  , ("19-three", (onResult sphereColour, onResult sphereColour))
  , -- Lesson 17's counts primes when its button is pressed. The page names
    -- its lesson-worker.js as halogenWorker.script before each compile, so
    -- the component says jobs run in a Web Worker: Start.hs counts in the
    -- component's loop, which draws no frame meanwhile, and starts no
    -- worker; Solution.hs counts with Worker.run, and the pool's worker, a
    -- second compiler that compiles the same lesson and serves its jobs,
    -- answers. A worker without the page's static keys, or one that did not
    -- start, would fail the job with a WorkerError rather than give the
    -- count, so the count is the proof that the worker found the job by its
    -- key. The component waits for the job in a liftIO, so quiet is when
    -- the count has come back and been rendered: the first job also waits
    -- for the worker to start (the page's start-up without the download,
    -- and the lesson's compile), which on a slow runner, beside the other
    -- page's compiles, is long: hence the long timeout.

    ( "17-worker"
    ,
      ( onResult $ \page result -> do
          (text, _, drawn) <- countPrimes site page result
          unless ("Counted 5133 primes" `Text.isPrefixOf` text) $ expectationFailure ("Start.hs counts in the handler: " <> show text)
          unless (drawn == Just 0) $ expectationFailure ("no frame drawn while the loop counts: " <> show text)
          started <- workers page
          unless (started == 0) $ expectationFailure "Start.hs gives no job, and so starts no worker"
      , onResult $ \page result -> do
          (text, took, drawn) <- countPrimes site page result
          unless ("The job primes counted 5133 primes" `Text.isPrefixOf` text) $ expectationFailure ("Solution.hs counts with the job, in the worker: " <> show text)
          started <- workers page
          unless (started == 1) $ expectationFailure ("the pool of one started its worker: " <> show started <> " workers")
          -- While the worker starts and counts, the page is free: the loop
          -- draws a frame each animation frame, some sixty a second.
          unless (maybe False (>= 30) drawn) $ expectationFailure ("the page drew while the worker started and counted: " <> show text)
          -- A second job finds the worker started, and waits for the count
          -- alone.
          (text', took', drawn') <- countPrimes site page result
          unless ("The job primes counted 5133 primes" `Text.isPrefixOf` text') $ expectationFailure ("the second job, in the worker already started: " <> show text')
          note
            ( "lesson 17: the first job, which started the worker, answered in "
                <> seconds took
                <> " ("
                <> maybe "?" tshow drawn
                <> " frames drawn); the second in "
                <> seconds took'
                <> " ("
                <> maybe "?" tshow drawn'
                <> " frames drawn)"
            )
      )
    )
  , -- Lesson 18's feature comes from the lazy layer's demo library, which
    -- the page fetches and dyld links into the program when it is first
    -- shown: it greets the name, waves when clicked and, in Solution.hs,
    -- when the parent tells it to, the parent hearing it each time and
    -- asking it how often; hidden and shown again it starts over. The
    -- loader waits in the wrapper's fiber, in a liftIO, so after a press
    -- that may load it the program is waited for until quiet.

    ( "18-lazy"
    ,
      ( onResult $ \page result -> do
          let says = sees result "p"
              press' text = click (withTextExact text (locator result "button")) >> waitQuiet page
          says "Hello, Ada! I was loaded from a library of my own."
          press' "Wave"
          says "Waved 1 time."
          press' "Hide"
          says "The feature is hidden."
          press' "Show"
          says "Waved 0 times."
      , onResult $ \page result -> do
          let says = sees result "p"
              press' text = click (withTextExact text (locator result "button")) >> waitQuiet page
          says "Hello, Ada! I was loaded from a library of my own."
          press' "Wave"
          says "The feature has waved 1 time."
          press' "Wave at it"
          says "Waved 2 times."
          says "The feature has waved 2 times."
          press' "Ask it"
          says "Asked, the feature says it has waved 2 times."
          press' "Hide"
          says "The feature is hidden."
          press' "Show"
          says "Waved 0 times."
      )
    )
  , -- Lesson 16's is the component with Halogen.DevTools' panel under it:
    -- the panel lists the component and counts a click, and the count of
    -- renders climbs every second for Start.hs, whose timer writes a new
    -- state on every tick, and stays put for Solution.hs, which writes only
    -- a new minute.

    ( "16-devtools"
    ,
      ( onResult $ \page result -> do
          more <- devtoolsPanel page result
          unless (more >= 2) $ expectationFailure ("the timer renders every second: " <> show more <> " more renders in 2.5 s")
      , onResult $ \page result -> do
          more <- devtoolsPanel page result
          -- The minute may turn while the test waits.
          unless (more <= 1) $ expectationFailure ("the timer renders only a new minute: " <> show more <> " more renders in 2.5 s")
      )
    )
  , -- Lesson 14's is plain HTML around its custom element, <x-lesson>,
    -- whose shadow root Playwright's selectors go into; Start.hs dispatches
    -- no event and has no count() method, which the exercise adds. The
    -- page's buttons call the element from the page's script, which calls
    -- into the program, and the program's events are heard as it
    -- dispatches them: each settled with the click.

    ( "14-custom-element"
    ,
      ( onResult $ \_ result -> do
          let counter = sees result "x-lesson button"
              heard = sees result ".lesson-heard"
              press' what = click (locator result ("button[data-do=" <> what <> "]"))
          counter "Apples: 0"
          click (locator result "x-lesson button")
          counter "Apples: 1"
          press' "rename"
          counter "Pears: 1"
          press' "ask"
          heard "The element has no count() method."
      , onResult $ \_ result -> do
          let counter = sees result "x-lesson button"
              heard = sees result ".lesson-heard"
              press' what = click (locator result ("button[data-do=" <> what <> "]"))
          counter "Apples: 0"
          click (locator result "x-lesson button")
          heard "The page heard a count event: 1"
          press' "rename"
          counter "Pears: 1"
          press' "ask"
          heard "count() answered 1"
          press' "reset"
          counter "Pears: 0"
      )
    )
  ]

-- | A check of the lesson's page, given the page and the result's element.
onResult :: (Page -> Locator -> IO ()) -> Page -> IO ()
onResult checkPage page = checkPage page (locator page "#learn-result")

-- | Press lesson 17's button and read the answer once the program is quiet:
-- the text, how long it took, and the frames the page drew meanwhile, as
-- the component counts them.
countPrimes :: Site -> Page -> Locator -> IO (Text, Double, Maybe Int)
countPrimes site page result = do
  sees result ".lesson-workers" "Jobs run in a Web Worker."
  (_, took) <- timed $ do
    -- The click's settle has the long timeout: Start.hs counts in the
    -- handler, holding the page's thread.
    click (withText "Count the primes below 50000" (locator (locator (long site page) "#learn-result") "button:not([disabled])"))
    waitQuiet (long site page)
  text <- textContent (locator result ".lesson-answer")
  let drawn = case Text.breakOn "(" text of
        (_, rest) | not (Text.null rest) -> let digits = Text.takeWhile isDigit (Text.drop 1 rest) in if Text.null digits then Nothing else Just (read (Text.unpack digits))
        _ -> Nothing
  pure (text, took, drawn)

-- | Lesson 16's panel: the component's row, a click counted, and how many
-- more renders 2.5 seconds bring after the first tick. The panel shows
-- what the trace reported on a timer of its own (a tenth of a second after
-- a batch's first event), so its row and its counts are waited for as
-- conditions; the seconds are the lesson's clock, which is what is
-- measured.
devtoolsPanel :: Page -> Locator -> IO Int
devtoolsPanel _ result = do
  let row = locator result ".halogen-devtools [data-label=\"Lesson.State\"]"
  waitFor row Attached
  click (withText "+1" (locator result "button"))
  locator row ".halogen-devtools-actions" `shouldHaveText` "1"
  waitFor (first (withText "count = 1" (locator result ".halogen-devtools-state"))) Attached
  -- The first tick, a second in, writes the minute for the first time.
  threadDelay 1500000
  let renders = read . Text.unpack . Text.strip <$> textContent (locator row ".halogen-devtools-renders")
  before <- renders
  threadDelay 2500000
  after <- renders
  pure (after - before)

-- | Lesson 19's scene: a canvas, once Three.js has come from the CDN and
-- drawn it, whose sphere is at the canvas's centre (the camera looks at
-- it, and Solution.hs's floats less than its radius); a click there is
-- raycast to the sphere, whose colour the text under the scene names. It is
-- clicked until the text names another colour, for a minute, since a click
-- before Three.js has loaded and initialised, in its own JavaScript and not
-- in the program, goes nowhere.
sphereColour :: Page -> Locator -> IO ()
sphereColour page result = do
  let canvas = locator result "canvas"
  waitFor canvas Attached
  sees result "p" "The sphere is amber."
  eventuallyWithin 60000 $ do
    box <- boundingBox canvas
    forM_ box $ \b -> when (b.width > 0) $ mouseClick page (b.x + b.width / 2) (b.y + b.height / 2)
    said <- textContent result
    unless (any (\c -> ("The sphere is " <> c <> ".") `Text.isInfixOf` said) ["sky", "rose", "lime"]) $
      expectationFailure ("no click reached the sphere: the result says " <> show said)

spec :: Walk -> SpecWith Page
spec walk = do
  let site = walk.site
  step "compiles every lesson's Start.hs and Solution.hs, two at a time on two pages" $ \page -> do
    requireLoaded walk
    watch <- watchOf walk page
    -- The lessons are compiled two at a time, on this page and on a second
    -- one, each taking every other lesson so that the two halves weigh
    -- about the same: one compile at a time leaves cores idle that a second
    -- compiler can use. The second page is in a context of its own, as a
    -- reader's second browser would be: it loads the app and the compiler
    -- for itself (its Cache API is empty, so the archives are downloaded
    -- again, from the local server), while this page starts on its half.
    let indexed = zip [0 :: Int ..] site.lessons
        halves = ([l | (i, l) <- indexed, even i], [l | (i, l) <- indexed, odd i])
        checks = pageLessons site
    runs <- newIORef Map.empty
    let runHalf w p half = forM_ half $ \lesson -> do
          openLesson site w p lesson
          let pageCheck = lookup lesson.slug checks
          start <- runLesson site p "button.learn-reset" (fst <$> pageCheck)
          solution <- runLesson site p "button.learn-solution" (snd <$> pageCheck)
          modifyIORef' runs (Map.insert lesson.slug (start, solution))
    loopStart <- getMonotonicTime
    withContext page.playwright defaultContextOptions {viewport = Just (1400, 900), acceptDialogs = True} $ \ctx -> do
      other <- withTimeout 30000 <$> newPage ctx
      otherWatch <- newWatch site ctx (const False) walk.allowed
      withFailureContext other $ do
        let otherReady = case snd halves of
              [] -> pure ()
              firstLesson : _ -> do
                since <- getMonotonicTime
                visit (long site other) (site.url <> firstLesson.slug <> "/")
                entered otherWatch other
                readOnce (locator other ".learn-editor .cm-editor") "the second page's editor"
                _ :: Maybe Value <- evaluate other "(window.learnNoReload = 1, null)"
                untilReady site other since "second page: "
        -- Both halves run to their end even when one fails, so that every
        -- lesson's line below is printed and nothing goes on on the second
        -- page after it is closed.
        (doneA, doneB) <-
          concurrently
            (try @SomeException (runHalf watch page (fst halves)))
            (try @SomeException (otherReady >> runHalf otherWatch other (snd halves)))
        loopEnd <- getMonotonicTime
        done <- readIORef runs
        failed <- newIORef False
        -- The lines in lesson order, as the lessons are listed.
        forM_ site.lessons $ \lesson -> forM_ (Map.lookup lesson.slug done) $ \(start, solution) -> do
          let compiledAndRan r = "Compiled and ran" `Text.isPrefixOf` r.outcome
              startOk = compiledAndRan start && null start.errors && isNothing start.checked
              solutionOk = compiledAndRan solution && null solution.diagnostics && shown solution && isNothing solution.checked
              reported = case lookup lesson.slug specLessons of
                Just (forStart, forSolution) -> reportProblems start forStart "Start.hs" <> reportProblems solution forSolution "Solution.hs"
                Nothing -> ""
              problems =
                Text.concat
                  [ if startOk then "" else "; Start.hs: " <> start.outcome <> " " <> Text.intercalate " | " start.errors <> maybe "" (\c -> " (its page: " <> c <> ")") start.checked
                  , if solutionOk
                      then ""
                      else
                        "; Solution.hs: "
                          <> solution.outcome
                          <> " "
                          <> Text.intercalate " | " solution.diagnostics
                          <> (if shown solution then "" else " (nothing shown)")
                          <> maybe "" (\c -> " (its page: " <> c <> ")") solution.checked
                  , reported
                  ]
              summary r = maybe "no summary" id (lookupSummary r.stdout)
              summaries = case lookup lesson.slug specLessons of
                Just _ -> " (" <> summary start <> "; then " <> summary solution <> ", nothing left mounted)"
                Nothing -> ""
              paged = if lesson.slug `elem` map fst checks then " (its page does what the lesson says, for each)" else ""
              ok = startOk && solutionOk && Text.null reported
          Text.IO.putStrLn ((if ok then "    ok  " else "    FAIL ") <> lesson.slug <> ": Start.hs " <> seconds start.took <> ", Solution.hs " <> seconds solution.took <> (if ok then summaries <> paged else problems))
          unless ok $ modifyIORef' failed (const True)
        note ("every lesson compiled in " <> seconds (loopEnd - loopStart) <> ", two at a time")
        either (expectationFailure . ("the first page's half: " <>) . displayException) pure doneA
        either (expectationFailure . ("the second page's half: " <>) . displayException) pure doneB
        anyFailed <- readIORef failed
        when anyFailed $ expectationFailure "a lesson's Start.hs or Solution.hs did not compile or did not show what it should (its line above says what)"
        -- Following the lessons' links never reloads, on either page.
        forM_ [page, other] $ \p -> do
          flag :: Maybe Int <- evaluate p "window.learnNoReload ?? null"
          unless (flag == Just 1) $ expectationFailure "following the lessons' links reloaded the page"
        checkWatch otherWatch other
  where
    lookupSummary out =
      let isSummary l = case Text.words l of
            [n, ex, f, fl] -> Text.all isDigit n && ex `elem` ["examples,", "example,"] && Text.all isDigit f && fl `elem` ["failures", "failure"]
            _ -> False
       in case filter isSummary (Text.lines out) of
            l : _ -> Just l
            [] -> Nothing
