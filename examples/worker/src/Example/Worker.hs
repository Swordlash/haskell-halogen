{-# LANGUAGE StaticPointers #-}

-- | Counting primes, which takes the page a second or two: on the page's
-- thread, where it stops the spinner beside it, or in a Web Worker, where
-- the spinner goes on turning, or in both of the pool's workers at once, a
-- part of the range each. The spinner is turned by the component itself, a
-- frame at a time, so it turns only while the component can render; and
-- each result says how many frames the component drew while it was being
-- computed.
--
-- With "Halogen.Worker.Par" the range is counted in sixteen parts, a task
-- each, which runPar sends to the workers in batches as each is free.
--
-- A job can be stopped: a slow count that checkpoints after each chunk is
-- cancelled by Cancel, a count that does not is ended with its worker by
-- Kill, and a race between a quick count and the slow one cancels the
-- loser. The slow count logs each chunk to the console, prefixed with its
-- worker's number, so that you can see it stop.
--
-- The jobs are exported, which is what "Halogen.Worker" advises for every
-- static pointer.
module Example.Worker
  ( component
  , primes
  , primesBetween
  , primesSlowly
  , failing
  , countPrimes
  , size
  , parts
  , manyParts
  , slowSize
  , raceSize
  )
where

import Data.Row (Empty)
import GHC.Clock (getMonotonicTime)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits.Promise (JSError (..))
import Halogen.JSBits.Promise qualified as Promise
import Halogen.Query.Event.Window qualified as Window
import Halogen.Worker (WorkerError (..))
import Halogen.Worker qualified as Worker
import Halogen.Worker.Par (runPar, task)
import Protolude hiding (State)
import System.IO (hFlush)
import System.IO.Error (userError)
import Text.Printf (printf)

-- | How many primes there are below @n@, by trial division: slow on
-- purpose: a second or two of work for two million.
countPrimes :: Int -> Int
countPrimes = countBetween 2

-- | How many primes there are from @lo@ up to @hi@, @hi@ left out.
countBetween :: Int -> Int -> Int
countBetween lo hi = length (filter isPrime [max 2 lo .. hi - 1])
  where
    isPrime k = all (\d -> k `rem` d /= 0) (takeWhile (\d -> d * d <= k) (2 : [3, 5 ..]))

-- | Where the primes are counted up to.
size :: Int
size = 2000000

-- | The range below 'size' in four parts, for the pool's two workers to
-- take turns with.
parts :: [(Int, Int)]
parts = [(lo, lo + quarter) | lo <- [0, quarter .. size - quarter]]
  where
    quarter = size `div` 4

-- | The range below 'size' in sixteen parts, for 'runPar' to cut into
-- batches.
manyParts :: [(Int, Int)]
manyParts = [(lo, lo + sixteenth) | lo <- [0, sixteenth .. size - sixteenth]]
  where
    sixteenth = size `div` 16

primes :: StaticPtr (Worker.Job Int Int)
primes = static (Worker.job (\n -> pure (countPrimes n)))

-- | The primes in one part of the range, and which worker counted them
-- ('Nothing' for the page).
primesBetween :: StaticPtr (Worker.Job (Int, Int) (Maybe Int, Int))
primesBetween = static (Worker.job (\(lo, hi) -> (,countBetween lo hi) <$> Worker.whichWorker))

-- | Where the slow count counts up to: some ten seconds of work in a
-- worker, a chunk at a time.
slowSize :: Int
slowSize = 6000000

-- | Where the quick count counts up to, in a race with the slow count.
raceSize :: Int
raceSize = 1000000

-- | The primes below @n@, counted a chunk of a hundred thousand numbers at a
-- time, with a 'Worker.checkpoint' after each chunk: where a 'Worker.cancel'
-- reaches the job. Each chunk is logged, so the console shows the count
-- going on, and stopping.
primesSlowly :: StaticPtr (Worker.Job Int Int)
primesSlowly = static (Worker.job countSlowly)

countSlowly :: Int -> IO Int
countSlowly n = go 0 2
  where
    go total lo
      | lo >= n = pure total
      | otherwise = do
          let hi = min n (lo + 100000)
          count <- evaluate (countBetween lo hi)
          worker <- Worker.whichWorker
          putStrLn ("the slow count has counted up to " <> show hi <> maybe " on the page" (\w -> " in worker " <> show w) worker :: Text)
          hFlush stdout
          Worker.checkpoint
          go (total + count) hi

-- | A job that always fails, to show what a failure looks like.
failing :: StaticPtr (Worker.Job () Int)
failing = static (Worker.job (\() -> throwIO (userError "this job always fails")))

-- | One part of the range as it came back: the part, its count and the
-- worker that counted it.
data Part = Part {range :: (Int, Int), count :: Int, worker :: Maybe Int}

data Outcome = Outcome
  { place :: Text
  , below :: Int
  , answer :: Either Text Int
  , millis :: Double
  , frames :: Int
  }

data State = State
  { frame :: Int
  , workers :: Bool
  , running :: Maybe Text
  , outcome :: Maybe Outcome
  , spread :: [Part]
  , current :: Maybe (Promise.Promise Int)
  -- ^ The job a count in a worker, or the slow count, is waiting for, for
  -- Cancel and Kill.
  , note :: Text
  }

data Action = Frame | Here | InWorker | Spread | InTasks | Slow | Race | Cancel | Kill | Fail | Terminate

-- | The page, with the pool its jobs run on.
component :: forall q i o m. (MonadIO m) => Worker.Pool -> H.Component q i o m
component pool =
  H.component
    (\_ -> pure State {frame = 0, workers = False, running = Nothing, outcome = Nothing, spread = [], current = Nothing, note = ""})
    render
    [ H.onInitialize initialize
    , H.onAction handleAction
    ]
  where
    initialize :: H.HalogenM State Action Empty o m ()
    initialize = do
      void (H.subscribe (Frame <$ Window.animationFrame))
      available <- Worker.available
      modify (\st -> st {workers = available})

    render :: State -> H.ComponentHTML Action Empty m
    render st =
      HH.div_
        [ HH.p
            [HP.id "available"]
            [ HH.text $
                if st.workers
                  then "Web Workers are available: the pool starts one when it first needs it."
                  else "No Web Worker can be made here: a job runs on the page."
            ]
        , HH.div
            [HP.class_ (HH.ClassName "spinner-row")]
            [ HH.div
                [ HP.class_ (HH.ClassName "spinner")
                , HP.styleText ("transform: rotate(" <> show ((st.frame * 6) `mod` 360) <> "deg)")
                ]
                []
            , HH.span [HP.id "frames"] [HH.text (show st.frame)]
            , HH.text " frames drawn"
            ]
        , HH.p
            [HP.class_ (HH.ClassName "controls")]
            [ button "here" Here "Count here, on the page"
            , button "worker" InWorker (if st.workers then "Count in a worker" else "Count with Worker.start (here, on the page)")
            , button "spread" Spread (if st.workers then "Count in both workers, a part each" else "Count in parts with Worker.start (here, on the page)")
            , button "tasks" InTasks (if st.workers then "Count in sixteen tasks with runPar" else "Count in sixteen tasks with runPar (here, on the page)")
            , button "fail" Fail "Run a job that fails"
            , HH.button [HP.id "terminate", HE.onClick (const Terminate)] [HH.text "Terminate the workers"]
            ]
        , HH.p
            [HP.class_ (HH.ClassName "controls")]
            [ button "slow" Slow ("Count slowly to " <> show slowSize <> ", with checkpoints")
            , button "race" Race ("Race a count to " <> show raceSize <> " against the slow count")
            , stopping "cancel" Cancel "Cancel"
            , stopping "kill" Kill "Kill"
            ]
        , HH.p [HP.id "status"] [HH.text (maybe st.note ("Counting " <>) st.running)]
        , HH.p [HP.id "result"] [HH.text (maybe "" describe st.outcome)]
        , HH.ol [HP.id "parts"] (map part st.spread)
        ]
      where
        button name action label =
          HH.button [HP.id name, HP.disabled (isJust st.running), HE.onClick (const action)] [HH.text label]
        stopping name action label =
          HH.button [HP.id name, HP.disabled (isNothing st.current), HE.onClick (const action)] [HH.text label]
        part p =
          HH.li
            [HP.attr (HH.AttrName "data-worker") (maybe "page" show p.worker)]
            [ HH.text $
                show p.count
                  <> " primes from "
                  <> show (fst p.range)
                  <> " up to "
                  <> show (snd p.range)
                  <> ", counted "
                  <> maybe "on the page" (\n -> "in worker " <> show n) p.worker
            ]

    describe :: Outcome -> Text
    describe outcome =
      case outcome.answer of
        Left message -> outcome.place <> ": failed: " <> message
        Right count ->
          outcome.place
            <> ": "
            <> show count
            <> " primes below "
            <> show outcome.below
            <> " in "
            <> toS (printf "%.0f" outcome.millis :: [Char])
            <> " ms, while the page drew "
            <> show outcome.frames
            <> " frames"

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Frame -> modify (\st -> st {frame = st.frame + 1})
      -- On the page: in the component's loop, which renders nothing until
      -- the count is done, and on the page's only thread, which the page
      -- needs to draw and to answer clicks.
      Here -> measure "here" size $ H.liftEffect $ liftIO $ do
        start <- getMonotonicTime
        count <- evaluate (countPrimes size)
        end <- getMonotonicTime
        pure (Right count, (end - start) * 1000)
      -- In a worker: the wait is a liftIO, on a thread of its own, so the
      -- loop renders meanwhile, and the work is on the worker's thread, so
      -- the page draws. Where no worker can be made, Worker.start counts on
      -- a thread of its own, still beside the loop, but on the page's.
      InWorker -> do
        inWorker <- gets (.workers)
        stoppable (if inWorker then "in a worker" else "with Worker.start") primes size
      -- The same with a job that checkpoints, which Cancel can stop.
      Slow -> stoppable "the slow count" primesSlowly slowSize
      -- Worker.run cancels its job when its wait is interrupted, so the
      -- losing side of a race is cancelled: the slow count, which stops at
      -- its next checkpoint (its log in the console stops too).
      Race -> measure "the race" raceSize $ liftIO $ do
        start <- getMonotonicTime
        result <- try (race (Worker.run pool primes raceSize) (Worker.run pool primesSlowly slowSize))
        end <- getMonotonicTime
        let answer = case result of
              Left (e :: WorkerError) -> Left e.message
              Right (Left count) -> Right count
              Right (Right _) -> Left "the slow count won"
        pure (answer, (end - start) * 1000)
      Cancel -> gets (.current) >>= traverse_ (Worker.cancel pool)
      Kill -> gets (.current) >>= traverse_ (Worker.kill pool)
      -- The range in parts, each given to the pool with Worker.start, which
      -- gives a promise at once, so the two workers count side by side
      -- (they take turns, a part each); mapConcurrently waits for them all
      -- and gives the counts in the parts' order, whichever finishes first.
      -- Where no worker can be made, each start counts its part on a thread
      -- of its own.
      Spread -> do
        inWorker <- gets (.workers)
        measure (if inWorker then "in both workers" else "in parts with Worker.start") size $ do
          (result, millis) <- liftIO $ do
            start <- getMonotonicTime
            result <- try (Promise.mapConcurrently (Worker.start pool primesBetween) parts)
            end <- getMonotonicTime
            pure (result, (end - start) * 1000)
          case result of
            Left (e :: JSError) -> pure (Left e.message, millis)
            Right counted -> do
              let spread = zipWith (\range (worker, count) -> Part {range, count, worker}) parts counted
              modify (\st -> st {spread})
              pure (Right (sum (map (.count) spread)), millis)
      -- The range in sixteen tasks: traverse collects them, and runPar cuts
      -- them into batches (four for each worker here, of two tasks each)
      -- and gives the next batch to whichever worker has answered its
      -- last; the counts come back in the parts' order. Where no worker
      -- can be made the batches run here, one after the other.
      InTasks -> do
        inWorker <- gets (.workers)
        measure (if inWorker then "in tasks" else "in tasks, here") size $ do
          (result, millis) <- liftIO $ do
            start <- getMonotonicTime
            result <- try (runPar pool (traverse (task primesBetween) manyParts))
            end <- getMonotonicTime
            pure (result, (end - start) * 1000)
          case result of
            Left (e :: WorkerError) -> pure (Left e.message, millis)
            Right counted -> do
              let spread = zipWith (\range (worker, count) -> Part {range, count, worker}) manyParts counted
              modify (\st -> st {spread})
              pure (Right (sum (map (.count) spread)), millis)
      Fail -> measure "the failing job" size $ liftIO $ do
        result <- try (Worker.run pool failing ())
        pure (first (\(e :: WorkerError) -> e.message) result, 0)
      Terminate -> do
        Worker.terminate pool
        modify (\st -> st {note = "The workers were terminated: the next job starts one again."})

    -- A count of the primes below n with the job, which Cancel and Kill can
    -- stop while it runs: started with Worker.start for its promise, kept
    -- in the state, and waited for. Should the wait be interrupted (the
    -- component goes away meanwhile), the job is cancelled, as Worker.run
    -- does.
    stoppable :: Text -> StaticPtr (Worker.Job Int Int) -> Int -> H.HalogenM State Action Empty o m ()
    stoppable place pointer n = measure place n $ do
      -- Timed from before the start: where no worker can be made, the
      -- forked job can run to its end before this thread runs again.
      start <- liftIO getMonotonicTime
      job <- Worker.start pool pointer n
      modify (\st -> st {current = Just job})
      result <- liftIO (Promise.waitCatch job `onException` Worker.cancel pool job)
      end <- liftIO getMonotonicTime
      modify (\st -> st {current = Nothing})
      pure (first (.message) result, (end - start) * 1000)

    measure :: Text -> Int -> H.HalogenM State Action Empty o m (Either Text Int, Double) -> H.HalogenM State Action Empty o m ()
    measure place below compute = do
      modify (\st -> st {running = Just place, outcome = Nothing, spread = [], note = ""})
      before <- gets (.frame)
      (answer, millis) <- compute
      after <- gets (.frame)
      modify (\st -> st {running = Nothing, outcome = Just Outcome {place, below, answer, millis, frames = after - before}})
