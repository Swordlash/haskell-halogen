{-# LANGUAGE StaticPointers #-}

-- | Counting primes, which takes the page a second or two: on the page's
-- thread, where it stops the spinner beside it, or in a Web Worker, where
-- the spinner goes on turning, or in both of the pool's workers at once, a
-- part of the range each. The spinner is turned by the component itself, a
-- frame at a time, so it turns only while the component can render; and
-- each result says how many frames the component drew while it was being
-- computed.
--
-- The jobs are exported, which is what "Halogen.Worker" advises for every
-- static pointer.
module Example.Worker
  ( component
  , primes
  , primesBetween
  , failing
  , countPrimes
  , size
  , parts
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
import Protolude hiding (State)
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

primes :: StaticPtr (Worker.Job Int Int)
primes = static (Worker.job (\n -> pure (countPrimes n)))

-- | The primes in one part of the range, and which worker counted them
-- ('Nothing' for the page).
primesBetween :: StaticPtr (Worker.Job (Int, Int) (Maybe Int, Int))
primesBetween = static (Worker.job (\(lo, hi) -> (,countBetween lo hi) <$> Worker.whichWorker))

-- | A job that always fails, to show what a failure looks like.
failing :: StaticPtr (Worker.Job () Int)
failing = static (Worker.job (\() -> throwIO (userError "this job always fails")))

-- | One part of the range as it came back: the part, its count and the
-- worker that counted it.
data Part = Part {range :: (Int, Int), count :: Int, worker :: Maybe Int}

data Outcome = Outcome
  { place :: Text
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
  , note :: Text
  }

data Action = Frame | Here | InWorker | Spread | Fail | Terminate

-- | The page, with the pool its jobs run on.
component :: forall q i o m. (MonadIO m) => Worker.Pool -> H.Component q i o m
component pool =
  H.component
    (\_ -> pure State {frame = 0, workers = False, running = Nothing, outcome = Nothing, spread = [], note = ""})
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
            , button "worker" InWorker (if st.workers then "Count in a worker" else "Count with Worker.run (here, on the page)")
            , button "spread" Spread (if st.workers then "Count in both workers, a part each" else "Count in parts with Worker.start (here, on the page)")
            , button "fail" Fail "Run a job that fails"
            , HH.button [HP.id "terminate", HE.onClick (const Terminate)] [HH.text "Terminate the workers"]
            ]
        , HH.p [HP.id "status"] [HH.text (maybe st.note ("Counting " <>) st.running)]
        , HH.p [HP.id "result"] [HH.text (maybe "" describe st.outcome)]
        , HH.ol [HP.id "parts"] (map part st.spread)
        ]
      where
        button name action label =
          HH.button [HP.id name, HP.disabled (isJust st.running), HE.onClick (const action)] [HH.text label]
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
            <> show size
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
      Here -> measure "here" $ H.liftEffect $ liftIO $ do
        start <- getMonotonicTime
        count <- evaluate (countPrimes size)
        end <- getMonotonicTime
        pure (Right count, (end - start) * 1000)
      -- In a worker: the wait is a liftIO, on a thread of its own, so the
      -- loop renders meanwhile, and the work is on the worker's thread, so
      -- the page draws. Where no worker can be made, Worker.run counts on
      -- that thread itself, still beside the loop, but on the page's.
      InWorker -> do
        inWorker <- gets (.workers)
        measure (if inWorker then "in a worker" else "with Worker.run") $ liftIO $ do
          start <- getMonotonicTime
          result <- try (Worker.run pool primes size)
          end <- getMonotonicTime
          pure (first (\(e :: WorkerError) -> e.message) result, (end - start) * 1000)
      -- The range in parts, each given to the pool with Worker.start, which
      -- gives a promise at once, so the two workers count side by side
      -- (they take turns, a part each); mapConcurrently waits for them all
      -- and gives the counts in the parts' order, whichever finishes first.
      -- Where no worker can be made, each start counts its part before it
      -- returns.
      Spread -> do
        inWorker <- gets (.workers)
        measure (if inWorker then "in both workers" else "in parts with Worker.start") $ do
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
      Fail -> measure "the failing job" $ liftIO $ do
        result <- try (Worker.run pool failing ())
        pure (first (\(e :: WorkerError) -> e.message) result, 0)
      Terminate -> do
        Worker.terminate pool
        modify (\st -> st {note = "The workers were terminated: the next job starts one again."})

    measure :: Text -> H.HalogenM State Action Empty o m (Either Text Int, Double) -> H.HalogenM State Action Empty o m ()
    measure place compute = do
      modify (\st -> st {running = Just place, outcome = Nothing, spread = [], note = ""})
      before <- gets (.frame)
      (answer, millis) <- compute
      after <- gets (.frame)
      modify (\st -> st {running = Nothing, outcome = Just Outcome {place, answer, millis, frames = after - before}})
