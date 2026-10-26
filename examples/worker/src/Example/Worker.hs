-- | Counting primes, which takes the page a second or two: on the page's
-- thread, where it stops the spinner beside it, or in a Web Worker, where
-- the spinner goes on turning. The spinner is turned by the component
-- itself, a frame at a time, so it turns only while the component can
-- render; and each result says how many frames the component drew while it
-- was being computed.
module Example.Worker
  ( component
  , jobs
  , primes
  , failing
  , countPrimes
  , size
  )
where

import Data.Row (Empty)
import GHC.Clock (getMonotonicTime)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Query.Event.Window qualified as Window
import Halogen.Worker (WorkerError (..))
import Halogen.Worker qualified as Worker
import Protolude hiding (State)
import System.IO.Error (userError)
import Text.Printf (printf)

-- | How many primes there are below @n@, by trial division: slow on
-- purpose: a second or two of work for two million.
countPrimes :: Int -> Int
countPrimes n = length (filter isPrime [2 .. n - 1])
  where
    isPrime k = all (\d -> k `rem` d /= 0) (takeWhile (\d -> d * d <= k) (2 : [3, 5 ..]))

-- | Where the primes are counted up to.
size :: Int
size = 2000000

-- | The program's jobs: its table, which the page and every worker share.
jobs :: [Worker.SomeJob]
jobs = [Worker.some primes, Worker.some failing]

primes :: Worker.Job Int Int
primes = Worker.job "primes" (\n -> pure (countPrimes n))

-- | A job that always fails, to show what a failure looks like.
failing :: Worker.Job () Int
failing = Worker.job "failing" (\() -> throwIO (userError "this job always fails"))

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
  , note :: Text
  }

data Action = Frame | Here | InWorker | Fail | Terminate

-- | The page, with the pool its jobs run on.
component :: forall q i o m. (MonadIO m) => Worker.Pool -> H.Component q i o m
component pool =
  H.component
    (\_ -> pure State {frame = 0, workers = False, running = Nothing, outcome = Nothing, note = ""})
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
            , button "fail" Fail "Run a job that fails"
            , HH.button [HP.id "terminate", HE.onClick (const Terminate)] [HH.text "Terminate the workers"]
            ]
        , HH.p [HP.id "status"] [HH.text (maybe st.note ("Counting " <>) st.running)]
        , HH.p [HP.id "result"] [HH.text (maybe "" describe st.outcome)]
        ]
      where
        button name action label =
          HH.button [HP.id name, HP.disabled (isJust st.running), HE.onClick (const action)] [HH.text label]

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
      Fail -> measure "the failing job" $ liftIO $ do
        result <- try (Worker.run pool failing ())
        pure (first (\(e :: WorkerError) -> e.message) result, 0)
      Terminate -> do
        Worker.terminate pool
        modify (\st -> st {note = "The workers were terminated: the next job starts one again."})

    measure :: Text -> H.HalogenM State Action Empty o m (Either Text Int, Double) -> H.HalogenM State Action Empty o m ()
    measure place compute = do
      modify (\st -> st {running = Just place, outcome = Nothing, note = ""})
      before <- gets (.frame)
      (answer, millis) <- compute
      after <- gets (.frame)
      modify (\st -> st {running = Nothing, outcome = Just Outcome {place, answer, millis, frames = after - before}})
