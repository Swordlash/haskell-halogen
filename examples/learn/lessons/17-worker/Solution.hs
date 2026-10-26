{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (component, jobs) where

import Control.Exception (try)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets, modify)
import Data.Text (Text)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Query.Event.Window qualified as Window
import Halogen.Worker qualified as Worker

-- How many primes there are below n, by trial division: slow on purpose.
countPrimes :: Int -> Int
countPrimes n = length (filter isPrime [2 .. n - 1])
  where
    isPrime k = all (\d -> k `rem` d /= 0) (takeWhile (\d -> d * d <= k) (2 : [3, 5 ..]))

-- The count as a job: a function of JSON-able arguments, under a name.
primes :: Worker.Job Int Int
primes = Worker.job "primes" (\n -> pure (countPrimes n))

-- The program's table of jobs, which Main.hs hands to Worker.program.
jobs :: [Worker.SomeJob]
jobs = [Worker.some primes]

data State = State {frame :: Int, counting :: Bool, answer :: Text, workers :: Bool}

data Action = Frame | Count

component :: forall q o m. (MonadIO m) => Worker.Pool -> H.Component q () o m
component pool =
  H.component
    (\_ -> pure State {frame = 0, counting = False, answer = "", workers = False})
    render
    [ H.onInitialize $ do
        void (H.subscribe (Frame <$ Window.animationFrame))
        available <- Worker.available
        modify (\st -> st {workers = available})
    , H.onAction handleAction
    ]
  where
    render st =
      HH.div_
        [ HH.p_
            [ HH.span
                [ HP.styleText ("display: inline-block; width: 1.5em; height: 1.5em; border: 0.25em solid #cbd5e1; border-top-color: #1e3a8a; border-radius: 50%; box-sizing: border-box; vertical-align: middle; transform: rotate(" <> T.pack (show ((st.frame * 6) `mod` 360)) <> "deg)")
                ]
                []
            , HH.text (" frame " <> T.pack (show st.frame))
            ]
        , HH.p_ [HH.button [HP.disabled st.counting, HE.onClick (const Count)] [HH.text "Count the primes below 50000"]]
        , HH.p [HP.class_ (HH.ClassName "lesson-answer")] [HH.text st.answer]
        , HH.p
            [HP.class_ (HH.ClassName "lesson-workers")]
            [HH.text (if st.workers then "Jobs run in a Web Worker." else "No Web Worker here: jobs run on the page.")]
        ]

    handleAction = \case
      Frame -> modify (\st -> st {frame = st.frame + 1})
      Count -> do
        modify (\st -> st {counting = True, answer = "Counting..."})
        before <- gets (.frame)
        -- A wait, so a liftIO: on a thread of its own, while the tree goes
        -- on. With a worker the count is on the worker's thread.
        outcome <- liftIO (try (Worker.run pool primes 50000))
        after <- gets (.frame)
        let drawn = " (" <> T.pack (show (after - before)) <> " frames drawn meanwhile)"
        modify $ \st ->
          st
            { counting = False
            , answer = case outcome of
                Right n -> "The job primes counted " <> T.pack (show n) <> " primes" <> drawn
                Left (e :: Worker.WorkerError) -> "The job " <> e.job <> " failed: " <> e.message
            }
