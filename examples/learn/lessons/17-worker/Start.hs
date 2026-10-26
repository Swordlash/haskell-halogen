{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (component, jobs) where

import Control.Exception (evaluate)
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

-- The program's table of jobs, which Main.hs hands to Worker.program.
-- Empty, for now.
jobs :: [Worker.SomeJob]
jobs = []

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
        -- Counted right here, in the component's loop.
        n <- H.liftEffect (liftIO (evaluate (countPrimes 50000)))
        after <- gets (.frame)
        let drawn = " (" <> T.pack (show (after - before)) <> " frames drawn meanwhile)"
        modify (\st -> st {counting = False, answer = "Counted " <> T.pack (show n) <> " primes" <> drawn})
