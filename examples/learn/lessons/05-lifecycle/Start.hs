{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (component) where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (get, modify)
import Data.Text (Text)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.Query.Event.Window qualified as Window

data State = State {seconds :: Int, message :: Text}

data Action = Initialize | Tick | Remind | Finalize

component :: forall q o m. (MonadIO m) => H.Component q () o m
component =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure State {seconds = 0, message = ""}
      , H.render = render
      , H.eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.initialize = Just Initialize, H.finalize = Just Finalize}
      }
  where
    render state =
      HH.div_
        [ HH.p_ [HH.text ("Mounted " <> T.pack (show state.seconds) <> " seconds ago.")]
        , HH.button [HE.onClick (\_ -> Remind)] [HH.text "Remind me in two seconds"]
        , HH.p_ [HH.text state.message]
        ]
    handleAction action = case action of
      Initialize -> void (H.subscribe (Tick <$ Window.interval 1000))
      Tick -> modify (\s -> s {seconds = s.seconds + 1})
      Remind -> do
        modify (\s -> s {message = "Waiting..."})
        void $ H.fork $ do
          liftIO (threadDelay 2000000)
          modify (\s -> s {message = "Two seconds have passed."})
      Finalize -> do
        state <- get
        liftIO (putStrLn ("Finalized after " <> show state.seconds <> " seconds."))
