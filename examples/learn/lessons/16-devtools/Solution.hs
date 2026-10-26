{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (component, State (..)) where

import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (get, modify, put)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.Query.Event.Window qualified as Window

-- The panel shows a state with this Show instance, once the program has
-- said so with DevTools.showing.
data State = State {count :: Int, clock :: Text}
  deriving (Eq, Show)

data Action = Increment | Tick

component :: forall q o m. (MonadIO m) => H.Component q () o m
component =
  H.component
    (\_ -> pure State {count = 0, clock = "--:--"})
    render
    [ H.onInitialize (void (H.subscribe (Tick <$ Window.interval 1000)))
    , H.onAction handleAction
    ]
  where
    render st =
      HH.div_
        [ HH.button [HE.onClick (const Increment)] [HH.text "+1"]
        , HH.p_ [HH.text ("Clicked " <> T.pack (show st.count) <> " times. It is " <> st.clock <> " UTC.")]
        ]

    handleAction = \case
      Increment -> modify (\st -> st {count = st.count + 1})
      -- Every second: read the clock, and keep the minute only when it is
      -- another one. Fifty-nine ticks a minute now write nothing, and
      -- nothing renders for them.
      Tick -> do
        now <- liftIO getCurrentTime
        let minute = T.pack (formatTime defaultTimeLocale "%H:%M" now)
        st <- get
        when (st.clock /= minute) $ put st {clock = minute}
