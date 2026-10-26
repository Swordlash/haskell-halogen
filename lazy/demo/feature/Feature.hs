{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The demo library's component, which the tutorial's rootfs builds into
-- @liblazy-demo.so@ with "Entry" (toolchain/build-learn-rootfs.sh), and
-- lazy's test suite renders through "Halogen.Lazy" on every backend.
module Feature (feature) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (get, modify, put)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.Lazy.Demo (Feature, Output (..), Query (..))
import Prelude

data State = State {name :: Text, waves :: Int}

data Action = Pressed

feature :: forall m. (MonadIO m) => Feature m
feature =
  H.component (\name -> pure State {name, waves = 0}) render [H.onAction handleAction, H.onQuery handleQuery, H.onReceive receive]
  where
    render state =
      HH.div_
        [ HH.p_ [HH.text ("Hello, " <> state.name <> "! I was loaded from a library of my own.")]
        , HH.button [HE.onClick (\_ -> Pressed)] [HH.text "Wave"]
        , HH.p_ [HH.text ("Waved " <> Text.pack (show state.waves) <> (if state.waves == 1 then " time." else " times."))]
        ]
    handleAction Pressed = wave
    handleQuery :: Query a -> H.HalogenM State Action Empty Output m (Maybe a)
    handleQuery = \case
      Wave next -> wave >> pure (Just next)
      GetWaves reply -> Just . reply . (.waves) <$> get
    receive name = modify (\state -> state {name})
    wave = do
      state <- get
      let waves = state.waves + 1
      put state {waves}
      H.raise (Waved waves)
