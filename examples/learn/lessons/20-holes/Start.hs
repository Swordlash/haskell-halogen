{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Lesson (component) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (modify)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP

data State = State {name :: Text, count :: Int}

data Action = Rename Text | Increment

component :: forall q o m. (MonadIO m) => H.Component q () o m
component = H.component initialState render [H.onAction handleAction]

initialState _ = pure State {name = "Ada", count = 0}

render st =
  HH.div_
    [ HH.input [HP.value st.name, HE.onValueInput Rename]
    , HH.button [HE.onClick (\_ -> Increment)] [HH.text "+1"]
    , HH.p_ [HH.text (label st)]
    ]

handleAction :: (MonadIO m) => Action -> H.HalogenM State Action Empty o m ()
handleAction action = case action of
  -- Two holes: GHC says what each must be.
  Increment -> modify _
  Rename new -> modify (_rename new)

bump st = st {count = st.count + 1}

-- And a hole in a type.
label :: State -> _
label st = st.name <> " has counted to " <> T.pack (show st.count) <> "."
