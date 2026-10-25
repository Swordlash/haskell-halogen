{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (component) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (modify)
import DOM.HTML.Indexed (ButtonType (..), InputType (..))
import Data.Text (Text)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP

data State = State {name :: Text, shout :: Bool}

data Action = SetName Text | SetShout Bool | Submit

component :: forall q o m. (MonadIO m) => H.Component q () o m
component =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure State {name = "", shout = False}
      , H.render = render
      , H.eval = H.mkEval H.defaultEval {H.handleAction = handleAction}
      }
  where
    render state =
      HH.form
        [HE.preventingDefault (HE.onSubmit (\_ -> Submit))]
        [ HH.input [HP.value state.name, HE.onValueInput SetName, HP.placeholder "A name"]
        , HH.label_
            [HH.input [HP.type_ InputCheckbox, HP.checked state.shout, HE.onChecked SetShout], HH.text " Shout"]
        , HH.button [HP.type_ ButtonSubmit] [HH.text "Add"]
        , HH.p_ [HH.text ("You are typing: " <> state.name)]
        ]
    handleAction action = case action of
      SetName text -> modify (\s -> s {name = text})
      SetShout ticked -> modify (\s -> s {shout = ticked})
      Submit -> modify (\s -> s {name = ""})
