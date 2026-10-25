{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (component) where

import Control.Monad.State (modify)
import Control.Monad.UUID (MonadUUID)
import Data.Row (type (.+), type (.==))
import Data.Text (Text)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Material.Button qualified as Button
import Halogen.Material.Checkbox qualified as Checkbox
import Halogen.Material.Monad (MonadMaterial)
import Halogen.Material.TextField qualified as TextField

type Slots =
  ("field" .== H.Slot TextField.TextFieldQuery TextField.TextFieldOutput Int)
    .+ ("news" .== H.Slot Checkbox.CheckboxQuery Checkbox.CheckboxChange ())
    .+ ("send" .== H.Slot H.VoidF Button.ButtonClicked ())

data State = State {name :: Text, email :: Text, news :: Bool, sent :: Text}

data Action = Name Text | Email Text | News Bool | Send

-- The program around this module adds Material's script and stylesheet to
-- the page, then mounts this with m = BrowserDOM.
component :: forall q o m. (MonadMaterial m, MonadUUID m) => H.Component q () o m
component =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure State {name = "", email = "", news = False, sent = "Nothing sent yet."}
      , H.render = render
      , H.eval = H.mkEval H.defaultEval {H.handleAction = handleAction}
      }
  where
    render :: State -> H.ComponentHTML Action Slots m
    render st =
      HH.div_
        [ HH.slot "field" 0 TextField.textField TextField.emptyTextFieldSpec {TextField.label = Just "Name"} (\(TextField.InputChanged t) -> Name t)
        , HH.slot "field" 1 TextField.textField TextField.emptyTextFieldSpec {TextField.label = Just "Email"} (\(TextField.InputChanged t) -> Email t)
        , HH.slot "news" () Checkbox.checkbox Checkbox.emptyCheckboxSpec {Checkbox.label = "Send me news"} (\(Checkbox.CheckboxChange b) -> News b)
        , HH.slot "send" () Button.button Button.emptyButtonSpec {Button.label = "Send", Button.style = Just Button.Raised} (\_ -> Send)
        , HH.p_ [HH.text st.sent]
        ]
    handleAction action = case action of
      Name t -> modify (\st -> st {name = t})
      Email t -> modify (\st -> st {email = t})
      News b -> modify (\st -> st {news = b})
      Send -> modify (\st -> st {sent = "Sent: " <> st.name <> " <" <> st.email <> ">" <> (if st.news then ", with news" else "")})
