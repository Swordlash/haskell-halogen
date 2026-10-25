{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Lesson (component, spec) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (modify)
import Data.Text (Text)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Test.Hspec.Halogen

data State = State {name :: Text, count :: Int}

data Action = Named Text | Increment

component :: forall q o m. (MonadIO m) => H.Component q () o m
component =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure State {name = "", count = 0}
      , H.render = render
      , H.eval = H.mkEval H.defaultEval {H.handleAction = handleAction}
      }
  where
    render st =
      HH.div_
        [ HH.input [HP.class_ (HH.ClassName "name"), HP.placeholder "Your name", HE.onValueInput Named]
        , HH.p [HP.class_ (HH.ClassName "greeting")] [HH.text ("Hello, " <> st.name <> "!")]
        , HH.button [HP.class_ (HH.ClassName "increment"), HE.onClick (\_ -> Increment)] [HH.text "+"]
        , HH.p_ [HH.text "Count: ", HH.span [HP.class_ (HH.ClassName "count")] [HH.text (T.pack (show st.count))]]
        ]
    handleAction (Named t) = modify (\st -> st {name = t})
    handleAction Increment = modify (\st -> st {count = st.count + 1})

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = describe "Lesson.component" $ do
  it "greets by name" $ runPage $ do
    ui <- mount m component ()
    find ui "input.name" >>= (`typeText` "Ada")
    find ui ".greeting" >>= (`shouldHaveText` "Hello, Ada!")

  it "counts clicks" $ runPage $ do
    ui <- mount m component ()
    button <- find ui "button.increment"
    click button
    click button
    find ui ".count" >>= (`shouldHaveText` "2")

  it "greets a new name once the old one is cleared" $ runPage $ do
    ui <- mount m component ()
    field <- find ui "input.name"
    typeText field "Ada"
    clear field
    typeText field "Grace"
    find ui ".greeting" >>= (`shouldHaveText` "Hello, Grace!")
