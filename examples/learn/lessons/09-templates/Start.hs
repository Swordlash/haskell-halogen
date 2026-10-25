{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lesson (component) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (modify)
import Data.Text (Text)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.TH (hh)

data State = State {name :: Text, likes :: Int, items :: [Text]}

data Action = Like

component :: forall q o m. (MonadIO m) => H.Component q () o m
component =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure State {name = "Ada", likes = 0, items = ["Read the lesson", "Write a quote", "Compile it"]}
      , H.render = render
      , H.eval = H.mkEval H.defaultEval {H.handleAction = handleAction}
      }
  where
    render st =
      [hh|
        <div class="card">
          <h3 title={{ "Written for " <> st.name }}>Hello, {{ st.name }}</h3>
          <button onClick={{ \_ -> Like }} disabled={{ st.likes >= 3 }}>Like</button>
          <p class="likes {% if st.likes > 0 %}liked{% end %}">{{ T.pack (show st.likes) }} likes</p>
        </div>
      |]
    handleAction Like = modify (\st -> st {likes = st.likes + 1})
