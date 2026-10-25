{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lesson (component) where

import Clay (Css)
import Clay qualified as C
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (modify)
import Data.Text (Text)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.TH (clay, hh)

data State = State {name :: Text, likes :: Int, items :: [Text]}

data Action = Like

component :: forall q o m. (MonadIO m) => H.Component q () o m
component =
  H.component initialState render [H.onAction handleAction]
  where
    initialState _ = pure State {name = "Ada", likes = 0, items = ["Read the lesson", "Write a quote", "Compile it"]}
    render st =
      [hh|
        <div class="card">
          {{ style }}
          <h3 title={{ "Written for " <> st.name }}>Hello, {{ st.name }}</h3>
          <button onClick={{ \_ -> Like }} disabled={{ st.likes >= 3 }}>Like</button>
          <p class="likes {% if st.likes > 0 %}liked{% end %}">{{ T.pack (show st.likes) }} likes</p>
          <ol class="items">
            {% for item in st.items %}
              <li>{{ item }}</li>
            {% end %}
          </ol>
        </div>
      |]
    handleAction Like = modify (\st -> st {likes = st.likes + 1})

style :: Css
style =
  [clay|
    .card {
      font-family: system-ui, sans-serif;
      .liked { color: {{ accent }}; font-weight: bold; }
      .items {
        padding-left: 24px;
        li { padding: 2px 0; }
        li:hover { color: {{ accent }}; }
      }
    }
  |]
  where
    accent = C.rgb 0xc0 0x30 0x60
