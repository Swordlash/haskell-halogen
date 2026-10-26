{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

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

data State = State {draft :: Text, items :: [Text]}

data Action = Typed Text | Add | Clear

-- Nothing here knows that it is sent over a wire: the program around it
-- mounts it in RemoteDOM.
component :: forall q o m. (MonadIO m) => H.Component q () o m
component = H.component initialState render [H.onAction handleAction]

initialState :: (Applicative m) => () -> m State
initialState _ = pure State {draft = "", items = ["milk", "bread"]}

render :: State -> H.ComponentHTML Action Empty m
render st =
  HH.div_
    [ -- The page's input events cross the wire with the input's value in
      -- them, which onValueInput reads as it would in a page.
      HH.input [HP.value st.draft, HP.placeholder "Something to buy", HE.onValueInput Typed]
    , HH.button [HE.onClick (\_ -> Add)] [HH.text "Add"]
    , HH.button [HE.onClick (\_ -> Clear)] [HH.text "Clear"]
    , HH.ul_ [HH.li_ [HH.text item] | item <- st.items]
    ]

handleAction :: (MonadIO m) => Action -> H.HalogenM State Action Empty o m ()
handleAction action = case action of
  Typed text -> modify (\st -> st {draft = text})
  Add -> modify $ \st ->
    let item = T.strip st.draft
     in if T.null item then st else st {draft = "", items = st.items <> [item]}
  Clear -> modify (\st -> st {items = []})
