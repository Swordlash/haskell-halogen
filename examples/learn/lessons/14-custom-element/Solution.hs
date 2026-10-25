{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (component, definition) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets, modify)
import Data.Aeson (toJSON)
import Data.Maybe (fromMaybe)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.CustomElement (Definition (..))
import Halogen.CustomElement qualified as CE
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE

data Query a = Reset a | GetCount (Int -> a)

data State = State {label :: Text, count :: Int}

data Action = Increment

component :: forall m. (MonadIO m) => H.Component Query Text Int m
component =
  H.component initialState render [H.onAction handleAction, H.onReceive receive, H.onQuery handleQuery]
  where
    initialState label = pure State {label, count = 0}
    render state = HH.button [HE.onClick (\_ -> Increment)] [HH.text (state.label <> ": " <> T.pack (show state.count))]
    handleAction Increment = do
      modify (\s -> s {count = s.count + 1})
      gets (.count) >>= H.raise
    receive label = modify (\s -> s {label})
    handleQuery :: Query a -> H.HalogenM State Action Empty Int m (Maybe a)
    handleQuery (Reset next) = modify (\s -> s {count = 0}) >> pure (Just next)
    handleQuery (GetCount reply) = Just . reply <$> gets (.count)

definition :: Definition Query Text Int
definition =
  CE.defaultDefinition
    { input = \props -> fromMaybe "Clicks" (CE.attr "label" props)
    , observed = ["label"]
    , output = \count -> Just ("count", toJSON count)
    , methods = [CE.tellMethod "reset" (\() -> Reset), CE.method "count" (\() -> GetCount)]
    , styles = "button { font: inherit; padding: 0.5rem 1rem; border: 2px solid #4338ca; border-radius: 0.5rem; background: #eef2ff; cursor: pointer; }"
    }
