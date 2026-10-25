{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (component) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (modify, put)
import Data.Row (Empty, type (.==))
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE

data Query a = Reset a

data Output = Clicked

data CounterAction = Increment

counter :: forall m. (MonadIO m) => H.Component Query () Output m
counter =
  H.component initialState render [H.onAction handleAction, H.onQuery handleQuery]
  where
    initialState _ = pure (0 :: Int)
    render count = HH.button [HE.onClick (\_ -> Increment)] [HH.text ("Count: " <> T.pack (show count))]
    handleAction Increment = modify (+ 1) >> H.raise Clicked
    handleQuery :: Query a -> H.HalogenM Int CounterAction Empty Output m (Maybe a)
    handleQuery (Reset next) = put 0 >> pure (Just next)

type Slots = ("counter" .== H.Slot Query Output ())

data Action = HeardClick | ResetCounter

component :: forall q o m. (MonadIO m) => H.Component q () o m
component =
  H.component initialState render [H.onAction handleAction]
  where
    initialState _ = pure "Nothing heard yet."
    render :: T.Text -> H.ComponentHTML Action Slots m
    render message =
      HH.div_
        [ HH.slot "counter" () counter () (\Clicked -> HeardClick)
        , HH.button [HE.onClick (\_ -> ResetCounter)] [HH.text "Reset the counter"]
        , HH.p_ [HH.text message]
        ]
    handleAction HeardClick = put "The counter was clicked."
    handleAction ResetCounter = H.tell "counter" () Reset >> put "The parent reset the counter."
