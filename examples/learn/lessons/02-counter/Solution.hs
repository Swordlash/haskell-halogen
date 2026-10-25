{-# LANGUAGE OverloadedStrings #-}

module Lesson (component) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (modify)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE

data Action = Increment | Decrement

component :: forall q o m. (MonadIO m) => H.Component q () o m
component =
  H.component initialState render [H.onAction handleAction]
  where
    initialState _ = pure (0 :: Int)
    render count =
      HH.div_
        [ HH.button [HE.onClick (\_ -> Decrement)] [HH.text "-"]
        , HH.button [HE.onClick (\_ -> Increment)] [HH.text "+"]
        , HH.p_ [HH.text ("Count: " <> T.pack (show count))]
        ]
    handleAction action = case action of
      Increment -> modify (+ 1)
      Decrement -> modify (subtract 1)
