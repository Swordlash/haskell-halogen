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
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure (0 :: Int)
      , H.render = render
      , H.eval = H.mkEval H.defaultEval
      }
  where
    render count =
      HH.div_
        -- Neither button has an HE.onClick yet, so clicking them does
        -- nothing: giving them one is the exercise.
        [ HH.button [] [HH.text "-"]
        , HH.button [] [HH.text "+"]
        , HH.p_ [HH.text ("Count: " <> T.pack (show count))]
        ]
