{-# LANGUAGE OverloadedStrings #-}

module Lesson (component) where

import Control.Monad.IO.Class (MonadIO)
import Halogen qualified as H
import Halogen.HTML qualified as HH

component :: forall q o m. (MonadIO m) => H.Component q () o m
component =
  H.component initialState render []
  where
    initialState _ = pure ()
    render () =
      HH.div_
        [ HH.p_ [HH.text "Hello"]
        ]
