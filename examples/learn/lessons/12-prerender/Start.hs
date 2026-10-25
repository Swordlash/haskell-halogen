{-# LANGUAGE OverloadedStrings #-}

module Lesson (component) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (modify)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE

data Action = Increment

-- A build renders this natively, in MemDOM, with renderToHtml component ();
-- the program around this module mounts it with hydrateUI rather than runUI.
component :: forall q o m. (MonadIO m) => H.Component q () o m
component =
  H.component initialState render [H.onAction handleAction]
  where
    initialState _ = pure (0 :: Int)
    render count =
      HH.div_
        [ HH.p_ [HH.text "Count: ", HH.text (T.pack (show count))]
        , HH.button [HE.onClick (\_ -> Increment)] [HH.text "+"]
        ]
    handleAction Increment = modify (+ 1)
