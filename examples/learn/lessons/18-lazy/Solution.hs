{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (component) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify)
import Data.Row (type (.==))
import Data.Text (Text)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Lazy qualified as Lazy
import Halogen.Lazy.Demo qualified as Demo
import Halogen.Lazy.Dyld qualified as Dyld
import Halogen.VDom.DOM.Monad (BrowserDOM)

-- The demo library: a shared library that the page fetches, and GHC's
-- dynamic linker links into this program, when the feature is shown.
library :: Dyld.Library
library = Dyld.library "rootfs/layers/lazy/liblazy-demo.so" Demo.entry

loadFeature :: BrowserDOM (Demo.Feature BrowserDOM)
loadFeature = liftIO (Dyld.load library)

loading :: Lazy.Loading
loading =
  Lazy.Loading
    { Lazy.placeholder = HH.p_ [HH.text "Loading the feature..."]
    , Lazy.failed = \message -> HH.p_ [HH.text ("The feature did not load: " <> message)]
    }

data State = State
  { name :: Text
  , shown :: Bool
  , heard :: Text
  , feature :: BrowserDOM (Demo.Feature BrowserDOM)
  }

data Action = Rename Text | Toggle | Heard Demo.Output | WaveAtIt | Ask

type Slots = ("feature" .== H.Slot Demo.Query Demo.Output ())

component :: forall q o. H.Component q () o BrowserDOM
component =
  H.component initialState render [H.onAction handleAction]
  where
    initialState _ = do
      feature <- Lazy.memo loadFeature
      pure State {name = "Ada", shown = True, heard = "Nothing heard yet.", feature}
    render :: State -> H.ComponentHTML Action Slots BrowserDOM
    render state =
      HH.div_
        [ HH.p_
            [ HH.input [HP.value state.name, HE.onValueInput Rename]
            , HH.button [HE.onClick (\_ -> Toggle)] [HH.text (if state.shown then "Hide" else "Show")]
            , HH.button [HE.onClick (\_ -> WaveAtIt)] [HH.text "Wave at it"]
            , HH.button [HE.onClick (\_ -> Ask)] [HH.text "Ask it"]
            ]
        , if state.shown
            then HH.slot "feature" () (Lazy.lazy loading state.feature) state.name Heard
            else HH.p_ [HH.text "The feature is hidden."]
        , HH.p_ [HH.text state.heard]
        ]
    handleAction action = case action of
      Rename name -> modify (\s -> s {name})
      Toggle -> modify (\s -> s {shown = not s.shown})
      Heard (Demo.Waved waves) -> modify (\s -> s {heard = "The feature has waved " <> times waves <> "."})
      WaveAtIt -> H.tell "feature" () Demo.Wave
      Ask -> do
        waves <- H.request "feature" () Demo.GetWaves
        let answer = maybe "nothing: it is not there yet" (\n -> "it has waved " <> times n) waves
        modify (\s -> s {heard = "Asked, the feature says " <> answer <> "."})
    times :: Int -> Text
    times n = T.pack (show n) <> if n == 1 then " time" else " times"
