{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (component) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify)
import Data.Row (type (.==))
import Data.Text (Text)
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

data State = State {name :: Text, shown :: Bool}

data Action = Rename Text | Toggle

type Slots = ("feature" .== H.Slot Demo.Query Demo.Output ())

component :: forall q o. H.Component q () o BrowserDOM
component =
  H.component initialState render [H.onAction handleAction]
  where
    initialState _ = pure State {name = "Ada", shown = True}
    render :: State -> H.ComponentHTML Action Slots BrowserDOM
    render state =
      HH.div_
        [ HH.p_
            [ HH.input [HP.value state.name, HE.onValueInput Rename]
            , HH.button [HE.onClick (\_ -> Toggle)] [HH.text (if state.shown then "Hide" else "Show")]
            ]
        , if state.shown
            then HH.slot_ "feature" () (Lazy.lazy loading loadFeature) state.name
            else HH.p_ [HH.text "The feature is hidden."]
        ]
    handleAction action = case action of
      Rename name -> modify (\s -> s {name})
      Toggle -> modify (\s -> s {shown = not s.shown})
