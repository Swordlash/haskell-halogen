module Halogen.Component where

import Data.Row (Row)
import Halogen.Data.Slot
import Halogen.HTML
import Halogen.Query.HalogenM
import Halogen.Query.HalogenQ
import Protolude

data ComponentSlot (slots :: Row Type) m msg = forall query input output.
  ComponentSlot
  { get :: forall slot. SlotStorage slots slot -> Maybe (slot query output)
  , pop :: forall slot. SlotStorage slots slot -> Maybe (slot query output, SlotStorage slots slot)
  , set :: forall slot. slot query output -> SlotStorage slots slot -> SlotStorage slots slot
  , component :: Component query input output m
  , input :: input
  , output :: output -> Maybe msg
  }

data Component query input output m = forall model msg slots.
  Component
  { initState :: input -> m model
  , render :: model -> HTML msg (ComponentSlot slots m msg)
  , eval :: forall a. HalogenQ query msg input a -> HalogenM model msg slots output m a
  }
