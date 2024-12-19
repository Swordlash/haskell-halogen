module Halogen.Component where

import Data.Row (Row)
import Halogen.Data.Slot
import Halogen.HTML.Core
import Halogen.Query.HalogenM
import Halogen.Query.HalogenQ
import Protolude
import Control.Monad.Parallel
import Halogen.VDom.Thunk

data ComponentSlotBox slots m msg = forall query input output. ComponentSlotBox
  { get :: forall slot. SlotStorage slots slot -> Maybe (slot query output)
  , pop :: forall slot. SlotStorage slots slot -> Maybe (slot query output, SlotStorage slots slot)
  , set :: forall slot. slot query output -> SlotStorage slots slot -> SlotStorage slots slot
  , component :: Component query input output m
  , input :: input
  , output :: output -> Maybe msg
  }

data ComponentSlot (slots :: Row Type) m msg
  = ComponentSlot (ComponentSlotBox slots m msg)
  | ThunkSlot (Thunk (HTML (ComponentSlot slots m msg)) msg)

data ComponentSpec state query action slots input output m = ComponentSpec
  { initialState :: input -> state
  , render :: state -> HTML (ComponentSlot slots m action) action
  , eval :: HalogenQ query action input ~> HalogenM state action slots output m
  }

data Component query input output m
  = forall model msg slots.
  Component (ComponentSpec model query msg slots input output m)
