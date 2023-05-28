module Hascript.Component where

import Protolude
import Hascript.Html
import Hascript.Monad
import Data.Row (Row)
import Hascript.Data.Slot

data ComponentSlot (slots :: Row Type) m msg = forall query input output.
  ComponentSlot
  { get :: forall slot. SlotStorage slots slot -> Maybe (slot query output)
  , pop :: forall slot. SlotStorage slots slot -> Maybe (slot query output, SlotStorage slots slot)
  , set :: forall slot. slot query output -> SlotStorage slots slot -> SlotStorage slots slot
  , component :: Component query input output m
  , input :: input
  , output :: output -> Maybe msg
  }

data Component query input output m = forall model msg slots. Component 
  { initState    :: input -> m model
  , render       :: model -> Html msg (ComponentSlot slots m msg)
  , handleAction :: model -> HascriptM model msg slots output m ()
  }