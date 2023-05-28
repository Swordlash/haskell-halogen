module Hascript.Component where

import Protolude
import Hascript.Html
import Hascript.Monad

data Component = forall model msg w. MkComponent 
  { initState    :: IO model
  , render       :: model -> Html msg w
  , handleAction :: model -> HascriptM msg ()
  }