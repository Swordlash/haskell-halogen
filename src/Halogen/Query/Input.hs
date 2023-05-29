module Halogen.Query.Input where

import Halogen.VDom.DOM.Monad
import Protolude

newtype RefLabel = RefLabel Text

data Input msg m
  = Action msg
  | RefUpdate RefLabel (Maybe (Element m))
