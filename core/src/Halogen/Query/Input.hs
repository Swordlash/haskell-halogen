module Halogen.Query.Input where

import HPrelude
import Web.DOM.Element

newtype RefLabel = RefLabel Text

data Input msg
  = Action msg
  | RefUpdate RefLabel (Maybe Element)
  deriving (Functor)
