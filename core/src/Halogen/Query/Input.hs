module Halogen.Query.Input where

import HPrelude
import Halogen.VDom.DOM.Prop (ElemRef)
import Web.DOM.Element

newtype RefLabel = RefLabel Text

data Input msg
  = Action msg
  | -- | The element a @ref@ property is on was created or removed. A
    -- removal carries the element that goes, because it can come after the
    -- creation of the element that takes its place under the same label:
    -- among keyed children, those under new keys are built before those
    -- whose keys went are removed. The driver clears the label only if it
    -- still holds the element that goes.
    RefUpdate RefLabel (ElemRef Element)
  deriving (Functor)
