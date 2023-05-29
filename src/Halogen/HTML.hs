module Halogen.HTML where

import Halogen.VDom.DOM.Prop
import Halogen.VDom.Types

newtype HTML w msg = HTML (VDOM [Prop msg] w)
