module Hascript.Html where

import Hascript.VDom.VDom
import Hascript.Html.Prop

newtype Html w msg = Html (VDOM [Prop msg] w)