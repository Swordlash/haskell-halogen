module Halogen.HTML.Elements where

import Data.Coerce
import Data.Row
import Halogen.HTML
import Halogen.HTML.Properties
import Halogen.VDom.Types
import Protolude hiding (div)

type Node r w msg = [IProp r msg] -> [HTML w msg] -> HTML w msg

type Leaf r w msg = [IProp r msg] -> HTML w msg

type HTMLdiv = "class" .== Text

element :: ElemName -> [IProp r msg] -> [HTML w msg] -> HTML w msg
element name iprops children = HTML $ Element Nothing name (coerce iprops) (coerce children)

div :: Node HTMLdiv w msg
div = element "div"
