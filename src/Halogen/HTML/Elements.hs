module Halogen.HTML.Elements where

import Data.Coerce
import Data.Row
import Halogen.HTML.Core (HTML(..))
import Halogen.HTML.Properties
import Halogen.VDom.Types
import Protolude hiding (div)

type Node r w msg = [IProp r msg] -> [HTML w msg] -> HTML w msg

type Leaf r w msg = [IProp r msg] -> HTML w msg

type HTMLdiv = "class" .== Text

element :: ElemName -> [IProp r msg] -> [HTML w msg] -> HTML w msg
element eln iprops htmls = HTML $ Elem Nothing eln (coerce iprops) (coerce htmls)

div :: Node HTMLdiv w msg
div = element "div"
