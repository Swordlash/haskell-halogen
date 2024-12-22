module Halogen.HTML.Elements where

import Data.Coerce
import Data.Row
import HPrelude hiding (div)
import Halogen.HTML.Core (HTML (..))
import Halogen.HTML.Properties
import Halogen.VDom.Types
import Web.UIEvent.MouseEvent

type Node r w msg = [IProp r msg] -> [HTML w msg] -> HTML w msg

type Leaf r w msg = [IProp r msg] -> HTML w msg

-- TODO
type HTMLdiv = "class" .== Text

type HTMLbutton = "class" .== Text .+ "onClick" .== MouseEvent

element :: ElemName -> [IProp r msg] -> [HTML w msg] -> HTML w msg
element eln iprops htmls = HTML $ Elem Nothing eln (coerce iprops) (coerce htmls)

div :: Node HTMLdiv w msg
div = element "div"

div_ :: [HTML w i] -> HTML w i
div_ = div []

button :: forall w i. Node HTMLbutton w i
button = element (ElemName "button")
