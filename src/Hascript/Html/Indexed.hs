module Hascript.Html.Indexed where

import Protolude hiding (div)
import Hascript.Html.IProp
import Hascript.Html
import Data.Row
import Data.Coerce
import Hascript.VDom.VDom
import Web.DOM.Types 
import Web.DOM.Monad hiding (Node)

type Node r w msg = [IProp r msg] -> [Html w msg] -> Html w msg
type Leaf r w msg = [IProp r msg] -> Html w msg

type HTMLdiv = "class" .== Text


class_ :: HasType "class" Text r => ClassName -> IProp r msg
class_ (ClassName txt) = prop "class" txt

rows :: HasType "rows" Int r => Int -> IProp r msg
rows = prop "rows"

onClick :: MonadDOM m => HasType "onClick" (MouseEvent m) r => (MouseEvent m -> msg) -> IProp r msg
onClick hdl = handler "onclick" (mouseHandler $ Just . hdl)

element :: ElemName -> [IProp r msg] -> [Html w msg] -> Html w msg
element name iprops children = Html $ Element Nothing name (coerce iprops) (coerce children)

div :: Node HTMLdiv w msg
div = element "div"