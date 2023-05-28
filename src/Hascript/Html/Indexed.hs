module Hascript.Html.Indexed where

import Protolude hiding (div)
import Hascript.Html.IProp
import Hascript.Html
import Data.Row
import Data.Coerce
import Web.DOM.Types 
import Web.DOM.Monad hiding (Node)

type Node r msg w = [IProp r msg] -> [Html msg w] -> Html msg w
type Leaf r msg w = [IProp r msg] -> Html msg w

type HTMLdiv = "class" .== Text


class_ :: HasType "class" Text r => ClassName -> IProp r msg
class_ (ClassName txt) = prop "class" txt

rows :: HasType "rows" Int r => Int -> IProp r msg
rows = prop "rows"

onClick :: MonadDOM m => HasType "onClick" (MouseEvent m) r => (MouseEvent m -> msg) -> IProp r msg
onClick hdl = handler "onclick" (mouseHandler $ Just . hdl)

element :: ElemName -> [IProp r msg] -> [Html msg w] -> Html msg w
element name iprops children = Element Nothing name (coerce iprops) children

div :: Node HTMLdiv msg w
div = element "div"