module Halogen.HTML.Properties where

import Data.Coerce
import Data.Row
import Halogen.VDom.DOM.Monad
import Halogen.VDom.DOM.Prop
import Protolude hiding (Handler)
import Web.DOM.Types

newtype IProp (r :: Row Type) msg = IProp (Prop msg)

newtype ClassName = ClassName Text
  deriving (Eq, Ord, Show, IsString)

prop :: IsPropValue value => PropName -> value -> IProp r msg
prop name val = IProp $ Property name (propValue val)

attr :: AttrName -> Text -> IProp r msg
attr name val = IProp $ Attribute Nothing name val

handler :: forall m msg r. EventType -> (Event m -> Maybe msg) -> IProp r msg
handler evt hdl = IProp $ Handler evt hdl

class_ :: HasType "class" Text r => ClassName -> IProp r msg
class_ (ClassName txt) = prop "class" txt

rows :: HasType "rows" Int r => Int -> IProp r msg
rows = prop "rows"

onClick :: MonadDOM m => HasType "onClick" (MouseEvent m) r => (MouseEvent m -> msg) -> IProp r msg
onClick hdl = handler "onclick" (mouseHandler $ Just . hdl)

-- cast property from smaller row to greater row
castProp :: Subset r1 r2 => IProp r1 msg -> IProp r2 msg
castProp = coerce
