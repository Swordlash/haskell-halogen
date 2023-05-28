module Hascript.Html.IProp where

import Protolude hiding (Handler)
import Data.Row
import Hascript.Html.Prop
import Data.Coerce
import Web.DOM.Types
import Web.DOM.Monad


newtype IProp (r :: Row Type) msg = IProp (Prop msg)

newtype ClassName = ClassName Text
  deriving (Eq, Ord, Show, IsString)


prop :: IsPropValue value => PropName -> value -> IProp r msg
prop name val = IProp $ Property name (propValue val)

attr :: AttrName -> Text -> IProp r msg
attr name val = IProp $ Attribute Nothing name val

handler :: forall m msg r. EventType -> (Event m -> Maybe msg) -> IProp r msg
handler evt hdl = IProp $ Handler evt hdl

-- cast property from smaller row to greater row
castProp :: Subset r1 r2 => IProp r1 msg -> IProp r2 msg
castProp = coerce
