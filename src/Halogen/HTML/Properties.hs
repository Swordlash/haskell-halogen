module Halogen.HTML.Properties where

import Data.Coerce
import Data.Row
import Halogen.HTML.Core (IsProp (..))
import Halogen.Query.Input
import Halogen.VDom.DOM.Prop
import Protolude hiding (Handler)
import Web.HTML.Common

newtype IProp (r :: Row Type) msg = IProp (Prop (Input msg))

prop :: (IsProp value) => PropName value -> value -> IProp r msg
prop name val = IProp $ Property name (toPropValue val)

attr :: AttrName -> Text -> IProp r msg
attr name val = IProp $ Attribute Nothing name val

class_ :: (HasType "className" Text r) => ClassName -> IProp r msg
class_ (ClassName txt) = prop "className" txt

rows :: (HasType "rows" Int r) => Int -> IProp r msg
rows = prop "rows"

-- cast property from smaller row to greater row
castProp :: (Subset r1 r2) => IProp r1 msg -> IProp r2 msg
castProp = coerce
