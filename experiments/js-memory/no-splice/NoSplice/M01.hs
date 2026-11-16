module NoSplice.M01 where

import Data.Aeson qualified as Aeson
import Data.Text qualified as T

value :: Aeson.Value
value = Aeson.String (T.pack "x")
