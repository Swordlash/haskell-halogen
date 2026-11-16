{-# LANGUAGE TemplateHaskell #-}
module Twice.M10 where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T

value :: Int
value = 1

$(let n = BL.length (Aeson.encode (T.pack "x")) in n `seq` pure [])
