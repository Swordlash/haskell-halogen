{-# LANGUAGE TemplateHaskell #-}
module Light.M00 where

value :: Int
value = 1

$(pure [])
