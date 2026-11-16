{-# LANGUAGE TemplateHaskell #-}
module Light.M01 where

value :: Int
value = 1

$(pure [])
