{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- A hole of a type no element can hold; the instance that is missing is
-- reported at the hole, not at the quote, as the application of the
-- generated function takes the hole's place.
module Instance (view) where

import Halogen.HTML (HTML)
import Halogen.TH (hh)

view :: Int -> HTML w i
view n = [hh|<div><span>{{ n }}</span></div>|]
