{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- Quotes that parse but do not type check; the plugin expands them, so
-- the type checker reports each error at its hole.
module Typed (view, loop) where

import Data.Text (Text)
import Halogen.HTML (HTML)
import Halogen.TH (hh, hhs)

view :: Int -> HTML w i
view n = [hh|
  <div>
    <button onClick={{ n }}>x</button>
  </div>
|]

loop :: [Text] -> [HTML w i]
loop xs = [hhs|{% for (x, y) in xs %}<b>{{ x }}</b>{% end %}|]
