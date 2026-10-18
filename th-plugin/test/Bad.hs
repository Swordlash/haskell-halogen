{-# LANGUAGE QuasiQuotes #-}
-- Every kind of broken quote; the plugin reports each at its place.
module Bad (view, style, loop, late, escape, open) where

import Clay (Css, red)
import Data.Text (Text)
import Halogen.HTML (HTML)
import Halogen.TH (clay, hh, hhs)

view :: Int -> HTML w i
view n = [hh|
  <div>
    <span class="n">{{ show n) }}</span>
  </div>
|]

style :: Css
style = [clay| .a { color: {{ red) }}; } |]

loop :: [Text] -> [HTML w i]
loop xs = [hhs|{% for (x in xs %}<b>{{ x }}</b>{% end %}|]

late :: HTML w i
late = [hh|<p>{{
  bad ) }}</p>|]

escape :: HTML w i
escape = [hh|<p>{{	show "\q" }}</p>|]

open :: HTML w i
open = [hh|<div>{{ x</div>|]
