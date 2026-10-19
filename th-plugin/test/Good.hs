{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- Quotes with nothing wrong; the plugin leaves them to their quoters.
module Good (view, style, dot) where

import Clay (Css, red)
import Data.Text (Text)
import Data.Text qualified as T
import Halogen.HTML (HTML)
import Halogen.TH (clay, hh, svg)

view :: [Text] -> HTML w i
view xs = [hh|
  <ul class="list">
    {% for (i, x) in zip [1 :: Int ..] xs %}<li>{{ T.pack (show i) }}. {{ x }}</li>{% end %}
    {% if null xs %}<li>{{
      "nothing" :: Text }}</li>{% end %}
  </ul>
|]

style :: Css
style = [clay| .list { color: {{ red }}; li { margin: 0; } } |]

dot :: Double -> HTML w i
dot x = [svg|<circle cx={{ x }} cy="4" r="2" class="dot"/>|]
