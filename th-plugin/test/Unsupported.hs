{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- A hole with what a template cannot hold: the plugin refuses it as the
-- quoter would, so a module builds the same with and without the plugin.
module Unsupported (view) where

import Data.Text (Text)
import Halogen.HTML (HTML)
import Halogen.TH (hh)

view :: (Text -> Text) -> HTML w i
view f = [hh|<p>{{ proc x -> f -< x }}</p>|]
