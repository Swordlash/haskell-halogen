{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Clay.Extra.Grid
  ( module Clay.Grid

    -- $gridIntro
  , gridTemplateAreas
  , gridTemplateRows
  )
where

import Clay.Grid
import Clay.Property (noCommas)
import Clay.Size (Size)
import Clay.Stylesheet (Css, key)
import Data.Text (Text)
import Prelude

gridTemplateAreas :: [Text] -> Css
gridTemplateAreas = key "grid-template-areas" . noCommas

gridTemplateRows :: [Size a] -> Css
gridTemplateRows = key "grid-template-rows" . noCommas
