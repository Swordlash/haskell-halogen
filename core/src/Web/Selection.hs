{-# LANGUAGE TemplateHaskell #-}

-- | The reader's selection in the page, @window.getSelection()@, as
-- functions. Meant to be imported qualified (@Selection.addRange@). The
-- fields themselves (@selection.removeAllRanges@) are 'Selection''s
-- instances, in "Web.DOM.Internal.Types". The selection is read with
-- 'Web.HTML.Window.getSelection'.
module Web.Selection
  ( Selection (..)
  , removeAllRanges
  , addRange
  )
where

import Halogen.JSBits.Fields (jsFieldFunctions)
import Web.DOM.Internal.Types (Selection (..))

$(jsFieldFunctions ''Selection [])
