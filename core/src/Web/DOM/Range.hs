{-# LANGUAGE TemplateHaskell #-}

-- | A @Range@, a stretch of the document, as functions. Meant to be
-- imported qualified (@Range.selectNodeContents@). The field itself
-- (@range.selectNodeContents@) is 'Range''s instance, in
-- "Web.DOM.Internal.Types". A range is made by
-- 'Web.HTML.HTMLDocument.createRange', and selected with "Web.Selection".
module Web.DOM.Range
  ( Range (..)
  , selectNodeContents
  )
where

import Halogen.JSBits.Fields (jsFieldFunctions)
import Web.DOM.Internal.Types (Range (..))

$(jsFieldFunctions ''Range [])
