{-# LANGUAGE TemplateHaskell #-}

-- | What a drag or a paste carries: a @DataTransfer@. Meant to be imported
-- qualified (@DataTransfer.files@).
module Web.HTML.Event.DataTransfer
  ( DataTransfer (..)
  , files
  )
where

import Data.Foreign (Foreign)
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsFieldFunctions, jsFields)
import Web.File.FileList (FileList (..))

-- | A @DataTransfer@: 'Web.HTML.Event.DragEvent.dataTransfer',
-- 'Web.Clipboard.ClipboardEvent.clipboardData'.
newtype DataTransfer = DataTransfer (Foreign DataTransfer)
  deriving newtype (Inert)

$( jsFields
     ''DataTransfer
     [ -- The files it carries, which is empty for a drag of text. The
       -- browser hands the same list each time it is asked; its files can
       -- be read only while the event that carries them is dispatched, so
       -- read them in the handler.
       Fixed "files" ''FileList
     ]
 )

$(jsFieldFunctions ''DataTransfer [])
