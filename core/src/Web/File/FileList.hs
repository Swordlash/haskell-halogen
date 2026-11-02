{-# LANGUAGE TemplateHaskell #-}

-- | The files of a drop, a paste or a file input: a @FileList@. Meant to be
-- imported qualified (@FileList.items@).
module Web.File.FileList
  ( FileList (..)
  , length
  , item
  , items
  )
where

import Data.Foreign (Foreign)
import HPrelude hiding (length)
import Halogen.JSBits (Inert)
import Halogen.JSBits.Fields (Member (..), jsFieldFunctions, jsFields)
import Web.File.File (File (..))

-- | A @FileList@: 'Web.HTML.Event.DataTransfer.files',
-- 'Web.HTML.HTMLInputElement.files'.
newtype FileList = FileList (Foreign FileList)
  deriving newtype (Inert)

$( jsFields
     ''FileList
     [ -- How many files it holds.
       Fixed "length" [t|Int|]
     , -- The file at a position, from 0, or 'Nothing' past the end.
       Method "item" [t|Int -> IO (Maybe File)|]
     ]
 )

$(jsFieldFunctions ''FileList [])

-- | Every file, in order.
items :: FileList -> IO [File]
items files = catMaybes <$> traverse files.item [0 .. files.length - 1]
