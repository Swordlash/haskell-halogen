{-# LANGUAGE TemplateHaskell #-}

-- | The files of a drop, a paste or a file input: a @FileList@. Meant to be
-- imported qualified (@FileList.items@).
module Web.File.FileList
  ( FileList (..)
  , length
  , item
  , items
  , now
  )
where

import Data.Foreign (Foreign, readElements)
import HPrelude hiding (length)
import HPrelude qualified
import Halogen.JSBits (Inert, JSVal, inBrowser)
import Halogen.JSBits.Fields (Member (..), inlineJS, jsFieldFunctions, jsFields)
import Web.File.File (File (..))

-- | A @FileList@: 'Web.HTML.Event.DataTransfer.files',
-- 'Web.HTML.HTMLInputElement.files'.
--
-- Natively a list is what an event made from JSON says of it: an array of
-- files ("Web.File.File" reads each), and empty for anything else.
newtype FileList = FileList (Foreign FileList)
  deriving newtype (Inert)

-- Natively, the files of a JSON array.
nativeFiles :: FileList -> [File]
nativeFiles (FileList value) = map File (readElements value)

nativeItem :: FileList -> Int -> IO (Maybe File)
nativeItem files index
  | index < 0 = pure Nothing
  | otherwise = pure (head (drop index (nativeFiles files)))

$( jsFields
     ''FileList
     [ -- How many files it holds. Read each time: the list a drop's
       -- 'Web.HTML.Event.DataTransfer.files' gives is live, and an input's
       -- list empties when the input is cleared.
       Natively [|pure . HPrelude.length . nativeFiles|] (Live "length" ''Int)
     , -- The file at a position, from 0, or 'Nothing' past the end.
       Natively [|nativeItem|] (Method "item" [t|Int -> IO (Maybe File)|])
     ]
 )

$(jsFieldFunctions ''FileList [])

-- | Every file, in order.
items :: FileList -> IO [File]
items files = do
  count <- files.length
  catMaybes <$> traverse files.item [0 .. count - 1]

$(inlineJS "js_files_now" "Array.from($1)" [t|FileList -> JSVal|])

$(inlineJS "js_files_count" "$1.length" [t|JSVal -> Int|])

$(inlineJS "js_files_at" "$1[$2]" [t|JSVal -> Int -> File|])

-- | The files as they are at the moment the answer is forced, read purely:
-- for a handler that takes a drop's or a paste's files in the event's
-- listener ('Halogen.HTML.Events.handler'' decides there), since the
-- browser empties the event's list once it has dispatched the event, while
-- the files taken from it stay readable. Force it in the listener
-- (@Just $! …@, or a strict field): forcing the list copies it. Natively
-- the files of the list's JSON, as an event made from JSON has them.
now :: FileList -> [File]
now files
  | inBrowser =
      let taken = js_files_now files
          count = js_files_count taken
       in map (js_files_at taken) [0 .. count - 1]
  | otherwise = nativeFiles files
