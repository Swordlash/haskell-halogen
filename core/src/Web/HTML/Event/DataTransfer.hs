{-# LANGUAGE TemplateHaskell #-}

-- | What a drag or a paste carries: a @DataTransfer@. Meant to be imported
-- qualified (@DataTransfer.files@).
--
-- Natively a transfer is what an event made from JSON says of it (a
-- test's, or one a remote client sent: "Halogen.Remote.Protocol" spells
-- it), @{"types": [...], "files": [{"name", "type", "size"}], "data":
-- {"text/plain": "..."}}@: its 'types', its files' names, types and sizes,
-- and 'getData' for the formats it has. A file's bytes do not cross: see
-- "Web.File.File".
module Web.HTML.Event.DataTransfer
  ( DataTransfer (..)
  , files
  , types
  , getData
  )
where

import Data.Foreign (Foreign, foreignToString, readElements, readProp)
import HPrelude
import Halogen.JSBits (Inert, JSText, JSVal, fromJSText, inBrowser, jsNull)
import Halogen.JSBits.Fields (Member (..), inlineJS, jsFieldFunctions, jsFields)
import Web.File.FileList (FileList (..))

-- | A @DataTransfer@: 'Web.HTML.Event.DragEvent.dataTransfer',
-- 'Web.Clipboard.ClipboardEvent.clipboardData'.
newtype DataTransfer = DataTransfer (Foreign DataTransfer)
  deriving newtype (Inert)

-- Natively, the transfer's JSON's @files@, and an empty list without one.
nativeFiles :: DataTransfer -> FileList
nativeFiles (DataTransfer value) = FileList (fromMaybe jsNull (readProp "files" Just value))

-- Natively, the text the transfer's JSON has for a format under @data@, and
-- empty for one it does not have. @"text"@ and @"url"@ are the browsers'
-- old names for @"text/plain"@ and @"text/uri-list"@.
nativeGetData :: DataTransfer -> Text -> IO Text
nativeGetData (DataTransfer value) format =
  pure (fromMaybe "" (readProp "data" (readProp (named format) (Just . foreignToString)) value))
  where
    named = \case
      "text" -> "text/plain"
      "url" -> "text/uri-list"
      other -> other

$( jsFields
     ''DataTransfer
     [ -- The files it carries, which is empty for a drag of text. The
       -- browser hands the same list each time it is asked; its files can
       -- be read only while the event that carries them is dispatched, so
       -- read them in the handler. Chromium lists a drag's files only at
       -- its drop: during @dragover@ the list is empty and 'types' says
       -- @"Files"@.
       Natively [|nativeFiles|] (Fixed "files" ''FileList)
     , -- The data it carries in a format (@"text/plain"@, @"text/uri-list"@,
       -- @"text/html"@), or empty when it has none. A browser gives it only
       -- at a @drop@ and a @paste@ (and to the page that started a drag),
       -- and only while the event is dispatched: read it in the handler.
       Natively [|nativeGetData|] (Method "getData" [t|Text -> IO Text|])
     ]
 )

$(jsFieldFunctions ''DataTransfer [])

$(inlineJS "js_types" "Array.from($1.types ?? [], String)" [t|DataTransfer -> JSVal|])

$(inlineJS "js_types_count" "$1.length" [t|JSVal -> Int|])

$(inlineJS "js_type_at" "$1[$2]" [t|JSVal -> Int -> JSText|])

-- | The types of data it carries, in the order the browser lists them:
-- @"Files"@ when it carries files, and the formats of the rest
-- (@"text/plain"@, @"text/uri-list"@). A browser lists them during every
-- event of a drag, unlike the data itself. Read purely, as the transfer is
-- when the answer is forced: in the handler. Natively the JSON's @types@.
types :: DataTransfer -> [Text]
types transfer@(DataTransfer value)
  | inBrowser =
      let listed = js_types transfer
       in map (fromJSText . js_type_at listed) [0 .. js_types_count listed - 1]
  | otherwise = maybe [] (map foreignToString . readElements) (readProp "types" Just (value :: JSVal))
