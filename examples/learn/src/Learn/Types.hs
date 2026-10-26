-- | The types of the reader's top-level bindings, as the compiler reports
-- them: for the editor's tooltips and the list under it.
--
-- Every run of the playground that typechecks the program, a compile or a
-- check, writes a line to stdout for each top-level binding of @Lesson@
-- before what the program itself writes there (@learn/Playground.hs@), its
-- fields separated by tabs:
--
-- > learn-type	12	1	render	Int -> H.ComponentHTML Action () m
module Learn.Types
  ( Binding (..)
  , splitTypes
  , displayName
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text qualified as T
import Protolude

data Binding = Binding
  { line :: Int
  , column :: Int
  -- ^ Where the name is bound, from 1 as GHC counts.
  , name :: Text
  , type_ :: Text
  -- ^ As GHCi's @:type@ prints it, on one line.
  }
  deriving stock (Eq, Show)

-- | The editor's tooltips want this (see @js\/editor.js@).
instance ToJSON Binding where
  toJSON b = object ["line" .= b.line, "column" .= b.column, "name" .= b.name, "type" .= b.type_]

-- | The bindings a run reported, and the rest of its stdout, the program's
-- own output, as it was. A line that starts as a @learn-type@ line but is
-- not one is the program's.
splitTypes :: Text -> ([Binding], Text)
splitTypes out =
  let (bindings, rest) = partitionEithers [maybe (Right l) Left (binding l) | l <- T.splitOn "\n" out]
   in (bindings, T.intercalate "\n" rest)
  where
    binding l = case T.splitOn "\t" l of
      ["learn-type", row, col, name, type_]
        | Just line <- number row
        , Just column <- number col
        , not (T.null name) ->
            Just Binding {line, column, name, type_}
      _ -> Nothing
    number t = readMaybe (toS t :: [Char])

-- | The name as the module would use it on its own: an operator in
-- parentheses.
displayName :: Binding -> Text
displayName b = case T.uncons b.name of
  Just (c, _) | not (isAlpha c || c == '_') -> "(" <> b.name <> ")"
  _ -> b.name
