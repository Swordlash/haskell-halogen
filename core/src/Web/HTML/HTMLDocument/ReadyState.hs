module Web.HTML.HTMLDocument.ReadyState where

import HPrelude

data ReadyState
  = Loading
  | Interactive
  | Complete
  deriving (Eq, Ord, Show)

print :: ReadyState -> Text
print = \case
  Loading -> "loading"
  Interactive -> "interactive"
  Complete -> "complete"

parse :: Text -> Maybe ReadyState
parse = \case
  "loading" -> Just Loading
  "interactive" -> Just Interactive
  "complete" -> Just Complete
  _ -> Nothing
