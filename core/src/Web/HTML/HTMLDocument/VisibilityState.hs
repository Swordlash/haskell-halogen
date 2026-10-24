-- | Whether the page can be seen: the document's @visibilityState@.
--
-- The standard once had a third state, @prerender@, which browsers have since
-- dropped; a page that is not hidden is visible.
module Web.HTML.HTMLDocument.VisibilityState where

import HPrelude

data VisibilityState
  = -- | At least part of the page is on a screen: its tab is the one shown
    -- in a window that is not minimised.
    Visible
  | -- | Nothing of the page is on any screen: its tab is in the background,
    -- its window is minimised, or the screen is locked.
    Hidden
  deriving (Eq, Ord, Show)

print :: VisibilityState -> Text
print = \case
  Visible -> "visible"
  Hidden -> "hidden"

parse :: Text -> Maybe VisibilityState
parse = \case
  "visible" -> Just Visible
  "hidden" -> Just Hidden
  _ -> Nothing
