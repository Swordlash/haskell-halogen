module Halogen.HTML.Layout.BoxLayout where

import Clay qualified as C
import Data.Default
import HPrelude
import Halogen.HTML qualified as HH
import Halogen.HTML.Layout
import Halogen.HTML.Properties qualified as HP

newtype BoxLayout w i = BoxLayout [HH.HTML w i]

settingsToDir :: LayoutSettings BoxLayout -> C.FlexDirection
settingsToDir Horizontal = C.row
settingsToDir Vertical = C.column

instance Default (LayoutConstraints BoxLayout) where
  def = Next

instance Layout BoxLayout where
  data LayoutConstraints _ = Next
  data LayoutSettings _ = Horizontal | Vertical
  layout settings (BoxLayout children) =
    HH.div
      [HP.style $ C.display C.inlineFlex <> C.flexDirection (settingsToDir settings)]
      $ reverse children

  layoutEnd = BoxLayout []
  addComponent Next h = coerce (h :)
