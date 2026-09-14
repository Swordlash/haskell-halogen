module Halogen.HTML.Layout.NoLayout where

import HPrelude
import Halogen.HTML qualified as HH
import Halogen.HTML.Layout

newtype NoLayout w i = NoLayout_ [HH.HTML w i]

instance Layout NoLayout where
  data LayoutConstraints _ = Next
  data LayoutSettings _ = NoLayout
  layout _ (NoLayout_ children) = HH.div [] $ reverse children

  layoutEnd = NoLayout_ []
  addComponent Next h = coerce $ (:) h
