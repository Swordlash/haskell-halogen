module Halogen.HTML.Layout.GridLayout where

import Clay qualified as C
import Data.Default
import HPrelude
import Halogen.HTML.Layout
import Halogen.HTML.Layout.GridBagLayout

-- grid layout is like grid-bag but all cells are the same size

newtype GridLayout w i = GridLayout (GridBagLayout w i)

defGridSettings :: LayoutSettings GridLayout
defGridSettings = def

instance Default (LayoutSettings GridLayout) where
  def =
    GridLayoutSettings
      { rows = 0
      , cols = 0
      , gap = Nothing
      , width = Nothing
      , height = Nothing
      }

instance Layout GridLayout where
  data LayoutConstraints _ = GridLayoutConstraints
    { gridx :: Int
    , gridy :: Int
    }

  data LayoutSettings _ = GridLayoutSettings
    { rows :: Int
    , cols :: Int
    , gap :: forall a. Maybe (C.Size a)
    , width :: forall a. Maybe (C.Size a)
    , height :: forall a. Maybe (C.Size a)
    }

  layout GridLayoutSettings {..} (GridLayout gbl) =
    layout GridBagLayoutSettings {..} gbl

  layoutEnd = GridLayout $ GridBagLayout []

  addComponent GridLayoutConstraints {..} h (GridLayout gbl) =
    GridLayout $ addComponent GridBagLayoutConstraints {gridwidth = 1, gridheight = 1, ..} h gbl
