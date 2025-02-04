module Halogen.HTML.Layout.GridBagLayout where

import Clay qualified as C
import Clay.Extra.Grid qualified as C
import Data.Default
import HPrelude
import Halogen.HTML qualified as HH
import Halogen.HTML.Layout
import Halogen.HTML.Properties qualified as HP

newtype GridBagLayout w i = GridBagLayout [(LayoutConstraints GridBagLayout, HH.HTML w i)]

gridBagLayoutStyle :: LayoutSettings GridBagLayout -> C.Css
gridBagLayoutStyle GridBagLayoutSettings {..} = do
  C.display C.inlineGrid
  C.gridTemplateColumns $ replicate cols $ C.fr 1
  C.gridTemplateRows $ replicate rows $ C.fr 1

  traverse_ C.gridGap gap
  traverse_ C.width width
  traverse_ C.height height

defGridBagSettings :: LayoutSettings GridBagLayout
defGridBagSettings = def

instance Default (LayoutSettings GridBagLayout) where
  def =
    GridBagLayoutSettings
      { rows = 1
      , cols = 1
      , gap = Nothing
      , width = Nothing
      , height = Nothing
      }

instance Layout GridBagLayout where
  data LayoutConstraints _ = GridBagLayoutConstraints
    { gridx :: Int
    , gridy :: Int
    , gridwidth :: Int
    , gridheight :: Int
    }

  data LayoutSettings GridBagLayout = GridBagLayoutSettings
    { rows :: Int
    , cols :: Int
    , gap :: forall a. Maybe (C.Size a)
    , width :: forall a. Maybe (C.Size a)
    , height :: forall a. Maybe (C.Size a)
    }

  layout settings (GridBagLayout hms) =
    HH.div
      [HP.style $ gridBagLayoutStyle settings]
      $ flip map hms
      $ \(GridBagLayoutConstraints {..}, html) ->
        HH.div
          [ HP.style $ do
              C.gridRow $ gridx C.// C.span_ gridwidth
              C.gridColumn $ gridy C.// C.span_ gridheight
          ]
          [html]

  layoutEnd = GridBagLayout []
  addComponent constraints html = coerce $ (:) (constraints, html)
