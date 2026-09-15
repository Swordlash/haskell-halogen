{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Halogen.HTML.Layout.BorderLayout (BorderLayout) where

import Clay qualified as C
import Clay.Extra.Grid qualified as C
import Data.Default
import HPrelude
import Halogen.HTML qualified as HH
import Halogen.HTML.Layout
import Halogen.HTML.Properties qualified as HP

data BorderLayout w i = BorderLayout
  { north :: Last (HH.HTML w i)
  , south :: Last (HH.HTML w i)
  , east :: Last (HH.HTML w i)
  , west :: Last (HH.HTML w i)
  , center :: Last (HH.HTML w i)
  }

borderLayoutStyle :: LayoutSettings BorderLayout -> C.Css
borderLayoutStyle BorderLayoutSettings {..} = do
  C.display C.inlineGrid
  traverse_ C.gridGap gap
  traverse_ C.width width
  traverse_ C.height height

  C.gridTemplateAreas
    $ [ "north north north"
      , "west center east"
      , "south south south"
      ]

  C.gridTemplateColumns [C.auto, C.fr 1, C.auto]
  C.gridTemplateRows [C.auto, C.fr 1, C.auto]

instance Default (LayoutSettings BorderLayout) where
  def =
    BorderLayoutSettings
      { gap = Nothing
      , width = Nothing
      , height = Nothing
      }

instance Layout BorderLayout where
  data LayoutConstraints _ = North | South | East | West | Center

  data LayoutSettings _ = BorderLayoutSettings
    { gap :: forall a. Maybe (C.Size a)
    , width :: forall a. Maybe (C.Size a)
    , height :: forall a. Maybe (C.Size a)
    }

  layout settings BorderLayout {..} =
    HH.div [HP.style $ borderLayoutStyle settings]
      $ catMaybes
        [ HH.div [HP.style $ C.gridArea "north"] . pure <$> getLast north
        , HH.div [HP.style $ C.gridArea "south"] . pure <$> getLast south
        , HH.div [HP.style $ C.gridArea "east"] . pure <$> getLast east
        , HH.div [HP.style $ C.gridArea "west"] . pure <$> getLast west
        , HH.div [HP.style $ C.gridArea "center"] . pure <$> getLast center
        ]

  layoutEnd = BorderLayout mempty mempty mempty mempty mempty

  addComponent side h b = case side of
    North -> b {north = b.north <> pure h}
    South -> b {south = b.south <> pure h}
    East -> b {east = b.east <> pure h}
    West -> b {west = b.west <> pure h}
    Center -> b {center = b.center <> pure h}
