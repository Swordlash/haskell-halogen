-- | A checkbox the component never reads, beside a button that re-renders
-- the component for a reason of its own.
--
-- The box keeps what the user ticked across that render: a render writes a
-- property only when its rendered value changes, and this one always renders
-- the box unticked.
module Example.Agreement (component) where

import DOM.HTML.Indexed (InputType (..))
import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Protolude

data Action = Rerender

component :: forall q i o m. (Monad m) => H.Component q i o m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = const (pure (0 :: Int))
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = \Rerender -> modify (+ 1)}
      }
  where
    render :: Int -> H.ComponentHTML Action Empty m
    render renders =
      HH.div_
        [ HH.label_
            [ HH.input [HP.type_ InputCheckbox, HP.class_ (HH.ClassName "agree"), HP.checked False]
            , HH.text "I agree"
            ]
        , HH.button [HP.class_ (HH.ClassName "rerender"), HE.onClick (const Rerender)] [HH.text "Re-render"]
        , HH.span [HP.class_ (HH.ClassName "renders")] [HH.text (show renders)]
        ]
