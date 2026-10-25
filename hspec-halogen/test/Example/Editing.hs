-- | Keys and edits whose outcome is the browser's to decide, which the
-- harness's in-page mode has to reproduce as a browser would: a button
-- whose keydown handler prevents the default, so that Space and Enter do
-- not press it (only a click does); an element the user edits directly
-- (@contenteditable@), which the component renders empty and never patches
-- inside, counting its input events; and a number field, whose value is
-- shown as typed, where a value such as @-42@ or @1.5@ passes through
-- prefixes (@-@, @1.@) that are not numbers yet.
module Example.Editing (component) where

import DOM.HTML.Indexed (InputType (..))
import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Protolude hiding (State)

data State = State {clicks :: Int, edits :: Int, number :: Text}

data Action = Pressed | Clicked | Edited | Typed Text

component :: forall q i o m. (Monad m) => H.Component q i o m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = const (pure (State 0 0 ""))
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction}
      }
  where
    render :: State -> H.ComponentHTML Action Empty m
    render model =
      HH.div_
        [ HH.button
            [ HP.class_ (HH.ClassName "guarded")
            , HE.preventingDefault (HE.onKeyDown (const Pressed))
            , HE.onClick (const Clicked)
            ]
            [HH.text "guarded"]
        , HH.span [HP.class_ (HH.ClassName "clicks")] [HH.text (show model.clicks)]
        , HH.div [HP.class_ (HH.ClassName "note"), HP.attr (H.AttrName "contenteditable") "true", HE.onInput (const Edited)] []
        , HH.span [HP.class_ (HH.ClassName "edits")] [HH.text (show model.edits)]
        , HH.input [HP.class_ (HH.ClassName "number"), HP.type_ InputNumber, HE.onValueInput Typed]
        , HH.span [HP.class_ (HH.ClassName "typed")] [HH.text model.number]
        ]

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Pressed -> pass
      Clicked -> modify $ \s -> s {clicks = s.clicks + 1}
      Edited -> modify $ \s -> s {edits = s.edits + 1}
      Typed t -> modify $ \s -> s {number = T.strip t}
