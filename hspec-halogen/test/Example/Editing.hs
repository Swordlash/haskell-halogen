-- | Keys and edits whose outcome is the browser's to decide, which the
-- harness's in-page mode has to reproduce as a browser would: a button
-- whose keydown handler prevents the default, so that Space and Enter do
-- not press it (only a click does); an element the user edits directly
-- (@contenteditable@), which the component renders empty and never patches
-- inside, counting its input events; and a number field, whose value is
-- shown as typed, where a value such as @-42@ or @1.5@ passes through
-- prefixes (@-@, @1.@) that are not numbers yet. And the fields a browser
-- edits less than it is asked to: a read-only one, one whose keypress
-- handler turns the character @x@ down, one whose @beforeinput@ handler
-- turns every edit down, and one with a @maxlength@ of 3. A field the text
-- typed into is shown from, for characters longer than one UTF-16 code unit
-- (an emoji), which Backspace must take out whole. And two forms Enter
-- submits from a text field, as HTML's implicit submission has it, by
-- activating the form's first submit button: one whose button is disabled,
-- so that Enter does nothing, and one whose button counts its clicks.
module Example.Editing (component) where

import DOM.HTML.Indexed (ButtonType (..), InputType (..))
import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Protolude hiding (State)
import Web.Event.Event (EventType (..))
import Web.UIEvent.KeyboardEvent qualified as KE

data State = State
  { clicks :: Int
  , edits :: Int
  , number :: Text
  , smiled :: Text
  , lockedSubmits :: Int
  , orderClicks :: Int
  , orderSubmits :: Int
  }

data Action = Pressed | Clicked | Edited | Typed Text | Smiled Text | SubmittedLocked | ClickedOrder | SubmittedOrder

component :: forall q i o m. (Monad m) => H.Component q i o m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = const (pure (State 0 0 "" "" 0 0 0))
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
        , HH.input [HP.class_ (HH.ClassName "readonly"), HP.readOnly True, HP.value "fixed"]
        , HH.input
            [ HP.class_ (HH.ClassName "no-x")
            , HE.preventingDefault (HE.handler' (EventType "keypress") (HE.keyHandler (\e -> if KE.key e == "x" then Just Pressed else Nothing)))
            ]
        , HH.input [HP.class_ (HH.ClassName "refuses"), HE.preventingDefault (HE.handler (EventType "beforeinput") (const Pressed))]
        , HH.input [HP.class_ (HH.ClassName "short"), HP.maxLength 3]
        , HH.input [HP.class_ (HH.ClassName "emoji"), HE.onValueInput Smiled]
        , HH.span [HP.class_ (HH.ClassName "smiled")] [HH.text model.smiled]
        , HH.form
            [HP.class_ (HH.ClassName "locked"), HE.preventingDefault (HE.onSubmit (const SubmittedLocked))]
            [ HH.input [HP.class_ (HH.ClassName "locked-field")]
            , HH.button [HP.type_ ButtonSubmit, HP.disabled True] [HH.text "locked"]
            ]
        , HH.span [HP.class_ (HH.ClassName "locked-submits")] [HH.text (show model.lockedSubmits)]
        , HH.form
            [HP.class_ (HH.ClassName "order"), HE.preventingDefault (HE.onSubmit (const SubmittedOrder))]
            [ HH.input [HP.class_ (HH.ClassName "order-field")]
            , HH.input [HP.class_ (HH.ClassName "order-note")]
            , HH.button [HP.type_ ButtonSubmit, HE.onClick (const ClickedOrder)] [HH.text "order"]
            ]
        , HH.span [HP.class_ (HH.ClassName "order-clicks")] [HH.text (show model.orderClicks)]
        , HH.span [HP.class_ (HH.ClassName "order-submits")] [HH.text (show model.orderSubmits)]
        ]

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Pressed -> pass
      Clicked -> modify $ \s -> s {clicks = s.clicks + 1}
      Edited -> modify $ \s -> s {edits = s.edits + 1}
      Typed t -> modify $ \s -> s {number = T.strip t}
      Smiled t -> modify $ \s -> s {smiled = t}
      SubmittedLocked -> modify $ \s -> s {lockedSubmits = s.lockedSubmits + 1}
      ClickedOrder -> modify $ \s -> s {orderClicks = s.orderClicks + 1}
      SubmittedOrder -> modify $ \s -> s {orderSubmits = s.orderSubmits + 1}
