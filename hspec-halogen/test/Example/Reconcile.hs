-- | A component whose buttons each change one thing about what it renders,
-- for watching how a render reconciles the page with the new HTML: what it
-- keeps, what it patches, and what it replaces.
module Example.Reconcile (component) where

import Data.Row (HasType, type (.==))
import Example.Counter qualified as Counter
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Protolude hiding (State)

data State = State
  { renders :: Int
  , items :: Int
  , label :: Text
  , disabled :: Bool
  , bold :: Bool
  , titled :: Bool
  }

data Action = Rerender | Grow | Relabel | ToggleDisabled | ToggleTag | ToggleTitle

type Slots = ("counter" .== H.Slot Counter.Query Int ())

component :: forall q i o m. (Monad m) => H.Component q i o m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = const (pure State {renders = 0, items = 1, label = "first", disabled = False, bold = False, titled = False})
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction}
      }
  where
    cls :: forall r a. (HasType "class" Text r) => Text -> HP.IProp r a
    cls = HP.class_ . HH.ClassName

    button name action = HH.button [cls name, HE.onClick (const action)] [HH.text name]

    render :: State -> H.ComponentHTML Action Slots m
    render model =
      HH.div_
        [ button "rerender" Rerender
        , button "grow" Grow
        , button "relabel" Relabel
        , button "toggle-disabled" ToggleDisabled
        , button "toggle-tag" ToggleTag
        , button "toggle-title" ToggleTitle
        , HH.span [cls "renders"] [HH.text (show model.renders)]
        , HH.ul [cls "items"] [HH.li_ [HH.text ("item " <> show n)] | n <- [1 .. model.items]]
        , HH.span [cls "label"] [HH.text model.label]
        , -- The component never renders a value for this one.
          HH.input [cls "free"]
        , HH.button [cls "target", HP.disabled model.disabled] [HH.text "target"]
        , (if model.bold then HH.b else HH.i) [cls "styled"] [HH.text "styled"]
        , HH.span ([cls "titled"] <> [HP.title "a title" | model.titled]) [HH.text "titled"]
        , HH.slot_ "counter" () Counter.component ()
        ]

    handleAction :: Action -> H.HalogenM State Action Slots o m ()
    handleAction = \case
      Rerender -> modify $ \s -> s {renders = s.renders + 1} :: State
      Grow -> modify $ \s -> s {items = s.items + 1} :: State
      Relabel -> modify $ \s -> s {label = "second"} :: State
      ToggleDisabled -> modify $ \s -> s {disabled = not s.disabled} :: State
      ToggleTag -> modify $ \s -> s {bold = not s.bold} :: State
      ToggleTitle -> modify $ \s -> s {titled = not s.titled} :: State
