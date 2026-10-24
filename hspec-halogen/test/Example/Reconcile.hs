-- | A component whose buttons each change one thing about what it renders,
-- for watching how a render reconciles the page with the new HTML: what it
-- keeps, what it patches, and what it replaces. And one whose element under
-- a ref is replaced by another under the same label, or kept under another
-- label, for watching what the ref holds afterwards.
module Example.Reconcile (component, replacing) where

import Control.Monad.UUID (MonadUUID)
import Data.Row (Empty, HasType, type (.==))
import Example.Counter qualified as Counter
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.VDom.DOM.Monad (MonadBrowserDOM, setAttribute)
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

data Replacing = Replacing
  { field :: H.RefLabel
  , spare :: H.RefLabel
  , generation :: Int
  , retagged :: Bool
  , lost :: Bool
  , stale :: Bool
  }

data ReplacingAction = Rekey | Retag | RelabelRef | Mark

-- | A keyed element under a ref, among keyed siblings. "rekey" gives it a
-- new key and "retag" a new tag, either of which replaces it with a new
-- element under the same label, and "relabel-ref" puts the same element under
-- the spare label (keeping the one it was under in reserve); "mark" finds
-- the element through the ref and writes an attribute on it, so a test can
-- see which element the ref holds, and says so on the page when the ref
-- holds none, or when the spare label still holds one.
replacing :: forall q i o m. (MonadUUID m, MonadBrowserDOM m) => H.Component q i o m
replacing =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> do
          field <- H.newRefLabel "field"
          spare <- H.newRefLabel "spare"
          pure Replacing {field, spare, generation = 0, retagged = False, lost = False, stale = False}
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction}
      }
  where
    cls :: forall r a. (HasType "class" Text r) => Text -> HP.IProp r a
    cls = HP.class_ . HH.ClassName

    button name action = HH.button [cls name, HE.onClick (const action)] [HH.text name]

    render :: Replacing -> H.ComponentHTML ReplacingAction Empty m
    render model =
      HH.div_
        [ button "rekey" Rekey
        , button "retag" Retag
        , button "relabel-ref" RelabelRef
        , button "mark" Mark
        , HH.span [cls "lost"] [HH.text (if model.lost then "lost" else "")]
        , HH.span [cls "stale"] [HH.text (if model.stale then "stale" else "")]
        , HH.withKeys
            HH.div
            [cls "fields"]
            [ ("before", HH.span_ [HH.text "before"])
            , ("field-" <> show model.generation, (if model.retagged then HH.b else HH.i) [cls "field", HP.ref model.field] [HH.text "field"])
            , ("after", HH.span_ [HH.text "after"])
            ]
        ]

    handleAction :: ReplacingAction -> H.HalogenM Replacing ReplacingAction Empty o m ()
    handleAction = \case
      Rekey -> modify $ \s -> s {generation = s.generation + 1} :: Replacing
      Retag -> modify $ \s -> s {retagged = not s.retagged} :: Replacing
      RelabelRef -> modify $ \s -> s {field = s.spare, spare = s.field} :: Replacing
      Mark -> do
        (H.getRef =<< gets (.field)) >>= \case
          Just element -> H.liftEffect (setAttribute Nothing (HH.AttrName "data-marked") "marked" element)
          Nothing -> modify $ \s -> s {lost = True} :: Replacing
        (H.getRef =<< gets (.spare)) >>= \case
          Just _ -> modify $ \s -> s {stale = True} :: Replacing
          Nothing -> pass
