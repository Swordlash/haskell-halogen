-- | A todo list: type what needs doing and press Enter (or click Add), tick
-- items off, move them up, delete them, and see how many are left.
--
-- The items are keyed, so an item's element moves with it when it is
-- reordered. Each has a note field the component never reads, which keeps
-- what was typed into it only because its element is the one that moved.
module Example.Todo (component, Query (..)) where

import DOM.HTML.Indexed (ButtonType (..), InputType (..))
import Data.Row (Empty, HasType)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Protolude hiding (State)
import Web.Event.Event (Event, preventDefault)

data Item = Item
  { key :: Int
  , title :: Text
  , done :: Bool
  }

data State = State
  { items :: [Item]
  , draft :: Text
  , nextKey :: Int
  }

data Action
  = Draft Text
  | Add Event
  | Toggle Int Bool
  | MoveUp Int
  | Remove Int

-- | The titles, in order.
newtype Query a = GetTitles ([Text] -> a)

component :: forall i o m. (MonadIO m) => H.Component Query i o m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = const (pure State {items = [], draft = "", nextKey = 0})
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.handleQuery = handleQuery}
      }
  where
    cls :: forall r a. (HasType "class" Text r) => Text -> HP.IProp r a
    cls = HP.class_ . HH.ClassName

    render :: State -> H.ComponentHTML Action Empty m
    render model =
      HH.div_
        [ HH.form
            [HE.onSubmit Add]
            [ HH.input [cls "draft", HP.value model.draft, HP.placeholder "What needs doing?", HE.onValueInput Draft]
            , HH.button [cls "add", HP.type_ ButtonSubmit] [HH.text "Add"]
            ]
        , HH.withKeys HH.ul [cls "items"] (map row model.items)
        , HH.p [cls "left"] [HH.text (show (length (filter (not . (.done)) model.items)) <> " left")]
        ]

    row item =
      ( show item.key
      , HH.li
          [cls (if item.done then "item done" else "item")]
          [ HH.input [HP.type_ InputCheckbox, cls "toggle", HP.checked item.done, HE.onChecked (Toggle item.key)]
          , HH.span [cls "title"] [HH.text item.title]
          , HH.input [cls "note", HP.placeholder "Note"]
          , HH.button [cls "up", HE.onClick (const (MoveUp item.key))] [HH.text "Up"]
          , HH.button [cls "remove", HE.onClick (const (Remove item.key))] [HH.text "Delete"]
          ]
      )

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Draft text -> modify $ \s -> s {draft = text} :: State
      Add event -> do
        -- The form would otherwise be submitted, and the page navigate away;
        -- at once, before the browser decides (see 'H.liftEffect').
        H.liftEffect (preventDefault event)
        model <- get
        let title = T.strip model.draft
        unless (T.null title) $
          put model {items = model.items <> [Item model.nextKey title False], draft = "", nextKey = model.nextKey + 1}
      Toggle key done -> modify $ \s -> s {items = map (\i -> if i.key == key then i {done} else i) s.items} :: State
      MoveUp key -> modify $ \s -> s {items = moveUp key s.items} :: State
      Remove key -> modify $ \s -> s {items = filter ((/= key) . (.key)) s.items} :: State

    handleQuery :: Query a -> H.HalogenM State Action Empty o m (Maybe a)
    handleQuery (GetTitles reply) = Just . reply . map (.title) <$> gets (.items)

    moveUp key = \case
      a : b : rest | b.key == key -> b : a : rest
      a : rest -> a : moveUp key rest
      [] -> []
