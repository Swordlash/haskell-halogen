-- | A parent that adds and removes child components.
--
-- A child asks to be removed by raising an output, which the parent turns
-- into its own; the parent answers queries by asking its children. Each child
-- finds its own element through a ref when it is initialised, as a component
-- that sets up a JavaScript widget does, and says so; its finaliser writes to
-- a journal the test can read.
module Example.Family (component, Query (..), Output (..)) where

import Data.Map qualified as Map
import Data.Row (Empty, type (.==))
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Control.Monad.UUID (MonadUUID)
import Data.IORef (IORef, modifyIORef)
import Protolude hiding (State)
import UnliftIO (MonadUnliftIO)

--------------------------------------------------------------------------------
-- The child
--------------------------------------------------------------------------------

newtype ChildQuery a = GetName (Text -> a)

data ChildOutput = RemoveMe

data ChildAction = Initialize | Finalize | ClickRemove

data ChildState = ChildState
  { name :: Text
  , journal :: IORef [Text]
  , ref :: H.RefLabel
  , found :: Bool
  }

child :: forall m. (MonadIO m, MonadUUID m) => H.Component ChildQuery (Text, IORef [Text]) ChildOutput m
child =
  H.mkComponent
    H.ComponentSpec
      { initialState = \(name, journal) -> do
          ref <- H.newRefLabel "child"
          pure ChildState {name, journal, ref, found = False}
      , render
      , eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = handleAction
              , H.handleQuery = handleQuery
              , H.initialize = Just Initialize
              , H.finalize = Just Finalize
              }
      }
  where
    render :: ChildState -> H.ComponentHTML ChildAction Empty m
    render model =
      HH.li
        [HP.ref model.ref, HP.class_ (HH.ClassName "child")]
        [ HH.span [HP.class_ (HH.ClassName "name")] [HH.text model.name]
        , HH.span [HP.class_ (HH.ClassName "found")] [HH.text (if model.found then "found its element" else "")]
        , HH.button [HP.class_ (HH.ClassName "remove"), HE.onClick (const ClickRemove)] [HH.text "Remove"]
        ]

    handleAction :: ChildAction -> H.HalogenM ChildState ChildAction Empty ChildOutput m ()
    handleAction = \case
      Initialize -> do
        element <- H.getHTMLElementRef =<< gets (.ref)
        modify $ \s -> s {found = isJust element}
      Finalize -> do
        model <- get
        liftIO $ modifyIORef model.journal (<> ["finalised " <> model.name])
      ClickRemove -> H.raise RemoveMe

    handleQuery :: ChildQuery a -> H.HalogenM ChildState ChildAction Empty ChildOutput m (Maybe a)
    handleQuery (GetName reply) = Just . reply <$> gets (.name)

--------------------------------------------------------------------------------
-- The parent
--------------------------------------------------------------------------------

type Slots = ("child" .== H.Slot ChildQuery ChildOutput Text)

-- | The children's names, in order of their slots.
newtype Query a = GetNames ([Text] -> a)

newtype Output = Removed Text
  deriving (Eq, Show)

data Action = AddChild | ChildSaid Text ChildOutput

data State = State
  { children :: [Text]
  , added :: Int
  , journal :: IORef [Text]
  }

-- | The journal is where the children's finalisers write.
component :: forall m. (MonadUnliftIO m, MonadUUID m) => H.Component Query (IORef [Text]) Output m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \journal -> pure State {children = [], added = 0, journal}
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.handleQuery = handleQuery}
      }
  where
    render :: State -> H.ComponentHTML Action Slots m
    render model =
      HH.div_
        [ HH.button [HP.class_ (HH.ClassName "add-child"), HE.onClick (const AddChild)] [HH.text "Add a child"]
        , HH.ul_ [HH.slot "child" name child (name, model.journal) (ChildSaid name) | name <- model.children]
        ]

    handleAction :: Action -> H.HalogenM State Action Slots Output m ()
    handleAction = \case
      AddChild -> modify $ \s -> s {children = s.children <> [fromMaybe (show s.added) (atMay names s.added)], added = s.added + 1} :: State
      ChildSaid name RemoveMe -> do
        modify $ \s -> s {children = filter (/= name) s.children} :: State
        H.raise (Removed name)

    handleQuery :: Query a -> H.HalogenM State Action Slots Output m (Maybe a)
    handleQuery (GetNames reply) = Just . reply . Map.elems <$> H.requestAll "child" GetName

    names = ["ada", "bo", "cy", "di", "ed", "flo"]
