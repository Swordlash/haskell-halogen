-- | Keeps one note in IndexedDB: it shows what is kept when it starts, and
-- keeps its own note when asked, so a second one mounted later finds what
-- the first kept, as a page reloaded finds what it saved.
module Example.Notebook (component, Input (..), notes) where

import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Protolude hiding (State)
import Web.Storage.IndexedDB (Database, MonadIndexedDB, StoreName)
import Web.Storage.IndexedDB qualified as IDB

data Input = Input
  { database :: Database
  , note :: Text
  }

data State = State
  { input :: Input
  , kept :: Maybe Text
  }

data Action = Initialize | Keep

-- | The store the note is kept in.
notes :: StoreName
notes = "notes"

component :: forall q o m. (MonadIndexedDB m) => H.Component q Input o m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input -> pure State {input, kept = Nothing}
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.initialize = Just Initialize}
      }
  where
    render :: State -> H.ComponentHTML Action Empty m
    render current =
      HH.div_
        [ HH.p [HP.class_ (HH.ClassName "kept")] [HH.text (fromMaybe "nothing yet" current.kept)]
        , HH.button [HP.class_ (HH.ClassName "keep"), HE.onClick (const Keep)] [HH.text "Keep"]
        ]

    -- The store is reached through 'lift', which waits on a worker thread
    -- while the rest of the tree goes on: IndexedDB answers later.
    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Initialize -> load
      Keep -> do
        input <- gets (.input)
        lift (IDB.setItem input.database notes "note" input.note)
        load

    load :: H.HalogenM State Action Empty o m ()
    load = do
      input <- gets (.input)
      found <- lift (IDB.getItem input.database notes "note")
      modify $ \current -> current {kept = either (const Nothing) Just =<< found}
