-- | The store the tests share: a count and a name, two parts a component
-- can follow one of.
module Test.Fixture
  ( Store (..)
  , StoreAction (..)
  , initialStore
  , reducer
  , runIn
  )
where

import Halogen.Store (HalogenStore, StoreT (..))
import Protolude

data Store = Store {count :: Int, name :: Text}
  deriving stock (Eq, Show)

data StoreAction
  = Increment
  | Rename Text
  | -- | Changes nothing: an update that hands back the same store.
    Touch

initialStore :: Store
initialStore = Store {count = 0, name = "nobody"}

reducer :: Store -> StoreAction -> Store
reducer store = \case
  Increment -> Store {count = store.count + 1, name = store.name}
  Rename name -> Store {count = store.count, name}
  Touch -> store

-- | Run a 'StoreT' program against a store the test made itself, as
-- 'Halogen.Store.runStoreT' does every program of a tree.
runIn :: forall m a. HalogenStore StoreAction Store -> StoreT StoreAction Store m a -> m a
runIn store (StoreT program) = runReaderT program store
