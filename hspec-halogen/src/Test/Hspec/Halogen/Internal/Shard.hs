-- | Divide parallel examples between browser contexts. Unmarked examples
-- run in a separate phase before any parallel context starts.
module Test.Hspec.Halogen.Internal.Shard (Shard (..), shardSpec, onePage) where

import Data.Traversable (mapAccumL)
import Prelude
import Test.Hspec.Core.Spec qualified as Spec

data Shard = Serial | Parallel Int Int
  deriving (Eq, Show)

-- | Keep each example in exactly one shard, retaining its groups and hooks.
-- Focus is resolved before partitioning: a focused example on another page
-- must not make this page run examples that were not focused.
shardSpec :: Shard -> Spec.Spec -> Spec.Spec
shardSpec shard = Spec.mapSpecForest partition . Spec.focus
  where
    partition forest = Spec.pruneForest . map (fmap snd) . Spec.filterForest selected $ snd (mapAccumL numberTree 0 forest)
    numberTree n tree = mapAccumL (\i item -> (i + 1, (i, item))) n tree
    selected (i, item) =
      Spec.itemIsFocused item && case shard of
        Serial -> Spec.itemIsParallelizable item /= Just True
        Parallel index count -> Spec.itemIsParallelizable item == Just True && i `mod` count == index

-- | A browser page has one input state. Disable hspec's thread parallelism
-- after choosing a shard, so hooks cannot change globals while another
-- example holds the page lock. Parallelism belongs between contexts.
onePage :: Spec.Spec -> Spec.Spec
onePage = Spec.mapSpecItem_ (\item -> item {Spec.itemIsParallelizable = Just False})
