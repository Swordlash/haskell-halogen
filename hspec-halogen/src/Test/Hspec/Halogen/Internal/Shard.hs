-- hspec-core gives an item's annotations only through its extension API,
-- Test.Hspec.Core.Extension.Item, which it marks as experimental with a
-- module deprecation; the module that holds them, Annotations, is hidden.
-- The package's bounds keep hspec-core to 2.11, whose API this is, and the
-- warning is silenced here only, a module that uses nothing else deprecated.
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Divide examples between browser contexts and Web Workers. Unmarked
-- examples run in a separate phase before any parallel context starts, and
-- examples marked 'inWorker' go to the workers when the runner starts some.
module Test.Hspec.Halogen.Internal.Shard
  ( Shard (..)
  , Workers (..)
  , shardSpec
  , onePage
  , inWorker
  , isInWorker
  )
where

import Data.Traversable (mapAccumL)
import Prelude
import Test.Hspec.Core.Extension.Item qualified as Item
import Test.Hspec.Core.Spec qualified as Spec

-- | Which part of a suite one copy of it runs: the page that runs the
-- unmarked examples first, page @i@ of @n@ for the parallel ones, or worker
-- @i@ of @n@ for those marked 'inWorker'.
data Shard = Serial | Parallel Int Int | Worker Int Int
  deriving (Eq, Show)

-- | Whether the runner starts workers beside the pages. Only then do the
-- pages leave the examples marked 'inWorker' out: without workers those
-- run on the pages like any other, so that a suite run with the defaults,
-- in browser GHCi or in a page with no runner loses none of its examples.
data Workers = WithoutWorkers | WithWorkers
  deriving (Eq, Show)

-- | The annotation 'inWorker' puts on an example (hspec-core keeps one value
-- of each type on an item, in 'Spec.itemAnnotations'). A type of our own, so
-- that nothing else's annotation can be mistaken for it.
data RunsInWorker = RunsInWorker

-- | Run these examples in a Web Worker when the runner starts some
-- (@HSPEC_HALOGEN_WORKERS@): a copy of the suite with a thread of its own
-- and no page, for examples that need none (pure checks, computations,
-- anything that does not 'Test.Hspec.Halogen.runPage'), so that they stop
-- holding the page's thread. Without workers it changes nothing, and the
-- examples run where they would have.
inWorker :: Spec.SpecWith a -> Spec.SpecWith a
inWorker = Spec.mapSpecItem_ (Item.setAnnotation RunsInWorker)

-- | Whether 'inWorker' marked the example.
isInWorker :: Spec.Item a -> Bool
isInWorker item = case Item.getAnnotation item of
  Just RunsInWorker -> True
  Nothing -> False

-- | Keep each example in exactly one shard, retaining its groups and hooks.
-- Focus eligibility is decided across the whole spec before partitioning.
-- Keep the original flags so hspec's focused-only and fail-on-focused
-- options still distinguish focused examples from ordinary ones.
--
-- The examples are numbered over the whole spec, so that pages and workers
-- agree on the numbers without telling each other anything; a worker takes
-- the marked examples whose number modulo the workers' count is its own,
-- whether or not they are also marked 'Spec.parallel', since a worker runs
-- beside the parallel pages in any case.
shardSpec :: Workers -> Shard -> Spec.Spec -> Spec.Spec
shardSpec workers shard = Spec.mapSpecForest partition
  where
    partition forest = Spec.pruneForest . map (fmap snd) . Spec.filterForest (selected focused) $ snd (mapAccumL numberTree 0 forest)
      where
        focused = any (any Spec.itemIsFocused) forest
    numberTree n tree = mapAccumL (\i item -> (i + 1, (i, item))) n tree
    selected focused (i, item) =
      (not focused || Spec.itemIsFocused item) && case shard of
        Serial -> onPages item && Spec.itemIsParallelizable item /= Just True
        Parallel index count -> onPages item && Spec.itemIsParallelizable item == Just True && i `mod` count == index
        Worker index count -> isInWorker item && i `mod` count == index
    onPages item = workers == WithoutWorkers || not (isInWorker item)

-- | A browser page has one input state. Disable hspec's thread parallelism
-- after choosing a shard, so hooks cannot change globals while another
-- example holds the page lock. Parallelism belongs between contexts. A
-- worker runs one example at a time too: hspec's threads would share its
-- one JavaScript thread anyway.
onePage :: Spec.Spec -> Spec.Spec
onePage = Spec.mapSpecItem_ (\item -> item {Spec.itemIsParallelizable = Just False})
