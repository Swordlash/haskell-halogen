module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Data.Foldable (toList)
import Data.IORef
import Data.List (sort)
import System.Environment (withArgs)
import Test.Hspec
import Test.Hspec.Core.Spec qualified as Spec
import Test.Hspec.Halogen.Internal.Shard
import Test.Hspec.Runner
import Prelude

labels :: Shard -> Spec -> IO [String]
labels = labelsWith WithoutWorkers

labelsWith :: Workers -> Shard -> Spec -> IO [String]
labelsWith workers shard spec = map Spec.itemRequirement . concatMap toList . snd <$> Spec.runSpecM (shardSpec workers shard spec)

-- Every shard of a run with two pages and two workers, in the runner's order.
withWorkers :: Spec -> IO [[String]]
withWorkers spec = traverse (\shard -> labelsWith WithWorkers shard spec) [Serial, Parallel 0 2, Parallel 1 2, Worker 0 2, Worker 1 2]

-- Run all partitions with the real hspec options, without printing the
-- expected failures of a focused example to the outer suite's report.
runShards :: Config -> Spec -> IO [Summary]
runShards = runShardsWith WithoutWorkers [Serial, Parallel 0 2, Parallel 1 2]

runShardsWith :: Workers -> [Shard] -> Config -> Spec -> IO [Summary]
runShardsWith workers shards config spec =
  withArgs [] $
    traverse
      (\shard -> hspecWithResult quiet (onePage (shardSpec workers shard spec)))
      shards
  where
    quiet = config {configFormat = Just (\_ -> pure (\_ -> pure ()))}

examples :: Spec
examples = do
  it "unmarked" (pure () :: IO ())
  parallel $ do
    it "first" (pure () :: IO ())
    it "second" (pure () :: IO ())
    sequential $ it "sequential" (pure () :: IO ())
    describe "nested" $ it "third" (pure () :: IO ())

-- The examples above, and some for workers: marked parallel or not, and
-- nested in a group the pages also have examples in.
mixed :: Spec
mixed = do
  examples
  inWorker $ it "pure" (pure () :: IO ())
  parallel $ inWorker $ do
    it "computed" (pure () :: IO ())
    it "forked" (pure () :: IO ())
  describe "nested" $ do
    it "on a page" (pure () :: IO ())
    inWorker $ it "in a worker" (pure () :: IO ())

main :: IO ()
main = hspec $ do
  it "keeps unmarked and sequential examples in the serial phase" $
    labels Serial examples `shouldReturn` ["unmarked", "sequential"]
  it "partitions parallel examples exactly once" $ do
    selected <- traverse (\i -> labels (Parallel i 3) examples) [0 .. 2]
    sort (concat selected) `shouldBe` ["first", "second", "third"]
    sort (map length selected) `shouldBe` [0, 1, 2]
  it "retains duplicate descriptions" $ do
    let duplicate = parallel $ replicateM_ 4 (it "same" (pure () :: IO ()))
    counts <- traverse (\i -> length <$> labels (Parallel i 2) duplicate) [0 .. 1]
    counts `shouldBe` [2, 2]
  it "resolves focus before partitioning" $ do
    let focused = examples >> parallel (fit "focused" (pure () :: IO ()))
    labels Serial focused `shouldReturn` []
    selected <- traverse (\i -> labels (Parallel i 2) focused) [0 .. 1]
    concat selected `shouldBe` ["focused"]
  it "accepts ordinary examples with --fail-on=focused" $ do
    results <- runShards defaultConfig {configFailOnFocused = True} examples
    sum (map summaryExamples results) `shouldBe` 5
    sum (map summaryFailures results) `shouldBe` 0
  it "skips ordinary examples with --focused-only" $ do
    results <- runShards defaultConfig {configFocusedOnly = True} examples
    sum (map summaryExamples results) `shouldBe` 0
    sum (map summaryFailures results) `shouldBe` 0
  it "still rejects a focused example with --fail-on=focused" $ do
    let focused = examples >> parallel (fit "focused" (pure () :: IO ()))
    results <- runShards defaultConfig {configFailOnFocused = True} focused
    sum (map summaryExamples results) `shouldBe` 1
    sum (map summaryFailures results) `shouldBe` 1
  it "keeps an actual focused example with --focused-only" $ do
    let focused = examples >> parallel (fit "focused" (pure () :: IO ()))
    results <- runShards defaultConfig {configFocusedOnly = True} focused
    sum (map summaryExamples results) `shouldBe` 1
    sum (map summaryFailures results) `shouldBe` 0
  it "does not let a hook change globals during another example" $ do
    active <- newIORef False
    let enter = do
          readIORef active `shouldReturn` False
          writeIORef active True
        leave = writeIORef active False
        body = do
          threadDelay 10000
          readIORef active `shouldReturn` True
        suite = parallel $ before_ enter $ after_ leave $ do
          it "first" body
          it "second" body
    result <- hspecWithResult defaultConfig {configConcurrentJobs = Just 2} (onePage suite)
    summaryExamples result `shouldBe` 2
    summaryFailures result `shouldBe` 0
  it "preserves hooks and removes cleanup for empty shards" $ do
    calls <- newIORef ([] :: [String])
    let record x = modifyIORef' calls (<> [x])
        hooked =
          afterAll_ (record "cleanup")
            $ beforeAll_ (record "before")
            $ parallel
            $ it "example" (record "body")
    _ <- hspecWithResult defaultConfig (shardSpec WithoutWorkers (Parallel 1 2) hooked)
    readIORef calls `shouldReturn` []
    result <- hspecWithResult defaultConfig (shardSpec WithoutWorkers (Parallel 0 2) hooked)
    summaryExamples result `shouldBe` 1
    summaryFailures result `shouldBe` 0
    readIORef calls `shouldReturn` ["before", "body", "cleanup"]
  it "allows normal hspec match filters within a shard" $ do
    result <- hspecWithResult defaultConfig {configFilterPredicate = Just (\(_, label) -> label == "first")} (shardSpec WithoutWorkers (Parallel 1 2) examples)
    summaryExamples result `shouldBe` 1
    summaryFailures result `shouldBe` 0
  it "gives marked examples only to workers, and each to exactly one" $ do
    [serial, page0, page1, worker0, worker1] <- withWorkers mixed
    sort (serial <> page0 <> page1) `shouldBe` sort ["unmarked", "sequential", "first", "second", "third", "on a page"]
    sort (worker0 <> worker1) `shouldBe` sort ["pure", "computed", "forked", "in a worker"]
  it "divides marked examples between workers by their number in the whole spec" $ do
    -- Numbered from zero over the whole spec: pure is 5, computed 6, forked
    -- 7, on a page 8 and in a worker 9.
    labelsWith WithWorkers (Worker 0 2) mixed `shouldReturn` ["computed"]
    labelsWith WithWorkers (Worker 1 2) mixed `shouldReturn` ["pure", "forked", "in a worker"]
    labelsWith WithWorkers (Worker 0 1) mixed `shouldReturn` ["pure", "computed", "forked", "in a worker"]
  it "leaves marked examples on the pages when there are no workers" $ do
    labels Serial mixed `shouldReturn` ["unmarked", "sequential", "pure", "on a page", "in a worker"]
    selected <- traverse (\i -> labels (Parallel i 2) mixed) [0 .. 1]
    sort (concat selected) `shouldBe` sort ["first", "second", "third", "computed", "forked"]
  it "resolves focus across pages and workers" $ do
    let focusedInWorker = mixed >> inWorker (fit "focused" (pure () :: IO ()))
    shards <- withWorkers focusedInWorker
    concat shards `shouldBe` ["focused"]
    let focusedOnPage = mixed >> fit "focused" (pure () :: IO ())
    [serial, _, _, worker0, worker1] <- withWorkers focusedOnPage
    serial `shouldBe` ["focused"]
    worker0 <> worker1 `shouldBe` []
  it "runs every example once with hspec's runner across pages and workers" $ do
    results <- runShardsWith WithWorkers [Serial, Parallel 0 2, Parallel 1 2, Worker 0 2, Worker 1 2] defaultConfig {configFailOnFocused = True} mixed
    map summaryExamples results `shouldBe` [3, 2, 1, 1, 3]
    sum (map summaryFailures results) `shouldBe` 0
