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
labels shard spec = map Spec.itemRequirement . concatMap toList . snd <$> Spec.runSpecM (shardSpec shard spec)

-- Run all partitions with the real hspec options, without printing the
-- expected failures of a focused example to the outer suite's report.
runShards :: Config -> Spec -> IO [Summary]
runShards config spec =
  withArgs [] $
    traverse
      (\shard -> hspecWithResult quiet (onePage (shardSpec shard spec)))
      [Serial, Parallel 0 2, Parallel 1 2]
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
    _ <- hspecWithResult defaultConfig (shardSpec (Parallel 1 2) hooked)
    readIORef calls `shouldReturn` []
    result <- hspecWithResult defaultConfig (shardSpec (Parallel 0 2) hooked)
    summaryExamples result `shouldBe` 1
    summaryFailures result `shouldBe` 0
    readIORef calls `shouldReturn` ["before", "body", "cleanup"]
  it "allows normal hspec match filters within a shard" $ do
    result <- hspecWithResult defaultConfig {configFilterPredicate = Just (\(_, label) -> label == "first")} (shardSpec (Parallel 1 2) examples)
    summaryExamples result `shouldBe` 1
    summaryFailures result `shouldBe` 0
