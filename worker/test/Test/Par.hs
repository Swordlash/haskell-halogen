{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StaticPointers #-}

-- | "Halogen.Worker.Par": the planner that cuts a round's calls into
-- batches, which is pure and tested as it is, and 'Par.runPar' end to end
-- where no worker can be made (natively and under Node), where the batches
-- run in the program itself through the same codecs. In a browser the same
-- batches go to workers, which toolchain/test-worker.mjs checks.
--
-- The jobs are exported, as "Halogen.Worker"'s documentation advises for
-- every binding that holds a static.
module Test.Par
  ( spec
  , triple
  , failsOnFive
  )
where

import Control.Exception (throwIO)
import Data.List (sort)
import Data.Text qualified as Text
import GHC.StaticPtr (StaticPtr)
import Halogen.Worker (Job, WorkerError (..))
import Halogen.Worker qualified as Worker
import Halogen.Worker.Internal (Batch (..), plan)
import Halogen.Worker.Par (runPar, task)
import Test.Hspec
import Test.Run (broken, double)

triple :: StaticPtr (Job Int Int)
triple = static (Worker.job (\n -> pure (n * 3)))

failsOnFive :: StaticPtr (Job Int Int)
failsOnFive = static (Worker.job (\n -> if n == 5 then throwIO (userError "five") else pure n))

failsWith :: String -> Selector WorkerError
failsWith fragment e = "Test." `Text.isPrefixOf` e.job && Text.pack fragment `Text.isInfixOf` e.message

spec :: Spec
spec = describe "Halogen.Worker.Par" $ do
  describe "plan" $ do
    it "groups the calls by job, in the order the jobs first come, each group's calls in order" $
      plan 2 "abaab"
        `shouldBe` [ Batch {key = 'a', offset = 0, positions = [0]}
                   , Batch {key = 'a', offset = 1, positions = [2]}
                   , Batch {key = 'a', offset = 2, positions = [3]}
                   , Batch {key = 'b', offset = 0, positions = [1]}
                   , Batch {key = 'b', offset = 1, positions = [4]}
                   ]

    it "cuts a group into four batches a worker, the larger first, their offsets the group's positions" $
      plan 2 (replicate 20 'x')
        `shouldBe` [ Batch {key = 'x', offset, positions = [offset .. offset + n - 1]}
                   | (offset, n) <- zip [0, 3, 6, 9, 12, 14, 16, 18] [3, 3, 3, 3, 2, 2, 2, 2]
                   ]

    it "counts a pool of no workers as one" $
      map (.positions) (plan 0 (replicate 5 'x')) `shouldBe` [[0, 1], [2], [3], [4]]

    it "places every call once, and makes nothing of no calls" $ do
      let keys = take 101 (cycle "abcab")
      sort (concatMap (.positions) (plan 3 keys)) `shouldBe` [0 .. 100]
      plan 3 ([] :: [Char]) `shouldBe` []

  describe "runPar, where no worker can be made" $ do
    it "runs independent tasks together and a dependent one after them" $ do
      pool <- Worker.newPool 2
      result <- runPar pool $ do
        a <- task double 1
        b <- task double 2
        c <- task double (a + b)
        pure (a, b, c)
      result `shouldBe` (2, 4, 12)

    it "maps a job over a list, the results in the list's order" $ do
      pool <- Worker.newPool 2
      runPar pool (traverse (task double) [1 .. 100]) `shouldReturn` map (* 2) [1 .. 100]

    it "runs calls of two jobs in one round" $ do
      pool <- Worker.newPool 2
      runPar pool ((,) <$> traverse (task double) [1, 2, 3] <*> traverse (task triple) [1, 2, 3])
        `shouldReturn` ([2, 4, 6], [3, 6, 9])

    it "fails with a WorkerError naming the job and the index of the call that failed" $ do
      pool <- Worker.newPool 2
      runPar pool (traverse (task failsOnFive) [0 .. 9]) `shouldThrow` failsWith "its call at index 5: user error (five)"

    it "fails for a result that does not decode, naming the call" $ do
      pool <- Worker.newPool 1
      runPar pool (task broken ()) `shouldThrow` failsWith "its call at index 0: its result does not decode"
