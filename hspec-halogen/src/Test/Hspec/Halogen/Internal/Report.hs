-- | The totals of the retry report (@HSPEC_HALOGEN_RETRIES@): for every
-- place that waited, how many of its waits passed at their first look and
-- how many retried, and the rule for which waits count. The report's lines
-- themselves are written in "Test.Hspec.Halogen"; this module is plain
-- Haskell, so that the rule and the totals are tested natively.
module Test.Hspec.Halogen.Internal.Report
  ( Ended (..)
  , counted
  , startTotals
  , countWait
  , totals
  )
where

import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Prelude
import System.IO.Unsafe (unsafePerformIO)

-- | How a wait ended: an attempt passed, its own deadline passed, or the
-- deadline of a wait around it cut it short.
data Ended = Passed | TimedOut | Cut
  deriving stock (Eq, Show)

-- | Whether a wait counts in the totals, and if so whether as retried,
-- from how many of its own attempts finished and how it ended. A wait none
-- of whose own attempts finished does not count: what it waited for was a
-- wait inside it, which counts at its own place. Otherwise it retried
-- unless its first attempt passed.
counted :: Int -> Ended -> Maybe Bool
counted attempts ended
  | attempts <= 0 = Nothing
  | otherwise = Just (attempts > 1 || ended /= Passed)

-- | Every place that waited in this run of the suite, by the function called
-- and its call site, with how many of its waits passed at the first look
-- and how many retried.
places :: IORef (Map (Text, Text) (Int, Int))
places = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE places #-}

-- | Start a run's totals afresh: a suite run again in the same runtime
-- (@:main@ again in browser GHCi, after a run that finished or was
-- interrupted) counts its own waits only.
startTotals :: IO ()
startTotals = atomicWriteIORef places Map.empty

-- | Count a wait at its place, by 'counted'.
countWait :: (Text, Text) -> Int -> Ended -> IO ()
countWait place attempts ended = case counted attempts ended of
  Nothing -> pure ()
  Just retried ->
    atomicModifyIORef' places $ \m ->
      (Map.insertWith (\(a, b) (c, d) -> (a + c, b + d)) place (if retried then (0, 1) else (1, 0)) m, ())

-- | The totals so far, by place.
totals :: IO [((Text, Text), (Int, Int))]
totals = Map.toList <$> readIORef places
