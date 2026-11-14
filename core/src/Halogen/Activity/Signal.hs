-- | What wakes a wait on "Halogen.Activity"'s readings: a count of the
-- changes of what a reading is made of, kept only while someone waits. Not
-- exposed: the loops ("Halogen.IO.Driver.Runtime") and the registry of trees
-- ("Halogen.Activity.Internal") call 'changed' after each such change, and
-- "Halogen.Activity"'s waits are its readers.
--
-- = Why no change is missed
--
-- A change is made with 'atomicModifyIORef'' (a loop's state, the registry)
-- and followed by 'changed', which reads 'waiters' and, when someone waits,
-- bumps 'version' in a transaction. A waiter ('awaitingChange') first counts
-- itself in 'waiters' with 'atomicModifyIORef'', then reads 'version', then
-- takes its reading, and sleeps (in 'STM', so no clock) until 'version'
-- differs from what it read. Both 'atomicModifyIORef'' and a committed
-- transaction are full barriers, so of a changer's write to its state and its
-- read of 'waiters', and a waiter's write to 'waiters' and its reading, at
-- least one of the two reads sees the other's write:
--
-- * the changer saw no waiter: its change was made before the waiter
--   counted itself, so the waiter's reading, taken after, includes it;
--
-- * the changer saw a waiter and bumped 'version': either before the waiter
--   read 'version', and then the change (made before the bump) is in the
--   reading taken after that, or after, and then the waiter wakes.
--
-- A waiter is woken by every change of every tree, and reads again: what it
-- waits for is decided by its reading alone, so a spurious wake costs a
-- reading, never a wrong answer. Nothing is kept per waiter, and a waiter
-- that is killed uncounts itself ('bracket_'), so once nobody waits a change
-- costs one read of 'waiters' again.
module Halogen.Activity.Signal
  ( changed
  , waitsUnderWay
  , Version
  , watching
  , currentVersion
  , changedSince
  )
where

import Control.Concurrent.STM (STM, TVar, atomically, check, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (bracket_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Prelude
import System.IO.Unsafe (unsafePerformIO)

-- | How many waits are under way in the process.
waiters :: IORef Int
waiters = unsafePerformIO (newIORef 0)
{-# NOINLINE waiters #-}

-- | Bumped by every change made while someone waits.
version :: TVar Version
version = unsafePerformIO (newTVarIO 0)
{-# NOINLINE version #-}

type Version = Word

-- | Called after a change of what a reading is made of. With nobody
-- waiting, one read of a reference.
changed :: IO ()
changed = do
  n <- readIORef waiters
  if n > 0 then atomically (readTVar version >>= writeTVar version . (+ 1)) else pure ()
{-# INLINE changed #-}

-- | How many waits are under way now.
waitsUnderWay :: IO Int
waitsUnderWay = readIORef waiters

-- | Run a wait counted as one, uncounted however it ends.
watching :: IO a -> IO a
watching = bracket_ (bump 1) (bump (-1))
  where
    bump n = atomicModifyIORef' waiters (\w -> (w + n, ()))

-- | The version now: read inside 'watching', before the reading it goes
-- with.
currentVersion :: IO Version
currentVersion = readTVarIO version

-- | Whether a change has been made since that version: retried until one
-- has, so a transaction that waits on it sleeps until then.
changedSince :: Version -> STM ()
changedSince v = readTVar version >>= check . (/= v)
