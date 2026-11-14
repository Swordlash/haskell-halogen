{-# LANGUAGE TemplateHaskell #-}

-- | The registry of mounted trees that "Halogen.Activity" reads, and the
-- probe a page can ask. Not exposed: the driver registers and deregisters
-- its trees here, and "Halogen.Activity" is the public face.
module Halogen.Activity.Internal
  ( Activity (..)
  , activity
  , treeReading
  , idle
  , quiet
  , register
  , deregister
  , waitReading
  , waitsUnderWay
  , ActivityEnded (..)
  )
where

import Data.IntMap.Strict qualified as IntMap
import HPrelude
import Halogen.Activity.Signal (changed, changedSince, currentVersion, waitsUnderWay, watching)
import Halogen.IO.Driver.Runtime (Loop, loopActivity)
import Halogen.JSBits (Callback, JSVal, Safety (..), browserJS, inBrowser, mkCallback)
import System.IO.Unsafe (unsafePerformIO)

-- The probe's side in the page: core/jsbits/activity.js.
$( browserJS
     ["jsbits/activity.js"]
     [ ("js_activity_probe_wanted", "js_activity_probe_wanted", Safe, [t|IO Bool|])
     , ("js_activity_probe_install", "js_activity_probe_install", Safe, [t|Callback -> IO ()|])
     , ("js_activity_answer", "js_activity_answer", Safe, [t|JSVal -> Int -> Int -> Int -> IO ()|])
     ]
 )

-- | What the program's mounted trees are doing, at one moment.
data Activity = Activity
  { trees :: Int
  -- ^ Trees mounted (by 'Halogen.IO.Driver.runUI', and so by
  -- @runUI@ and @hydrateUI@ of any backend) and not disposed of.
  , running :: Int
  -- ^ Of those, the ones whose loop is running a piece of work or has work
  -- queued.
  , waiting :: Int
  -- ^ Fibers of those trees waiting for an action on a worker thread: a
  -- 'Halogen.Query.HalogenM.lift' or 'liftIO' under way. Not a pause
  -- ('Halogen.Query.HalogenM.delay'), which is not counted.
  }
  deriving stock (Eq, Show)

-- | No tree is running or has work queued. Fibers may still be waiting, and
-- one whose wait has just ended may not have its next step queued yet: for
-- "done all it can while @n@ answers are held", ask for
-- @running == 0 && waiting == n@ instead.
idle :: Activity -> Bool
idle a = a.running == 0

-- | 'idle', and no fiber waiting either: everything the program waited for
-- has come back and been handled.
quiet :: Activity -> Bool
quiet a = idle a && a.waiting == 0

-- | The live trees' loops, under the numbers 'register' gave them. Only the
-- loops are kept: what they are doing is read from them when asked, so the
-- registry changes once per mount and once per dispose, and never has a
-- count of its own that could miss a step of a loop.
trees :: IORef (Int, IntMap Loop)
trees = unsafePerformIO (newIORef (0, IntMap.empty))
{-# NOINLINE trees #-}

-- | Whether this program has given the page's probe its function.
installed :: IORef Bool
installed = unsafePerformIO (newIORef False)
{-# NOINLINE installed #-}

-- | Read every live tree's loop.
activity :: IO Activity
activity = do
  loops <- IntMap.elems . snd <$> readIORef trees
  readings <- traverse loopActivity loops
  pure
    Activity
      { trees = length loops
      , running = length (filter fst readings)
      , waiting = sum (map snd readings)
      }

-- | One tree's reading, as its socket gives it, by the number 'register'
-- gave it and its loop: one tree from its registration until it is
-- deregistered (disposed of), with what its loop is doing, under the same
-- contract as 'activity'. Once it is deregistered, no tree, and what of its
-- loop is still at work: the fibers of its finalizers, and the forks they
-- started, which go on after the dispose returns when they wait. With it,
-- whether the reading can change no more: the tree is gone and its loop has
-- nothing running, queued or waiting (but for a pause, which is counted
-- nowhere).
treeReading :: Int -> Loop -> IO (Activity, Bool)
treeReading n loop = do
  registered <- IntMap.member n . snd <$> readIORef trees
  (busy, waits) <- loopActivity loop
  let reading = Activity {trees = if registered then 1 else 0, running = if busy then 1 else 0, waiting = waits}
  pure (reading, not registered && not busy && waits == 0)

-- | Return once a reading satisfies the predicate, woken by every change of
-- what the trees' readings are made of (see "Halogen.Activity.Signal" for
-- why none is missed) and by every change of @extra@, a value read in 'STM'
-- of whatever else the caller folds into its reading (a remote session's
-- unsent operations). Read before the reading, it is the other half of the
-- same argument: a change of it made after it was read wakes the wait.
--
-- The reading says, besides, whether it can change no more (a disposed tree
-- with nothing left of it at work): a predicate it does not satisfy then
-- throws 'ActivityEnded' rather than wait for ever.
waitReading :: (Eq s) => STM s -> IO (Activity, Bool) -> (Activity -> Bool) -> IO ()
waitReading extra reading done = watching go
  where
    go = do
      v <- currentVersion
      before <- atomically extra
      (a, final) <- reading
      unless (done a) $ do
        when final $ throwIO (ActivityEnded a)
        atomically (changedSince v `orElse` (extra >>= check . (/= before)))
        go

-- | A wait on one tree's reading (a socket's, a remote session's) whose
-- tree has been disposed of and has nothing left at work, with the reading
-- it ended with, which the predicate did not accept: nothing would ever wake
-- the wait again.
newtype ActivityEnded = ActivityEnded Activity
  deriving stock (Show)

instance Exception ActivityEnded

-- | Count a tree, and give the page's probe its function if the page has
-- one and this program has not done so yet. The driver calls it inside the
-- tree's first piece of work, so that the tree is never counted before its
-- loop is busy with that work; the number deregisters it.
register :: Loop -> IO Int
register loop = do
  n <- atomicModifyIORef' trees $ \(next, loops) -> ((next + 1, IntMap.insert next loop loops), next)
  changed
  -- A page whose probe is not what core expects must not stop a mount.
  when inBrowser $ void (try offerProbe :: IO (Either SomeException ()))
  pure n

-- | Stop counting a tree. Doing it twice does nothing.
deregister :: Int -> IO ()
deregister n = atomicModifyIORef' trees (\(next, loops) -> ((next, IntMap.delete n loops), ())) >> changed

-- | One read of a global per mount until the page turns out to want an
-- answer; then a callback, made once, for as long as the program runs.
offerProbe :: IO ()
offerProbe = do
  done <- readIORef installed
  unless done $ do
    wanted <- js_activity_probe_wanted
    when wanted $ do
      -- Claimed first: two trees mounted at once install one callback.
      already <- atomicModifyIORef' installed (True,)
      unless already $ do
        callback <- mkCallback $ \resolve -> do
          a <- activity
          js_activity_answer resolve a.trees a.running a.waiting
        js_activity_probe_install callback
