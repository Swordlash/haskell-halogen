{-# LANGUAGE TemplateHaskell #-}

-- | The registry of mounted trees that "Halogen.Activity" reads, and the
-- probe a page can ask. Not exposed: the driver registers and deregisters
-- its trees here, and "Halogen.Activity" is the public face.
module Halogen.Activity.Internal
  ( Activity (..)
  , activity
  , idle
  , quiet
  , register
  , deregister
  )
where

import Data.IntMap.Strict qualified as IntMap
import HPrelude
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
  -- 'Halogen.Query.HalogenM.lift' or 'liftIO' under way.
  }
  deriving stock (Eq, Show)

-- | No tree is running or has work queued. Fibers may still be waiting.
idle :: Activity -> Bool
idle a = a.running == 0

-- | 'idle', and no fiber waiting either.
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

-- | Count a tree, and give the page's probe its function if the page has
-- one and this program has not done so yet. The driver calls it inside the
-- tree's first piece of work, so that the tree is never counted before its
-- loop is busy with that work; the number deregisters it.
register :: Loop -> IO Int
register loop = do
  n <- atomicModifyIORef' trees $ \(next, loops) -> ((next + 1, IntMap.insert next loop loops), next)
  -- A page whose probe is not what core expects must not stop a mount.
  when inBrowser $ void (try offerProbe :: IO (Either SomeException ()))
  pure n

-- | Stop counting a tree. Doing it twice does nothing.
deregister :: Int -> IO ()
deregister n = atomicModifyIORef' trees (\(next, loops) -> ((next, IntMap.delete n loops), ()))

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
