-- | Whether the program's component trees are busy, read exactly, for a
-- test (or any tool) that has to know when a program has finished reacting
-- rather than guess with a clock.
--
-- A mounted tree runs on a loop of its own ("Halogen.IO.Driver.Runtime"),
-- and a component's program waits only in 'Halogen.Query.HalogenM.lift'
-- and 'liftIO', on a worker thread. 'activity' reads every live tree's loop
-- as it is at that moment: whether it is running work or has work queued,
-- and how many of its fibers wait for a worker. It keeps no counts of its
-- own beside the loops, so it cannot drift from them, whatever is
-- cancelled or fails.
--
-- = The contract
--
-- * 'trees' counts trees from the start of their first piece of work (the
--   first render and the synchronous part of every @Initialize@ in it)
--   until they are disposed of. A tree is counted as 'running' for the
--   whole of that first piece of work, so @trees >= 1@ with every tree
--   idle means the trees mounted so far have rendered and initialized.
--   A mount that fails is not counted, and disposing of a tree twice
--   leaves the count right.
--
-- * 'idle' means no tree is running work or has work queued: everything a
--   handler did synchronously, the renders it caused included, is done.
--
-- * 'quiet' means 'idle' and no fiber waiting on a worker either.
--
-- What 'idle' does not cover: a fiber waiting on the network (a 'liftIO'
-- around a request) is 'waiting', not 'running'; and a timer, an emitter
-- that has not fired yet, or an event the browser has not dispatched yet
-- is neither, since nothing of it is on a loop until it arrives. What a
-- finalizer forks after its tree is disposed of is not counted either.
--
-- = The probe
--
-- A page that wants to ask from outside the program (a test driving a
-- browser, such as @haskell-halogen-playwright@) sets
--
-- > globalThis.__halogenProbe = {}
--
-- before the program starts. When a tree is mounted in a browser (either
-- browser backend) and that object exists, core gives it, once per
-- program, a function:
--
-- > await __halogenProbe.activity()   // {trees, running, waiting}
--
-- It returns a promise because a Haskell callback returns nothing to
-- JavaScript ("Halogen.JSBits"' 'Halogen.JSBits.mkCallback'), and on the
-- JavaScript backend it runs only after the call has returned: the callback
-- is handed the promise's @resolve@ and calls it with what it read. It
-- reads, and does not wait for any loop, so it answers while a loop is
-- held by a thread that was preempted (WebAssembly hands the page back
-- after about 15 ms with threads still runnable): the answer then says
-- that tree is running. Several programs in one page each add theirs, and
-- the function sums them.
--
-- With no probe in the page, a mount costs one read of a global, and
-- nothing else; with none in the program at all (natively, in Node) it
-- costs nothing. Every mount and every dispose updates the registry once;
-- nothing happens per event.
module Halogen.Activity
  ( Activity (..)
  , activity
  , idle
  , quiet
  , waitIdle
  , waitQuiet
  )
where

import HPrelude
import Halogen.Activity.Internal

-- | Return once 'activity' is 'idle', asking a millisecond apart. For a
-- test in the same process as the trees (a server's): after an action has
-- been delivered, it returns once the tree has handled it.
waitIdle :: IO ()
waitIdle = pollUntil idle

-- | Return once 'activity' is 'quiet': idle, and what every fiber waited
-- for has come back.
waitQuiet :: IO ()
waitQuiet = pollUntil quiet

pollUntil :: (Activity -> Bool) -> IO ()
pollUntil done = do
  a <- activity
  unless (done a) $ threadDelay 1000 >> pollUntil done
