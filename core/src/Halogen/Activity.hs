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
-- around a request) is 'waiting', not 'running'; and a timer behind an
-- emitter, an emitter that has not fired yet, or an event the browser has
-- not dispatched yet is neither, since nothing of it is on a loop until it
-- arrives. A tree that has been disposed of is not in 'activity', so what
-- its finalizers still do once they wait (a @liftIO@ in a @Finalize@, and
-- what they fork) is not either: its socket's reading counts that (below).
--
-- = Waiting for a reading
--
-- 'waitActivity' returns once the reading satisfies a predicate. It is woken
-- by every change of what the reading is made of (a loop taken or given up,
-- work queued, a fiber starting or ending a wait, a tree mounted or disposed
-- of), never by a clock, and has no deadline of its own: wrap it in
-- 'System.Timeout.timeout'. Which predicate:
--
-- * after an action was delivered from outside the program (an event, a
--   query, an emitter's notification), 'idle' ('waitIdle'): the program has
--   done all it can do at once, and its renders are done. A fiber may be
--   waiting in a 'liftIO' meanwhile, and one whose wait has just ended may
--   not have been queued yet: 'idle' says nothing of what comes after a wait;
--
-- * once everything the program waits for has come back, 'quiet'
--   ('waitQuiet');
--
-- * when the test itself holds some of what the program waits for (a
--   backend whose answers the test lets go one at a time), "the program has
--   done all it can while the test still holds @n@ answers":
--   @'waitActivity' (\a -> a.running == 0 && a.waiting == n)@. After the
--   test lets one of @n + 1@ go, the fiber it held is counted as waiting
--   until its next step is queued on the loop, so this returns only after
--   that step has run and the program is idle again (where 'waitIdle' could
--   return before it was queued).
--
-- One tree's reading, and a wait on it, are its socket's:
-- 'Halogen.IO.Driver.HalogenSocket''s @activity@ and @waitActivity@, so that
-- a test can wait for its tree while others run beside it in the process.
-- Once the tree is disposed of, that reading is no tree, and what of it is
-- still at work (its finalizers that wait, and their forks): a wait for
-- 'quiet' on it returns once all of that is over, and a wait whose predicate
-- the last reading of a disposed tree does not satisfy throws
-- 'ActivityEnded' rather than wait for ever.
--
-- A wait sleeps in 'STM', so in a browser (either backend) it lets the page
-- run while it waits, as "Halogen.JSBits.Promise"'s waits do; it must not be
-- called from a component's own synchronous code, which holds the loop the
-- wait is waiting for. With nobody waiting, a change costs one read of a
-- reference more.
--
-- = Pauses
--
-- A timer written as a sleeping fiber is counted as the program writes it.
-- 'Halogen.Query.HalogenM.delayCounted' (@Hooks.delayCounted@ in hooks) is
-- a pause counted as waiting, as @liftIO (threadDelay n)@ is: the fiber is
-- 'waiting' until it wakes, so 'quiet' waits for it.
-- 'Halogen.Query.HalogenM.delay' (@Hooks.delay@) is a pause that is not
-- counted: while it sleeps the fiber is neither running nor waiting, and
-- once it wakes it is like any other again (its next piece of work is
-- queued, so the tree is 'running'). Only the application knows which of
-- its pauses a test should wait for, so it says so by how it writes them:
--
-- * a pause followed by work a test waits for, such as a debounce before a
--   request or an autosave two seconds after the last key, is
--   'Halogen.Query.HalogenM.delayCounted': 'quiet' then means the request
--   was answered;
--
-- * a pause that only takes something off the screen or ticks, such as a
--   \"Saved\" notice hidden after three seconds or a clock, is 'delay':
--   'quiet' does not wait for the notice to go (so a test can see it), and
--   a @forever@ ticker leaves the program quiet between its ticks.
--
-- A pause is still its fiber's: a fork that is killed, or a component that
-- goes away, ends it, and nothing after it runs. Both pauses sleep on a
-- test's clock in a tree mounted under one, and a test can hold the
-- program's other waits: "Halogen.Activity.Control".
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
-- per event, with no wait under way, a loop reads one reference more (to
-- find that nobody waits).
module Halogen.Activity
  ( Activity (..)
  , activity
  , idle
  , quiet

    -- * Waiting
  , waitActivity
  , waitIdle
  , waitQuiet
  , ActivityEnded (..)
  , waitsUnderWay

    -- * A reading of one's own
  , waitReading
  )
where

import HPrelude
import Halogen.Activity.Internal

-- | Return once 'activity' satisfies the predicate: at once if it does now,
-- otherwise after the change that makes it, woken by every change of the
-- trees' loops and of the registry of trees, never by a clock. No deadline:
-- a predicate that never holds waits for ever (wrap it in
-- 'System.Timeout.timeout'). See "Waiting for a reading" above for which
-- predicate to wait for.
waitActivity :: (Activity -> Bool) -> IO ()
waitActivity = waitReading (pure ()) ((,False) <$> activity)

-- | @'waitActivity' 'idle'@: no tree is running work or has work queued.
-- After an action has been delivered, the program has done what it does at
-- once, renders included. A fiber may still wait in a 'liftIO', or have
-- just come back from one without its next step queued yet; to wait for
-- that step while the test still holds other answers, wait for a count of
-- 'waiting' instead ("Waiting for a reading", above).
waitIdle :: IO ()
waitIdle = waitActivity idle

-- | @'waitActivity' 'quiet'@: idle, and what every fiber waited for has come
-- back. It does not return while a fiber waits for something the test
-- holds.
waitQuiet :: IO ()
waitQuiet = waitActivity quiet
