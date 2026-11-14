-- | How to control a program in a native test: hold its waits and let them
-- go in the order the test chooses, and move its pauses on with a clock the
-- test owns, so that a test of a component that loads, debounces or ticks
-- needs neither a backend of its own nor real time. (How to mount a
-- component natively and read and drive its DOM is in
-- "Halogen.VDom.DOM.Monad.Native"'s header; what the program is doing is
-- "Halogen.Activity".) It is plain Haskell, so it works on every backend,
-- a page included.
--
-- > withHeldWaits $ \waits -> do
-- >   socket <- mount
-- >   [readAsk] <- pending waits        -- the Initialize's request, held
-- >   click toggle
-- >   [_, readAsk2] <- pending waits    -- and the one the click made
-- >   release readAsk2                  -- answered out of order
-- >   release readAsk
-- >   socket.waitActivity quiet
--
-- > withTestClock $ \clock -> do
-- >   socket <- mount
-- >   typeText field "abc"              -- a debounce: delayCounted 0.3
-- >   advance clock 0.3                 -- it fires, at once
-- >   socket.waitActivity quiet         -- and its request comes back
--
-- = Which trees
--
-- A bracket controls the trees mounted inside it on the thread that opened
-- it (its body's thread): each mount asks, once, whether its thread has a
-- bracket open, and a tree it finds one for keeps it for its life. Trees
-- mounted outside, or by other threads (a server's per connection, specs
-- running side by side), are not touched, and a program no test controls
-- pays one read of a reference per mount and nothing per wait. The two
-- brackets go together (a tree mounted inside both is held and clocked),
-- and an inner bracket of the same kind shadows the outer for the trees
-- mounted inside it.
--
-- = Held waits
--
-- Inside 'withHeldWaits', a component's program that waits
-- ('Halogen.Query.HalogenM.lift', 'liftIO', the body of a
-- 'UnliftIO.withRunInIO') is held: its worker starts and the fiber counts
-- as waiting ("Halogen.Activity"), as always, but the action does not start
-- until the test lets it ('release'), or makes it throw ('failWith').
-- What is not a wait is not held: a 'Halogen.Query.HalogenM.liftEffect'
-- (it runs at once) and a pause ('Halogen.Query.HalogenM.delay',
-- 'Halogen.Query.HalogenM.delayCounted': those are the clock's).
-- A held wait whose fiber is cancelled (its fork killed, its component gone)
-- ends without running its action, and leaves the list.
--
-- 'pending' is the test's step: it returns the waits held once the
-- program has done all it can without the test (no tree running or with
-- work queued, every fiber that waits held), oldest first, with where each
-- was made (its component's id and label, as "Halogen.Trace" knows them).
-- When the bracket ends, every wait still held is released and its trees
-- hold no more: dispose of them inside it to have nothing run after.
--
-- = The test's clock
--
-- Inside 'withTestClock', the trees' pauses, 'Halogen.Query.HalogenM.delay'
-- and 'Halogen.Query.HalogenM.delayCounted' (@Hooks.delay@ and
-- @Hooks.delayCounted@, and so hooks' debouncer and throttle), sleep on the
-- test's clock: nothing wakes until 'advance' moves it past their time,
-- which it does as fake timers do (see 'advance'). Counting is as on the
-- real clock: a 'delay' asleep is neither running nor waiting, a
-- 'delayCounted' one is waiting. @liftIO (threadDelay n)@ is a wait like
-- any other, not a pause: it sleeps on the real clock (and is held, inside
-- 'withHeldWaits'), so a pause a test should move on is written with
-- 'Halogen.Query.HalogenM.delayCounted'. Timers that are not a program's
-- pauses keep the real clock: 'Halogen.Query.Event.Window.interval''s
-- emitter, "Halogen.Component.Debounced"'s state writes. There is no
-- mocking of the wall clock ('Data.Time.getCurrentTime'); 'now' is the test
-- clock's own reading. When the bracket ends, the pauses still asleep
-- sleep what is left of them on the real clock.
module Halogen.Activity.Control
  ( -- * Held waits
    withHeldWaits
  , HeldWaits
  , HeldWait (number, tag)
  , pending
  , release
  , failWith
  , releaseAll

    -- * The test's clock
  , withTestClock
  , TestClock
  , advance
  , now
  )
where

import Halogen.Activity.Control.Internal
