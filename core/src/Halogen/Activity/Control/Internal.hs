{-# LANGUAGE MultiWayIf #-}

-- | What "Halogen.Activity.Control" is made of, and what the driver calls:
-- a tree learns at its mount whether a test controls it ('controlFor'), and
-- its programs' waits ('awaitHeld') and pauses ('pause') ask the tree's
-- control, if it has one, what to do. Not exposed.
--
-- With nobody controlling, a mount reads one reference (the registry of
-- brackets, found empty), and a wait or a pause reads a field of the tree
-- that says 'Nothing': no more reference is read and nothing is written.
module Halogen.Activity.Control.Internal
  ( -- * Trees
    TreeControl
  , controlFor

    -- * Held waits
  , HeldWaits
  , HeldWait (..)
  , withHeldWaits
  , pending
  , release
  , releaseAll
  , failWith
  , awaitHeld
  , unliftedRuns

    -- * The test's clock
  , TestClock
  , withTestClock
  , advance
  , now
  , pause
  , sleepFor
  )
where

import Control.Concurrent (ThreadId, forkIO, forkIOWithUnmask, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, bracket, finally, mask, mask_, onException, throwIO, toException, try)
import Control.Monad (filterM, join, unless, void, when, (<=<))
import Data.Foldable (for_, traverse_)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Halogen.Activity.Internal (Activity (..), waitReading)
import Halogen.IO.Driver.Runtime
import Halogen.Trace.Internal (Tag (..))
import Prelude
import System.IO.Unsafe (unsafePerformIO)

----------------------------------------------------------------------
-- Which trees are controlled

-- | What a thread's open brackets control: the trees it mounts.
data Controls = Controls
  { heldBy :: Maybe HeldWaits
  , clockedBy :: Maybe TestClock
  }

-- | The brackets open, by the thread that opened them. Empty, as it is for
-- every program no test controls, a mount reads it once and goes on.
controllers :: IORef (Map ThreadId Controls)
controllers = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE controllers #-}

-- | Run the body with this thread's controls changed, and put them back as
-- they were however it ends. An inner bracket of the same kind shadows the
-- outer one for the trees mounted inside it.
controlling :: (Controls -> Controls) -> IO a -> IO a
controlling change body = do
  me <- myThreadId
  bracket
    (atomicModifyIORef' controllers $ \m -> let before = Map.lookup me m in (Map.insert me (change (fromMaybe none before)) m, before))
    (\before -> atomicModifyIORef' controllers (\m -> (Map.alter (const before) me m, ())))
    (const body)
  where
    none = Controls {heldBy = Nothing, clockedBy = Nothing}

-- | What a tree mounted under control keeps: the brackets it was mounted
-- in, its loop (which they read), and how many of its fibers counted as
-- waiting only the test can bring back.
data TreeControl = TreeControl
  { loop :: Loop
  , heldWaits :: Maybe HeldWaits
  , testClock :: Maybe TestClock
  , parked :: TVar Parked
  }

-- | The tree's waits that only the test can end: held ones, and counted
-- pauses asleep on the test's clock. With the number, how many times it has
-- changed, so that a reading can tell that it did not change while the
-- loops were read (a count alone could go down and up again in between).
data Parked = Parked {count :: Int, changes :: Word}
  deriving stock (Eq)

park :: TVar Parked -> Int -> STM ()
park v n = modifyTVar' v (\p -> Parked {count = p.count + n, changes = p.changes + 1})

-- | The control of a tree being mounted, on the thread that mounts it:
-- 'Nothing' unless that thread has a bracket open. The tree is added to
-- the brackets', which read its loop from then on.
controlFor :: Loop -> IO (Maybe TreeControl)
controlFor loop = do
  open <- readIORef controllers
  if Map.null open
    then pure Nothing
    else do
      me <- myThreadId
      case Map.lookup me open of
        Nothing -> pure Nothing
        Just cs -> do
          parked <- newTVarIO (Parked 0 0)
          let tc = TreeControl {loop, heldWaits = cs.heldBy, testClock = cs.clockedBy, parked}
          atomically $ do
            for_ cs.heldBy $ \h -> modifyTVar' h.trees (tc :)
            for_ cs.clockedBy $ \c -> modifyTVar' c.trees (tc :)
          pure (Just tc)

-- | Wait until none of these trees is running or has work queued and, when
-- asked, every fiber of theirs counted as waiting is parked (so nothing
-- will come back without the test). Woken as "Halogen.Activity"'s waits
-- are, and by every change of what is parked. No deadline.
settle :: Bool -> STM [TreeControl] -> IO ()
settle parkedToo trees = waitReading counts reading done
  where
    counts = trees >>= traverse (readTVar . (.parked))
    reading = do
      ts <- atomically trees
      before <- atomically (traverse (readTVar . (.parked)) ts)
      loops <- traverse (loopActivity . (.loop)) ts
      after <- atomically (traverse (readTVar . (.parked)) ts)
      -- What is parked changed while the loops were read: the reading is
      -- not of one moment, and counts as busy. The change woke the wait
      -- already (it differs from what the wait read before this), so the
      -- wait reads again at once.
      let stable = before == after
          busy = length (filter fst loops)
          unparked = sum (zipWith (\(_, waits) p -> abs (waits - p.count)) loops after)
      pure (Activity {trees = length ts, running = if stable then busy else busy + 1, waiting = unparked}, False)
    done a = a.running == 0 && (not parkedToo || a.waiting == 0)

----------------------------------------------------------------------
-- Held waits

-- | The waits of the trees mounted inside one 'withHeldWaits'.
data HeldWaits = HeldWaits
  { trees :: TVar [TreeControl]
  , holding :: TVar (IntMap HeldWait)
  -- ^ Held now, by number.
  , nextNumber :: TVar Int
  , closed :: TVar Bool
  }

-- | A wait of a component's program (a 'Halogen.Query.HalogenM.lift' or
-- 'liftIO', or the body of a 'UnliftIO.withRunInIO') that the test holds:
-- its worker has started and the fiber counts as waiting, but the action
-- has not started and does not until the wait is released.
data HeldWait = HeldWait
  { number :: Int
  -- ^ Its place among the bracket's waits, from 1, in the order the
  -- programs came to them: on the tree's loop, so the order is the
  -- program's, not the order in which their threads happened to start.
  , tag :: Tag
  -- ^ The component whose program waits, as "Halogen.Trace" knows it: its
  -- id, its parent's, and its label (the state type's name, or
  -- 'Halogen.Component.named''s).
  , decision :: TMVar Decision
  , waits :: HeldWaits
  , tree :: TreeControl
  , gone :: IO Bool
  -- ^ Whether the fiber that waits is over (cancelled): its worker is then
  -- about to be killed, and the wait is no longer the test's to decide.
  }

-- | Equal when they are the same wait.
instance Eq HeldWait where
  a == b = a.decision == b.decision

instance Show HeldWait where
  showsPrec p w =
    showParen (p > 10) $
      showString "HeldWait "
        . shows w.number
        . showString " of "
        . shows w.tag.label
        . showString " (component "
        . shows w.tag.id
        . showString ")"

data Decision = Go | Fail SomeException

-- | Hold the waits of the trees mounted inside the body (see
-- "Halogen.Activity.Control"). When the body is over, however it ends,
-- every wait still held is released, and the bracket's trees hold nothing
-- more: a tree may outlive the bracket, and a fiber held for ever would
-- keep it from going quiet, or from being disposed of cleanly.
withHeldWaits :: (HeldWaits -> IO a) -> IO a
withHeldWaits body = do
  hw <- HeldWaits <$> newTVarIO [] <*> newTVarIO IntMap.empty <*> newTVarIO 1 <*> newTVarIO False
  -- Closed first, in its own transaction: a worker that has not yet held
  -- its wait then lets it go at once, and one that has is released below.
  controlling (\c -> c {heldBy = Just hw}) (body hw)
    `finally` (atomically (writeTVar hw.closed True) >> releaseAll hw)

-- | Once the bracket's trees have done what they can (none is running or
-- has work queued, and every fiber of theirs that waits is held, or asleep
-- on a test clock with a counted pause), the waits held now, oldest first.
--
-- A wait that was released comes back to its program first: until its
-- program has gone on as far as it can, 'pending' waits. A fiber waiting
-- for something the test does not hold (a pause on the real clock with
-- 'Halogen.Query.HalogenM.delayCounted', a wait made before the bracket
-- opened) is waited for too. No deadline of its own: if the trees never
-- go idle (a loop that never ends, a wait that never comes back), neither
-- does 'pending'; wrap it in 'System.Timeout.timeout'.
pending :: HeldWaits -> IO [HeldWait]
pending hw = do
  settle True (readTVar hw.trees)
  -- A fiber cancelled just now (its component gone) is over at once, and
  -- its worker, which holds the wait, is killed a moment later: its wait is
  -- not one the program is waiting on any more.
  filterM (fmap not . (.gone)) . IntMap.elems =<< readTVarIO hw.holding

-- | Let a held wait's action run, on its worker; returns at once. A wait
-- that is no longer held (released already, or its fiber cancelled) is
-- left as it is.
release :: HeldWait -> IO ()
release = decide Go

-- | Have a held wait throw this instead of running its action, as if the
-- action had thrown it: for a test of a program's error path without a
-- backend that fails. Returns at once.
failWith :: (Exception e) => HeldWait -> e -> IO ()
failWith w e = decide (Fail (toException e)) w

decide :: Decision -> HeldWait -> IO ()
decide d w = atomically $ do
  held <- IntMap.lookup w.number <$> readTVar w.waits.holding
  for_ held $ \h -> when (h == w) $ do
    modifyTVar' w.waits.holding (IntMap.delete w.number)
    park w.tree.parked (-1)
    void (tryPutTMVar w.decision d)

-- | Release every wait held now.
releaseAll :: HeldWaits -> IO ()
releaseAll hw = atomically $ do
  held <- readTVar hw.holding
  writeTVar hw.holding IntMap.empty
  for_ held $ \w -> park w.tree.parked (-1) >> void (tryPutTMVar w.decision Go)

-- | The suspension of a 'Halogen.Query.HalogenM.Lift': 'await', held when
-- the tree is under 'withHeldWaits'. The wait is numbered here, on the
-- loop, and held by its worker.
awaitHeld :: Maybe TreeControl -> Tag -> IO a -> Turn a
awaitHeld control tag io = case control of
  Just tc | Just hw <- tc.heldWaits -> do
    n <- sync (atomically (stateTVar hw.nextNumber (\i -> (i, i + 1))))
    fb <- currentFiber
    await (gate tc hw n tag (fiberOver fb) io)
  _ -> await io

-- | On the worker: hold the wait until it is released (or the fiber is
-- cancelled, which kills the worker here and takes the wait off the list),
-- then run the action or throw what the test chose.
--
-- A fiber that is cancelled is over at once, and its worker is killed from
-- another thread a moment later. A wait released in between (a test that
-- disposes of its tree and then leaves the bracket, which releases what is
-- held) must not run its action: what follows a cancelled wait never runs.
-- So the fiber is asked once the wait is let go, and a worker whose fiber
-- is over ends here instead.
gate :: TreeControl -> HeldWaits -> Int -> Tag -> IO Bool -> IO a -> IO a
gate tc hw n tag gone io = do
  decision <- newEmptyTMVarIO
  let w = HeldWait {number = n, tag, decision, waits = hw, tree = tc, gone}
      leave = atomically $ do
        here <- IntMap.member n <$> readTVar hw.holding
        when here $ modifyTVar' hw.holding (IntMap.delete n) >> park tc.parked (-1)
  d <- mask $ \restore -> do
    held <- atomically $ do
      shut <- readTVar hw.closed
      unless shut $ modifyTVar' hw.holding (IntMap.insert n w) >> park tc.parked 1
      pure (not shut)
    if held then restore (atomically (takeTMVar decision)) `onException` leave else pure Go
  over <- gone
  when over (throwIO FiberGone)
  case d of
    Go -> io
    Fail e -> throwIO e

-- | What ends a worker whose fiber was cancelled while it was held: nobody
-- hears it, since a fiber that is over takes no result.
data FiberGone = FiberGone
  deriving stock (Show)

instance Exception FiberGone

-- | For the body of a 'Halogen.Query.HalogenM.Unlift' in a controlled tree:
-- an action to run for each program the body hands to the loop, which
-- gives two more: one to run once the program is queued, and one to run
-- when it has ended (on the loop, before the body hears of it).
--
-- The body's worker is counted as waiting for as long as the body runs, and
-- while it waits for a program of its own it can do nothing until that
-- program ends: it is parked, like a held wait, or 'pending' would wait for
-- ever for a worker that waits for a wait the test holds. It is parked
-- while any program it started is under way, and:
--
-- * parked only once the program is queued, so that the loop is running,
--   or the program's own wait is counted, by the time the worker reads as
--   parked: parked first, the tree would read as having nothing to do
--   between the two, and 'pending' would return without the program's wait;
-- * unparked on the loop as the last program ends, before the body goes
--   on, so that 'pending' does not return between the program's end and
--   what the body does next.
--
-- A program may end before its caller is parked (it waited for nothing):
-- then the caller is not parked for it at all. (A body that runs programs
-- from several threads and does other work on one of them while another's
-- program is held is taken for parked meanwhile.)
unliftedRuns :: Maybe TreeControl -> IO (IO (IO (), IO ()))
unliftedRuns = \case
  Nothing -> pure (pure (pure (), pure ()))
  Just tc -> do
    under <- newTVarIO (0 :: Int)
    pure $ do
      state <- newTVarIO Queued
      let parkNow = atomically $ do
            st <- readTVar state
            when (st == Queued) $ do
              writeTVar state Waited
              n <- readTVar under
              writeTVar under (n + 1)
              when (n == 0) (park tc.parked 1)
          ended = atomically $ do
            st <- readTVar state
            writeTVar state Ended
            when (st == Waited) $ do
              n <- readTVar under
              writeTVar under (n - 1)
              when (n == 1) (park tc.parked (-1))
      pure (parkNow, ended)

-- | Where a program a body runs stands: handed to the loop, its caller
-- parked for it, or over.
data Run = Queued | Waited | Ended
  deriving stock (Eq)

----------------------------------------------------------------------
-- The test's clock

-- | The clock of the trees mounted inside one 'withTestClock'.
data TestClock = TestClock
  { trees :: TVar [TreeControl]
  , time :: TVar NominalDiffTime
  -- ^ Since the bracket opened.
  , sleepers :: TVar (Map (NominalDiffTime, Int) Sleeper)
  -- ^ By when each is due, then the order in which they went to sleep.
  , nextSleeper :: TVar Int
  , closed :: TVar Bool
  , advancing :: MVar ()
  -- ^ One 'advance' at a time.
  }

data Sleeper = Sleeper
  { wake :: IO ()
  -- ^ Its time has come: its fiber goes on.
  , handOff :: NominalDiffTime -> IO ()
  -- ^ The bracket is over: it sleeps what is left of its pause on the real
  -- clock.
  }

data SleepState = OnClock | Handed | Over
  deriving stock (Eq)

-- | Run the body with a clock of the test's own for the trees mounted
-- inside it (see "Halogen.Activity.Control"). It starts at zero and moves
-- only with 'advance'. When the body is over, however it ends, the pauses
-- still asleep on it sleep what is left of them on the real clock, as the
-- pauses the trees make from then on do.
withTestClock :: (TestClock -> IO a) -> IO a
withTestClock body = do
  clock <- TestClock <$> newTVarIO [] <*> newTVarIO 0 <*> newTVarIO Map.empty <*> newTVarIO 0 <*> newTVarIO False <*> newMVar ()
  controlling (\c -> c {clockedBy = Just clock}) (body clock) `finally` close clock
  where
    close clock = do
      (t, left) <- atomically $ do
        writeTVar clock.closed True
        t <- readTVar clock.time
        left <- readTVar clock.sleepers
        writeTVar clock.sleepers Map.empty
        pure (t, left)
      for_ (Map.toList left) $ \((due, _), s) -> s.handOff (due - t)

-- | The time on the clock: how far it has been advanced since the bracket
-- opened.
now :: TestClock -> IO NominalDiffTime
now clock = readTVarIO clock.time

-- | Move the clock forward by this much (nothing for a duration that is not
-- positive), as a fake timer does: first the trees are let run until none
-- is running or has work queued; then, while a pause is due by the end of
-- the duration, the clock moves to the earliest one's due time (of two due
-- at once, the one that went to sleep first), it wakes, and the trees run
-- until none is running or has work queued again, so that a pause armed by
-- what it ran is honoured in the same 'advance' if it is due by then; then
-- the clock moves to the end of the duration.
--
-- It waits for no 'liftIO' (one may be held, see 'withHeldWaits'): what a
-- woken program waits for comes back afterwards, and a test waits for it as
-- it would for any other ('Halogen.Activity.waitActivity'). No deadline of
-- its own: a tree that never goes idle keeps it waiting. One 'advance' runs
-- at a time; not to be called from a component's own synchronous code,
-- which holds the loop it waits for.
advance :: TestClock -> NominalDiffTime -> IO ()
advance clock d = withMVar clock.advancing $ \() -> do
  settled
  target <- atomically ((+ max 0 d) <$> readTVar clock.time)
  let go = do
        -- Masked from taking the sleeper off the clock until it is woken
        -- (neither blocks): an 'advance' interrupted in between, by a
        -- test's 'timeout' say, would leave a sleeper that is on no clock
        -- and that nothing wakes. The wait for the trees is not masked.
        woke <- mask_ $ do
          due <- atomically $ do
            m <- readTVar clock.sleepers
            case Map.lookupMin m of
              Just (k@(at, _), s) | at <= target -> do
                writeTVar clock.sleepers (Map.delete k m)
                modifyTVar' clock.time (max at)
                pure (Just s)
              _ -> Nothing <$ modifyTVar' clock.time (max target)
          for_ due (.wake)
          pure (isJust due)
        when woke (settled >> go)
  go
  where
    settled = settle False (readTVar clock.trees)

-- | A pause of a component's program: on the tree's test clock if it was
-- mounted under one, otherwise a sleep on a worker, counted as waiting
-- ('Halogen.Query.HalogenM.delayCounted') or not
-- ('Halogen.Query.HalogenM.delay').
pause :: Maybe TreeControl -> Bool -> NominalDiffTime -> Turn ()
pause control counted d = case control of
  Just tc | Just clock <- tc.testClock -> onTestClock tc clock counted d
  _ -> (if counted then await else awaitUncounted) (sleepFor d)

-- | A pause on the test's clock. No worker: the fiber is set aside
-- ('suspend') with a sleeper on the clock, and 'advance' wakes it by
-- queuing its next step on the loop, so the tree is running from the
-- moment it wakes, with no gap for a reading to find it idle in. Counted,
-- it is waiting (and parked, as only the test can end it) until then.
-- A fiber cancelled while it sleeps takes its sleeper off the clock.
onTestClock :: TreeControl -> TestClock -> Bool -> NominalDiffTime -> Turn ()
onTestClock tc clock counted d = do
  fb <- currentFiber
  suspend $ \resume -> mask_ $ do
    state <- newIORef OnClock
    thread <- newIORef Nothing
    -- How to stop hearing the fiber's end, once the pause is over: a fiber
    -- that pauses for ever (a ticker) would otherwise keep a listener for
    -- every pause it ever made.
    unwatch <- newIORef (pure ())
    let move from to = atomicModifyIORef' state (\s -> if s == from then (to, True) else (s, False))
        uncount = when counted (countAwaiting tc.loop (-1))
        unpark = when counted (atomically (park tc.parked (-1)))
        -- Queued, then uncounted: the fiber is counted on one side or both.
        goOn = post tc.loop (resume (Right ())) >> uncount >> join (readIORef unwatch)
        wake = do
          mine <- move OnClock Over
          when mine (unpark >> goOn)
        handOff rest = do
          mine <- move OnClock Handed
          when mine $ do
            unpark
            t <- forkIOWithUnmask $ \unmask -> do
              slept <- try (unmask (sleepFor rest))
              case slept of
                Left (_ :: SomeException) -> pure ()
                Right () -> do
                  still <- move Handed Over
                  when still goOn
            writeIORef thread (Just t)
    when counted (countAwaiting tc.loop 1)
    entry <- atomically $ do
      when counted (park tc.parked 1)
      shut <- readTVar clock.closed
      if shut
        then pure Nothing
        else do
          t <- readTVar clock.time
          i <- stateTVar clock.nextSleeper (\i -> (i, i + 1))
          let key = (t + max 0 d, i)
          modifyTVar' clock.sleepers (Map.insert key Sleeper {wake, handOff})
          pure (Just key)
    traverse_ (writeIORef unwatch) <=< watchFiber fb $ \_ -> do
      before <- atomicModifyIORef' state (Over,)
      case before of
        OnClock -> do
          for_ entry $ \key -> atomically (modifyTVar' clock.sleepers (Map.delete key))
          unpark >> uncount
        -- From another thread: the sleeper may be between its wake and its
        -- post, masked, which 'killThread' would wait for.
        Handed -> do
          readIORef thread >>= traverse_ (void . forkIO . killThread)
          uncount
        Over -> pure ()
    -- The bracket is over: the real clock.
    when (null entry) (handOff d)

-- | Sleep for a duration, in parts small enough for an 'Int' of
-- microseconds on a 32-bit backend (WebAssembly's and JavaScript's 'Int'
-- holds about 35 minutes of them). Rounded up to the microsecond, so that a
-- pause is never shorter than asked; nothing at all for a duration that is
-- not positive.
sleepFor :: NominalDiffTime -> IO ()
sleepFor d = go (ceiling (nominalDiffTimeToSeconds d * 1_000_000) :: Integer)
  where
    go n
      | n <= 0 = pure ()
      | otherwise = do
          let part = min n chunk
          threadDelay (fromInteger part)
          go (n - part)
    -- A thousand seconds, well inside a 32-bit 'Int' of microseconds.
    chunk = 1_000_000_000
