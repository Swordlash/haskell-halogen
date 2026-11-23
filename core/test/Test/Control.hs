-- | "Halogen.Activity.Control": a test holds a program's waits and lets them
-- go in its own order, and moves its pauses on with a clock of its own. On
-- a renderer that records the text of every render, with no DOM, so on
-- every backend. Every tree is disposed of by its test, and every reading
-- is of the test's own trees, so this spec runs anywhere in the suite.
module Test.Control (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay, yield)
import Control.Concurrent.MVar
import Control.Exception (ErrorCall (..), SomeException, bracket, throwIO)
import Control.Monad (forever, replicateM_, void)
import Control.Monad.Catch (try)
import Control.Monad.State.Class (get, modify, put)
import Data.Foldable (for_, traverse_)
import Data.IORef
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Row (Empty, Row, type (.==))
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Data.Void (Void)
import GHC.Clock (getMonotonicTime)
import Halogen qualified as H
import Halogen.Activity (Activity (..))
import Halogen.Activity qualified as Activity
import Halogen.Activity.Control (HeldWait (..), HeldWaits, Sleeping (..), advance, describeSleeping, failWith, now, pending, release, sleeping, sleepingHere, withHeldWaits, withTestClock)
import Halogen.Component (ComponentSlot (..))
import Halogen.HTML qualified as HH
import Halogen.HTML.Core qualified as HC
import Halogen.IO.Driver qualified as AD
import Halogen.Query.Input (Input)
import Halogen.Subscription qualified as HS
import Halogen.Trace (Tag (..))
import Halogen.Trace qualified as Trace
import Halogen.VDom.Types (VDom (..), runGraft)
import Prelude
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)

----------------------------------------------------------------------
-- A renderer that records what it renders

data Rendered (s :: Type) (act :: Type) (ps :: Row Type) (o :: Type) = Rendered

-- | Renders the children, and appends the text of every render of a
-- component with text of its own (the components here show one each) to
-- the list.
recording :: IORef [String] -> AD.RenderSpec IO Rendered
recording renders =
  AD.RenderSpec
    { AD.render = render
    , AD.renderChild = id
    , AD.removeChild = \_ -> pure ()
    , AD.dispose = \_ -> pure ()
    , AD.sameElement = \_ _ -> pure False
    , AD.rootNode = \_ -> pure Nothing
    }
  where
    render
      :: (Input act -> IO ())
      -> AD.ChildSlots ps IO act Rendered
      -> HC.HTML (ComponentSlot ps IO act) act
      -> Maybe (Rendered s act ps o)
      -> IO (Rendered s act ps o)
    render _ AD.ChildSlots {AD.renderSlot = renderChild} html _ = do
      let vdom = HC.unHTML html
      traverse_ (\case ComponentSlot box -> void (renderChild box); ThunkSlot _ -> pure ()) (slots vdom)
      case texts vdom of
        [] -> pure ()
        ts -> modifyIORef renders (<> [concat ts])
      pure Rendered
    slots :: VDom p w -> [w]
    slots = \case
      Text _ -> []
      Elem _ _ _ cs -> foldMap slots cs
      Keyed _ _ _ cs -> foldMap (slots . snd) cs
      Widget w -> [w]
      Grafted g -> slots (runGraft g)
    texts :: VDom p w -> [String]
    texts = \case
      Text t -> [T.unpack t]
      Elem _ _ _ cs -> foldMap texts cs
      Keyed _ _ _ cs -> foldMap (texts . snd) cs
      Widget _ -> []
      Grafted g -> texts (runGraft g)

type Socket = AD.HalogenSocket H.VoidF Void IO

mountRecorded :: H.Component H.VoidF () Void IO -> IO (Socket, IORef [String])
mountRecorded c = do
  renders <- newIORef []
  socket <- AD.runUI (recording renders) c ()
  pure (socket, renders)

-- | A tree, with what it renders, taken away afterwards whatever the body
-- did: a test that failed would otherwise leave fibers held or asleep.
withTree :: H.Component H.VoidF () Void IO -> (Socket -> IORef [String] -> IO a) -> IO a
withTree c body = bracket (mountRecorded c) (dispose . fst) (uncurry body)

dispose :: AD.HalogenSocket q o IO -> IO ()
dispose AD.HalogenSocket {AD.dispose = d} = d

treeActivity :: AD.HalogenSocket q o IO -> IO Activity
treeActivity AD.HalogenSocket {AD.activity = a} = a

waitTree :: AD.HalogenSocket q o IO -> (Activity -> Bool) -> IO ()
waitTree AD.HalogenSocket {AD.waitActivity = w} p = within "the tree's reading" (w p)

within :: String -> IO a -> IO a
within what io = timeout 5_000_000 io >>= maybe (throwIO (ErrorCall ("timed out: " <> what))) pure

-- | 'pending', with a deadline.
pendingOf :: HeldWaits -> IO [HeldWait]
pendingOf waits = within "pending" (pending waits)

-- | The text of the last render.
lastShown :: IORef [String] -> IO String
lastShown renders = readIORef renders >>= \rs -> pure (if null rs then "" else last rs)

-- | Real time taken by the body.
timed :: IO a -> IO (a, Double)
timed io = do
  start <- getMonotonicTime
  a <- io
  end <- getMonotonicTime
  pure (a, end - start)

----------------------------------------------------------------------
-- Components

-- | A component that loads a word in its @Initialize@, and another for
-- every word its emitter sends (a click, say), each in an 'H.async' that
-- notes the word as it runs; it shows the words in the order they came
-- back.
loader :: HS.Emitter IO String -> IORef [String] -> H.Component H.VoidF () Void IO
loader asks ran =
  H.named "loader" $
    H.component
      (\() -> pure [])
      (\ws -> HH.text (T.pack (intercalate "," ws)) :: H.ComponentHTML String Empty IO)
      [ H.onInitialize (void (H.subscribe asks) >> load "first")
      , H.onAction load
      ]
  where
    load w = do
      got <- H.async (modifyIORef ran (<> [w]) >> pure w)
      modify (<> [got])

-- | A component whose @Initialize@ is this program, its state a number it
-- shows.
program :: H.HalogenM Int () Empty Void IO () -> H.Component H.VoidF () Void IO
program p =
  H.named "program" $
    H.component
      (\() -> pure 0)
      (\n -> HH.text (T.pack (show n)) :: H.ComponentHTML () Empty IO)
      [H.onInitialize p]

-- | A parent and its child, each waiting in its @Initialize@.
family :: H.Component H.VoidF () Void IO
family =
  H.named "parent" $
    H.component
      (\() -> pure ())
      (\() -> HH.div_ [HH.slot_ "child" () child ()] :: H.ComponentHTML () ("child" .== H.Slot H.VoidF Void ()) IO)
      [H.onInitialize (H.async (pure ()))]
  where
    child =
      H.named "child" $
        H.component
          (\() -> pure ())
          (\() -> HH.text "" :: H.ComponentHTML () Empty IO)
          [H.onInitialize (H.async (pure ()))]

-- | A search box: every word typed restarts a debounce of 0.3 s (a counted
-- pause in a fork, the last one killed), after which the word is looked up
-- in an 'H.async' that notes it, and the answer shown.
search :: HS.Emitter IO String -> IORef [String] -> H.Component H.VoidF () Void IO
search typed looked =
  H.named "search" $
    H.component
      (\() -> pure (Nothing, ""))
      (\(_, answer) -> HH.text (T.pack answer) :: H.ComponentHTML String Empty IO)
      [ H.onInitialize (void (H.subscribe typed))
      , H.onAction $ \w -> do
          (previous, answer) <- get
          traverse_ H.kill previous
          fid <- H.fork $ do
            H.delayCounted 0.3
            got <- H.async (modifyIORef looked (<> [w]) >> pure ("found " <> w))
            put (Nothing, got)
          put (Just fid, answer)
      ]

-- | Note something from the loop, with the test clock's time then.
note :: IORef [(String, NominalDiffTime)] -> IO NominalDiffTime -> String -> H.HalogenM s act ps o IO ()
note notes clock what = H.sync (clock >>= \t -> modifyIORef notes (<> [(what, t)]))

mark :: MVar () -> H.HalogenM s act ps o IO ()
mark var = H.sync (void (tryPutMVar var ()))

----------------------------------------------------------------------

spec :: Spec
spec = describe "Halogen.Activity.Control" $ do
  describe "held waits" $ do
    it "holds a program's waits, oldest first, and lets them go in the test's order" $ do
      ran <- newIORef []
      asks <- HS.create
      withHeldWaits $ \waits ->
        withTree (loader asks.emitter ran) $ \ui renders -> do
          [readAsk] <- pendingOf waits
          HS.notify asks.listener "second"
          [first, readAsk2] <- pendingOf waits
          assertWith "the first is still held, and first" (first == readAsk)
          assertWith "the newer one is numbered after it" (readAsk2.number > readAsk.number)
          readIORef ran >>= assertEqual "no action has run" []
          release readAsk2
          pendingOf waits >>= assertEqual "the first is still held, alone" [readAsk]
          readIORef ran >>= assertEqual "the second's action ran" ["second"]
          readIORef renders >>= assertEqual "the second shown first" ["", "second"]
          release readAsk
          pendingOf waits >>= assertEqual "nothing held" []
          readIORef renders >>= assertEqual "then the first" ["", "second", "second,first"]
          treeActivity ui >>= assertEqual "quiet" (Activity 1 0 0 0)

    it "returns from pending once the program has gone as far as it can: a wait made after a release is in the next" $
      withHeldWaits $ \waits ->
        withTree (program (H.async (pure ()) >> modify (+ 1) >> H.async (pure ()) >> modify (+ 1))) $ \_ renders -> do
          [w1] <- pendingOf waits
          release w1
          [w2] <- pendingOf waits
          assertWith "a new wait" (w2 /= w1 && w2.number == w1.number + 1)
          readIORef renders >>= assertEqual "the step between rendered" ["0", "1"]
          release w2
          pendingOf waits >>= assertEqual "nothing held" []
          readIORef renders >>= assertEqual "both steps rendered" ["0", "1", "2"]

    it "says which component made each wait" $
      withHeldWaits $ \waits ->
        withTree family $ \_ _ -> do
          [c, p] <- pendingOf waits
          assertEqual "the child's first (it initializes first)" "child" c.tag.label
          assertEqual "then the parent's" "parent" p.tag.label
          assertEqual "the child's parent" (Just p.tag.id) c.tag.parent
          release c >> release p
          pendingOf waits >>= assertEqual "nothing held" []

    it "makes a wait throw with failWith, its action not run" $ do
      ran <- newIORef False
      withHeldWaits $ \waits ->
        withTree (program (try (H.async (writeIORef ran True)) >>= put . either (\(_ :: SomeException) -> -1) (const 1))) $ \ui renders -> do
          [w] <- pendingOf waits
          failWith w (ErrorCall "the backend is down")
          pendingOf waits >>= assertEqual "nothing held" []
          lastShown renders >>= assertEqual "the program took the error path" "-1"
          readIORef ran >>= assertEqual "the action never ran" False
          treeActivity ui >>= assertEqual "quiet" (Activity 1 0 0 0)

    it "takes a wait whose component goes away off the list, its action never run" $ do
      ran <- newIORef []
      asks <- HS.create
      withHeldWaits $ \waits -> do
        (socket, _) <- mountRecorded (loader asks.emitter ran)
        [w] <- pendingOf waits
        dispose socket
        waitTree socket Activity.quiet
        pendingOf waits >>= assertEqual "nothing held" []
        release w
        readIORef ran >>= assertEqual "the action never ran" []

    it "does not run a wait let go after its component went away, by the test or by the bracket's end" $ do
      -- The fiber is over the moment the tree is disposed of; its worker,
      -- which holds the wait, is killed a moment later, from another
      -- thread. A release in between must run nothing.
      replicateM_ 200 $ do
        ran <- newIORef []
        asks <- HS.create
        socket <- withHeldWaits $ \waits -> do
          (socket, _) <- mountRecorded (loader asks.emitter ran)
          [w] <- pendingOf waits
          dispose socket
          pendingOf waits >>= assertEqual "not the program's any more" []
          release w
          pure socket
        waitTree socket Activity.quiet
        readIORef ran >>= assertEqual "the action never ran, released by the test" []
      replicateM_ 200 $ do
        ran <- newIORef []
        asks <- HS.create
        socket <- withHeldWaits $ \waits -> do
          (socket, _) <- mountRecorded (loader asks.emitter ran)
          [_] <- pendingOf waits
          socket <$ dispose socket
        waitTree socket Activity.quiet
        readIORef ran >>= assertEqual "the action never ran, released by the bracket's end" []

    it "holds the programs an H.asyncWith runs, whose caller waits for them" $ do
      -- The body's worker counts as waiting while it waits for a program of
      -- its own, which the test holds: 'pending' must not wait for it.
      ran <- newIORef ([] :: [String])
      let step w = H.async (modifyIORef ran (<> [w]))
      withHeldWaits $ \waits ->
        withTree (program (H.asyncWith (\run -> run (step "a") >> run (step "b")) >> modify (+ 1))) $ \ui renders -> do
          [body] <- pendingOf waits
          release body
          [a] <- pendingOf waits
          assertWith "a new wait, the first program's" (a /= body)
          readIORef ran >>= assertEqual "nothing has run" []
          release a
          -- Not before the body has gone on to its second program.
          [b] <- pendingOf waits
          readIORef ran >>= assertEqual "the first ran" ["a"]
          release b
          pendingOf waits >>= assertEqual "nothing held" []
          readIORef ran >>= assertEqual "both ran" ["a", "b"]
          lastShown renders >>= assertEqual "the program went on after the body" "1"
          treeActivity ui >>= assertEqual "quiet" (Activity 1 0 0 0)

    it "holds the trees mounted inside the bracket on its thread, and no other" $ do
      ranOutside <- newIORef []
      ranOther <- newIORef []
      ranInside <- newIORef []
      asks <- HS.create
      withTree (loader asks.emitter ranOutside) $ \outside _ -> do
        waitTree outside Activity.quiet
        withHeldWaits $ \waits -> do
          -- Mounted inside the body, by another thread: not held.
          otherVar <- newEmptyMVar
          _ <- forkIO (mountRecorded (loader asks.emitter ranOther) >>= putMVar otherVar)
          (other, _) <- within "the other thread's mount" (takeMVar otherVar)
          waitTree other Activity.quiet
          readIORef ranOther >>= assertEqual "the other thread's tree loaded" ["first"]
          withTree (loader asks.emitter ranInside) $ \_ _ -> do
            [w] <- pendingOf waits
            readIORef ranInside >>= assertEqual "the inside tree's held" []
            -- The outside tree hears the emitter too, and is not held.
            HS.notify asks.listener "again"
            waitTree outside Activity.quiet
            readIORef ranOutside >>= assertEqual "the outside tree's" ["first", "again"]
            [w', again] <- pendingOf waits
            assertWith "the first is the one held before" (w' == w)
            release w >> void (pendingOf waits)
            release again
            pendingOf waits >>= assertEqual "nothing held" []
            readIORef ranInside >>= assertEqual "the inside tree's, once released" ["first", "again"]
          dispose other

    it "releases what is still held when the bracket ends, and holds nothing more" $ do
      ran <- newIORef []
      asks <- HS.create
      (socket, _) <- withHeldWaits $ \waits -> do
        mounted <- mountRecorded (loader asks.emitter ran)
        [_] <- pendingOf waits
        pure mounted
      waitTree socket Activity.quiet
      readIORef ran >>= assertEqual "released" ["first"]
      HS.notify asks.listener "later"
      waitTree socket Activity.quiet
      readIORef ran >>= assertEqual "not held after the bracket" ["first", "later"]
      dispose socket

  describe "the test's clock" $ do
    it "fires a debounce when advanced by its time and not a moment before, with no real waiting" $ do
      looked <- newIORef []
      typed <- HS.create
      (_, took) <- timed $ withTestClock $ \clock ->
        withTree (search typed.emitter looked) $ \ui renders -> do
          traverse_ (HS.notify typed.listener) ["a", "ab", "abc"]
          waitTree ui Activity.idle
          treeActivity ui >>= assertEqual "the last debounce sleeps on the clock, counted; the killed ones are gone" (Activity 1 0 1 1)
          advance clock 0.299
          readIORef looked >>= assertEqual "nothing looked up yet" []
          treeActivity ui >>= assertEqual "still asleep" (Activity 1 0 1 1)
          advance clock 0.001
          waitTree ui Activity.quiet
          readIORef looked >>= assertEqual "the last word looked up, once" ["abc"]
          lastShown renders >>= assertEqual "and shown" "found abc"
          now clock >>= assertEqual "the clock" 0.3
      assertWith ("far less than 0.3 s of real time: " <> show took) (took < 0.2)

    it "ticks a ticker as often as the clock is advanced by its period" $
      withTestClock $ \clock ->
        withTree (program (void (H.fork (forever (H.delay 1 >> modify (+ 1)))))) $ \ui renders -> do
          advance clock 3
          lastShown renders >>= assertEqual "three ticks" "3"
          treeActivity ui >>= assertEqual "quiet between ticks" (Activity 1 0 0 0)
          advance clock 3 >> advance clock 3
          lastShown renders >>= assertEqual "nine ticks" "9"
          now clock >>= assertEqual "the clock" 9

    it "wakes sleepers in due order, those due together in the order they went to sleep" $ do
      notes <- newIORef []
      withTestClock $ \clock -> do
        let at = note notes (now clock)
        withTree
          ( program $ do
              void $ H.fork (H.delay 2 >> at "b")
              void $ H.fork (H.delay 1 >> at "a")
              void $ H.fork (H.delay 2 >> at "c")
              void $ H.fork (H.delayCounted 1.5 >> at "counted")
          )
          $ \_ _ -> do
            advance clock 5
            readIORef notes >>= assertEqual "in due order" [("a", 1), ("counted", 1.5), ("b", 2), ("c", 2)]
            now clock >>= assertEqual "the clock, at the end" 5

    it "honours a pause armed by what an advance woke, within the same advance" $ do
      notes <- newIORef []
      withTestClock $ \clock -> do
        let at = note notes (now clock)
        withTree (program (void (H.fork (H.delay 1 >> at "a" >> H.delay 1 >> at "b" >> H.delay 1 >> at "c")))) $ \_ _ -> do
          advance clock 2
          readIORef notes >>= assertEqual "the second pause too" [("a", 1), ("b", 2)]
          advance clock 0.5
          readIORef notes >>= assertEqual "not the third yet" [("a", 1), ("b", 2)]
          advance clock 0.5
          readIORef notes >>= assertEqual "the third" [("a", 1), ("b", 2), ("c", 3)]

    let counting what asleep = do
          withTree (program (void (H.fork (H.delay 60)))) $ \ui _ ->
            treeActivity ui >>= assertEqual (what <> ": a delay is neither running nor waiting") (Activity 1 0 0 0)
          withTree (program (void (H.fork (H.delayCounted 60)))) $ \ui _ ->
            treeActivity ui >>= assertEqual (what <> ": a delayCounted is waiting") (Activity 1 0 1 asleep)
    it "counts a delay as nowhere and a delayCounted as waiting, on the real clock" $ counting "the real clock" 0
    it "counts a delay as nowhere and a delayCounted as waiting, and asleep, on the test's clock" $
      withTestClock $
        \_ -> counting "the test's clock" 1

    -- Quiet does not wait for a counted pause asleep on the test's clock:
    -- the test advances the clock when it wants what comes after the
    -- pause. One that does not (it forgot) finds the program quiet with
    -- the pause asleep, and the reading and the trace say so.
    it "is quiet with a counted pause asleep on it, which advancing wakes" $ do
      woke <- newIORef False
      withTestClock $ \clock ->
        withTree (program (void (H.fork (H.delayCounted 2 >> H.sync (writeIORef woke True))))) $ \ui _ -> do
          within "quiet, the pause asleep" (waitTree ui Activity.quiet)
          treeActivity ui >>= assertEqual "one fiber waiting, asleep on the clock" (Activity 1 0 1 1)
          readIORef woke >>= assertEqual "the work after the pause has not run" False
          advance clock 2
          within "quiet again, the pause over" (waitTree ui Activity.quiet)
          readIORef woke >>= assertEqual "the work after the pause has run" True
          treeActivity ui >>= assertEqual "nothing waiting" (Activity 1 0 0 0)

    -- What a harness names when a test forgot to advance: each pause asleep
    -- on the clock, with its component, its length, whether it is counted,
    -- and when it is due; the first due first. On the bracket's thread
    -- only, and none outside it.
    it "lists the pauses asleep on it, with their component, length and due time" $ do
      sleepingHere >>= assertEqual "no clock open" [] . map describeSleeping
      withTestClock $ \clock ->
        withTree (program (void (H.fork (H.delayCounted 2)) >> void (H.fork (H.delay 0.5)))) $ \ui _ -> do
          waitTree ui Activity.quiet
          advance clock 0.25
          asleep <- sleeping clock
          assertEqual
            "the delay first, then the counted pause"
            [("program", 0.5, False, 0.5, 0.25), ("program", 2, True, 2, 0.25)]
            (map (\p -> (p.tag.label, p.seconds, p.counted, p.due, p.clockAt)) asleep)
          here <- sleepingHere
          assertEqual "the same, from the bracket's thread" (map describeSleeping asleep) (map describeSleeping here)
          case asleep of
            [_, p] -> assertEqual "in words" ("program#" <> show p.tag.id <> ": a counted pause (delayCounted) of 2s, due at 2s on the test's clock, which is at 0.25s") (describeSleeping p)
            _ -> throwIO (ErrorCall "two pauses")
          advance clock 2
          sleeping clock >>= assertEqual "both woken" [] . map describeSleeping

    it "is not quiet with a counted pause on the real clock" $
      withTree (program (void (H.fork (H.delayCounted 60)))) $ \ui _ -> do
        waitTree ui Activity.idle
        a <- treeActivity ui
        assertEqual "the pause waits, and is not asleep on a test's clock" (Activity 1 0 1 0) a
        assertEqual "not quiet" False (Activity.quiet a)

    it "names a counted pause in the trace, and whether it sleeps on the test's clock" $ do
      heard <- newIORef []
      stop <- Trace.subscribe $ \event -> case event.what of
        Trace.CountedPause seconds clocked -> modifyIORef' heard (<> [(seconds, clocked)])
        _ -> pure ()
      withTestClock $ \_ ->
        withTree (program (void (H.fork (H.delayCounted 2)))) $ \ui _ -> waitTree ui Activity.quiet
      withTree (program (void (H.fork (H.delayCounted 60)))) $ \ui _ -> waitTree ui Activity.idle
      stop
      readIORef heard >>= assertEqual "both pauses, the first on the test's clock" [(2, True), (60, False)]

    it "leaves a tree mounted outside the bracket on the real clock" $ do
      outsideWoke <- newEmptyMVar
      insideWoke <- newEmptyMVar
      withTestClock $ \clock -> do
        otherVar <- newEmptyMVar
        -- Mounted inside the body, by another thread: the real clock.
        _ <- forkIO (mountRecorded (program (H.delay 0.05 >> mark outsideWoke)) >>= putMVar otherVar)
        withTree (program (H.delay 0.05 >> mark insideWoke)) $ \_ _ -> do
          within "the other tree's pause" (takeMVar outsideWoke)
          threadDelay 100_000
          tryReadMVar insideWoke >>= assertEqual "the inside tree's pause sleeps on" Nothing
          advance clock 0.05
          tryReadMVar insideWoke >>= assertEqual "until the test's clock reaches it" (Just ())
        (other, _) <- takeMVar otherVar
        dispose other

    it "takes the pause of a component that goes away off the clock" $ do
      woke <- newIORef False
      withTestClock $ \clock -> do
        (socket, _) <- mountRecorded (program (void (H.fork (H.delayCounted 1 >> H.sync (writeIORef woke True)))))
        treeActivity socket >>= assertEqual "counted, asleep on the clock" (Activity 1 0 1 1)
        dispose socket
        waitTree socket Activity.quiet
        advance clock 2
        readIORef woke >>= assertEqual "never woke" False

    it "loses no sleeper when an advance is interrupted" $
      -- An advance cut short (by a test's timeout, say) between taking a
      -- sleeper off the clock and waking it would leave it on no clock.
      for_ [0 .. 299 :: Int] $ \i -> do
        woke <- newEmptyMVar
        withTestClock $ \clock ->
          withTree (program (H.delayCounted 1 >> mark woke)) $ \_ _ -> do
            t <- forkIO (advance clock 1)
            replicateM_ (i `mod` 12) yield
            killThread t
            advance clock 1
            within "the sleeper to wake" (takeMVar woke)

    it "hands what still sleeps on it to the real clock when the bracket ends" $ do
      woke <- newEmptyMVar
      (socket, _) <- withTestClock $ \clock -> do
        mounted <- mountRecorded (program (H.delay 0.05 >> mark woke))
        advance clock 0.04
        pure mounted
      within "the rest of the pause, on the real clock" (takeMVar woke)
      dispose socket

    -- Handed to the real clock, a counted pause is waiting there, and no
    -- longer asleep: the program is not quiet until it ends. A minute, so
    -- that it surely still sleeps when read; the dispose ends it.
    it "counts a counted pause handed to the real clock as waiting, not asleep, and not quiet" $ do
      socket <- withTestClock $ \clock -> do
        (mounted, _) <- mountRecorded (program (void (H.fork (H.delayCounted 60))))
        waitTree mounted Activity.quiet
        treeActivity mounted >>= assertEqual "asleep on the test's clock" (Activity 1 0 1 1)
        advance clock 1
        pure mounted
      a <- treeActivity socket
      assertEqual "waiting on the real clock" (Activity 1 0 1 0) a
      assertEqual "not quiet" False (Activity.quiet a)
      dispose socket

    -- A tree keeps its clock after the bracket, but a pause it makes then
    -- sleeps on the real clock: the trace says so, it is not asleep, and
    -- the closed clock lists nothing.
    it "puts a counted pause made after the bracket on the real clock, and says so in the trace" $ do
      gate <- newEmptyMVar
      paused <- newEmptyMVar
      stop <- Trace.subscribe $ \event -> case event.what of
        Trace.CountedPause _ clocked -> putMVar paused clocked
        _ -> pure ()
      (socket, clock) <- withTestClock $ \clock -> do
        (mounted, _) <- mountRecorded (program (H.async (takeMVar gate) >> H.delayCounted 60))
        pure (mounted, clock)
      putMVar gate ()
      clocked <- within "the pause" (takeMVar paused)
      stop
      assertEqual "on the real clock, as the trace says" False clocked
      waitTree socket Activity.idle
      treeActivity socket >>= assertEqual "waiting, not asleep" (Activity 1 0 1 0)
      sleeping clock >>= assertEqual "nothing asleep on the closed clock" [] . map describeSleeping
      sleepingHere >>= assertEqual "no clock open here" [] . map describeSleeping
      dispose socket

  it "debounces a typed value on the test's clock, then holds its request until the test releases it" $ do
    looked <- newIORef []
    typed <- HS.create
    (_, took) <- timed $ withHeldWaits $ \waits -> withTestClock $ \clock ->
      withTree (search typed.emitter looked) $ \ui renders -> do
        traverse_ (HS.notify typed.listener) ["a", "ab", "abc"]
        pendingOf waits >>= assertEqual "nothing held: the debounce sleeps" []
        advance clock 0.3
        [lookUp] <- pendingOf waits
        assertEqual "the search's request" "search" lookUp.tag.label
        readIORef looked >>= assertEqual "held, not run" []
        release lookUp
        pendingOf waits >>= assertEqual "nothing held" []
        readIORef looked >>= assertEqual "looked up" ["abc"]
        lastShown renders >>= assertEqual "and shown" "found abc"
        treeActivity ui >>= assertEqual "quiet" (Activity 1 0 0 0)
    assertWith ("no timer anywhere: " <> show took) (took < 0.2)
