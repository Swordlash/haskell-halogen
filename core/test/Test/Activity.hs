-- | "Halogen.Activity": what it reads of the mounted trees, on a renderer
-- with no DOM, so on every backend. Other specs of the suite may leave
-- trees mounted, so each test compares with a reading taken at its start
-- rather than with absolute numbers; this spec runs first, where nothing
-- else has a fiber waiting, so the waits are tested against a quiet
-- program.
module Test.Activity (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (ErrorCall (..), SomeException, bracket, throwIO, try)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.IORef
import Data.Kind (Type)
import Data.Row (Empty, Row)
import Data.Time (NominalDiffTime)
import Data.Void (Void)
import Halogen qualified as H
import Halogen.Activity (Activity (..))
import Halogen.Activity qualified as Activity
import Halogen.Component (ComponentSlot (..))
import Halogen.HTML qualified as HH
import Halogen.HTML.Core qualified as HC
import Halogen.IO.Driver qualified as AD
import Halogen.Query.Input (Input)
import Halogen.VDom.Types (VDom (..), runGraft)
import Prelude
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)

data Rendered (s :: Type) (act :: Type) (ps :: Row Type) (o :: Type) = Rendered

-- | A renderer that renders nothing but the children, and fails when told.
nowhere :: IORef Bool -> AD.RenderSpec IO Rendered
nowhere failing =
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
      fails <- readIORef failing
      if fails
        then throwIO (ErrorCall "the render fails")
        else do
          traverse_ (\case ComponentSlot box -> void (renderChild box); ThunkSlot _ -> pure ()) (slots (HC.unHTML html))
          pure Rendered
    slots :: VDom p w -> [w]
    slots = \case
      Text _ -> []
      Elem _ _ _ cs -> foldMap slots cs
      Keyed _ _ _ cs -> foldMap (slots . snd) cs
      Widget w -> [w]
      Grafted g -> slots (runGraft g)

-- | What a component does in its @Initialize@: note what the activity was
-- in its synchronous part, then wait in 'liftIO' for the gate, if given.
data Setup = Setup
  { seen :: IORef (Maybe Activity)
  , gate :: Maybe (MVar (), MVar ())
  -- ^ Signalled once the wait has started; then waited for.
  }

waiter :: Setup -> H.Component H.VoidF () Void IO
waiter setup =
  H.component
    (\() -> pure ())
    (\() -> HH.text "" :: H.ComponentHTML () Empty IO)
    [ H.onInitialize $ do
        H.liftEffect (Activity.activity >>= writeIORef setup.seen . Just)
        traverse_ (\(started, release) -> liftIO (putMVar started () >> takeMVar release)) setup.gate
    ]

mountWith :: IORef Bool -> Setup -> IO (AD.HalogenSocket H.VoidF Void IO)
mountWith failing setup = AD.runUI (nowhere failing) (waiter setup) ()

dispose :: AD.HalogenSocket q o IO -> IO ()
dispose AD.HalogenSocket {AD.dispose = d} = d

-- | The socket's reading of its own tree.
treeActivity :: AD.HalogenSocket q o IO -> IO Activity
treeActivity AD.HalogenSocket {AD.activity = a} = a

within :: String -> IO a -> IO a
within what io = timeout 5_000_000 io >>= maybe (throwIO (ErrorCall ("timed out: " <> what))) pure

-- | A component whose @Initialize@ is this program, for the pauses.
initializing :: H.HalogenM () () Empty Void IO () -> H.Component H.VoidF () Void IO
initializing program =
  H.component
    (\() -> pure ())
    (\() -> HH.text "" :: H.ComponentHTML () Empty IO)
    [H.onInitialize program]

mountProgram :: H.HalogenM () () Empty Void IO () -> IO (AD.HalogenSocket H.VoidF Void IO)
mountProgram program = do
  failing <- newIORef False
  AD.runUI (nowhere failing) (initializing program) ()

-- | Mount the program for the body, and take its tree away afterwards
-- whatever the body did. A test that failed with its tree still mounted
-- would leave its fiber counted, and every reading after it wrong: the
-- examples that follow would fail for a reason that is not theirs.
withProgram :: H.HalogenM () () Empty Void IO () -> (AD.HalogenSocket H.VoidF Void IO -> IO a) -> IO a
withProgram program = bracket (mountProgram program) dispose

-- | Note that the program got here, from the loop.
mark :: MVar () -> H.HalogenM () () Empty Void IO ()
mark var = H.liftEffect (void (tryPutMVar var ()))

-- | Long enough for a test to act while the pause sleeps, even on a busy
-- machine: every check that something did not happen waits twice as long.
pause :: NominalDiffTime
pause = 0.3

-- | Whether the program got there within twice the pause.
markedWithin :: MVar () -> IO Bool
markedWithin var = maybe False (const True) <$> timeout 600_000 (readMVar var)

-- | The reading, less the one taken before.
since :: Activity -> Activity -> (Int, Int, Int)
since before now = (now.trees - before.trees, now.running - before.running, now.waiting - before.waiting)

spec :: Spec
spec = describe "Halogen.Activity" $ do
  it "counts nothing before anything is mounted, and is idle and quiet then" $ do
    -- The first spec of the suite: nothing is mounted yet.
    a <- Activity.activity
    assertEqual "nothing mounted" (Activity {trees = 0, running = 0, waiting = 0}) a
    assertWith "idle" (Activity.idle a)
    assertWith "quiet" (Activity.quiet a)

  it "counts a mounted tree, idle once mounted, and running while it initializes" $ do
    failing <- newIORef False
    seen <- newIORef Nothing
    before <- Activity.activity
    ui <- mountWith failing Setup {seen, gate = Nothing}
    after <- Activity.activity
    assertEqual "one more tree, idle" (1, 0, 0) (since before after)
    assertWith "idle" (Activity.idle after)
    -- Inside the first piece of work the tree is counted and running.
    readIORef seen >>= \case
      Nothing -> throwIO (ErrorCall "the initializer did not run")
      Just inside -> assertEqual "inside its Initialize" (1, 1, 0) (since before inside)
    dispose ui
    Activity.activity >>= assertEqual "gone once disposed" (0, 0, 0) . since before

  it "counts a fiber waiting in liftIO as waiting, not running, until it comes back" $ do
    failing <- newIORef False
    seen <- newIORef Nothing
    started <- newEmptyMVar
    release <- newEmptyMVar
    before <- Activity.activity
    ui <- mountWith failing Setup {seen, gate = Just (started, release)}
    within "the wait to start" (takeMVar started)
    waiting <- Activity.activity
    assertEqual "waiting" (1, 0, 1) (since before waiting)
    assertWith "idle while it waits" (Activity.idle waiting)
    assertWith "not quiet while it waits" (not (Activity.quiet waiting))
    putMVar release ()
    within "quiet again" Activity.waitQuiet
    Activity.activity >>= assertEqual "quiet again" (1, 0, 0) . since before
    dispose ui

  it "counts a fiber as waiting from the moment its mount returns, before its worker has run" $ do
    -- No handshake with the worker here: the fiber is counted by the piece
    -- of work that suspends it, so the reading right after the mount
    -- cannot be a false "quiet", whether or not the worker's thread has
    -- started yet.
    failing <- newIORef False
    seen <- newIORef Nothing
    started <- newEmptyMVar
    release <- newEmptyMVar
    before <- Activity.activity
    ui <- mountWith failing Setup {seen, gate = Just (started, release)}
    Activity.activity >>= assertEqual "waiting at once" (1, 0, 1) . since before
    putMVar release ()
    within "quiet again" Activity.waitQuiet
    Activity.activity >>= assertEqual "quiet again" (1, 0, 0) . since before
    dispose ui

  it "counts two trees, and each dispose takes one out, twice doing nothing more" $ do
    failing <- newIORef False
    seen <- newIORef Nothing
    before <- Activity.activity
    one <- mountWith failing Setup {seen, gate = Nothing}
    two <- mountWith failing Setup {seen, gate = Nothing}
    Activity.activity >>= assertEqual "two trees" (2, 0, 0) . since before
    dispose one
    Activity.activity >>= assertEqual "one left" (1, 0, 0) . since before
    dispose one
    Activity.activity >>= assertEqual "disposing twice" (1, 0, 0) . since before
    dispose two
    Activity.activity >>= assertEqual "none left" (0, 0, 0) . since before

  it "does not count a tree whose mount fails" $ do
    failing <- newIORef True
    seen <- newIORef Nothing
    before <- Activity.activity
    result <- try (mountWith failing Setup {seen, gate = Nothing})
    case result of
      Left (_ :: SomeException) -> pure ()
      Right _ -> throwIO (ErrorCall "the mount did not fail")
    Activity.activity >>= assertEqual "nothing counted" (0, 0, 0) . since before

  it "has waitIdle return at once while a fiber waits, and waitQuiet only once it is back" $ do
    failing <- newIORef False
    seen <- newIORef Nothing
    started <- newEmptyMVar
    release <- newEmptyMVar
    ui <- mountWith failing Setup {seen, gate = Just (started, release)}
    within "the wait to start" (takeMVar started)
    within "idle" Activity.waitIdle
    quietEarly <- timeout 50_000 Activity.waitQuiet
    assertEqual "not quiet while the fiber waits" Nothing quietEarly
    returned <- newEmptyMVar
    _ <- forkIO (Activity.waitQuiet >> putMVar returned ())
    threadDelay 10_000
    tryReadMVar returned >>= assertEqual "still waiting" Nothing
    putMVar release ()
    within "waitQuiet to return" (takeMVar returned)
    dispose ui

  describe "one tree's reading (the socket's activity)" $ do
    it "reads its own tree alone: a busy tree beside an idle one, each read separately" $ do
      failing <- newIORef False
      seen <- newIORef Nothing
      started <- newEmptyMVar
      release <- newEmptyMVar
      before <- Activity.activity
      busy <- mountWith failing Setup {seen, gate = Just (started, release)}
      calm <- mountWith failing Setup {seen, gate = Nothing}
      within "the wait to start" (takeMVar started)
      treeActivity busy >>= assertEqual "the waiting tree" Activity {trees = 1, running = 0, waiting = 1}
      treeActivity calm >>= assertEqual "the idle tree" Activity {trees = 1, running = 0, waiting = 0}
      -- The process's reading sums them.
      Activity.activity >>= assertEqual "both, read together" (2, 0, 1) . since before
      putMVar release ()
      within "quiet again" Activity.waitQuiet
      treeActivity busy >>= assertEqual "the tree that waited, now quiet" Activity {trees = 1, running = 0, waiting = 0}
      dispose busy
      dispose calm

    it "reads another tree as idle from inside this tree's work, which the process reads as running" $ do
      inside <- newEmptyMVar
      other <- mountProgram (pure ())
      let program = H.liftEffect ((,) <$> treeActivity other <*> Activity.activity >>= putMVar inside)
      -- The other tree's reading, taken from inside this tree's Initialize:
      -- this tree's loop is busy, the other's is not.
      withProgram program $ \_ -> do
        (theOther, everything) <- within "the reading" (takeMVar inside)
        assertEqual "the other tree, idle" Activity {trees = 1, running = 0, waiting = 0} theOther
        assertWith "the process, busy" (not (Activity.idle everything))
      dispose other

    it "counts its tree as running while the loop is held" $ do
      go <- newEmptyMVar
      holding <- newEmptyMVar
      letGo <- newEmptyMVar
      -- After a wait, an effect that holds the loop until the test lets it.
      let program = liftIO (takeMVar go) >> H.liftEffect (putMVar holding () >> takeMVar letGo)
      withProgram program $ \ui -> do
        treeActivity ui >>= assertEqual "waiting" Activity {trees = 1, running = 0, waiting = 1}
        putMVar go ()
        within "the effect" (takeMVar holding)
        treeActivity ui >>= assertEqual "running" Activity {trees = 1, running = 1, waiting = 0}
        putMVar letGo ()
        within "quiet again" Activity.waitQuiet
        treeActivity ui >>= assertEqual "quiet" Activity {trees = 1, running = 0, waiting = 0}

    it "counts no tree once it is disposed of, and a pause as neither running nor waiting" $ do
      woke <- newEmptyMVar
      ui <- mountProgram (H.delay 60 >> mark woke)
      treeActivity ui >>= assertEqual "asleep" Activity {trees = 1, running = 0, waiting = 0}
      dispose ui
      treeActivity ui >>= assertEqual "gone" Activity {trees = 0, running = 0, waiting = 0}
      dispose ui
      treeActivity ui >>= assertEqual "gone, disposed of twice" Activity {trees = 0, running = 0, waiting = 0}

  describe "a pause (H.delay)" $ do
    it "counts a fiber in a pause as neither running nor waiting, and waitQuiet returns while it sleeps" $ do
      woke <- newEmptyMVar
      before <- Activity.activity
      -- A pause far longer than the test, so that it is surely still asleep
      -- while the test reads; disposing of the tree ends it.
      withProgram (H.delay 60 >> mark woke) $ \ui -> do
        Activity.activity >>= assertEqual "neither running nor waiting" (1, 0, 0) . since before
        quietWhileAsleep <- timeout 1_000_000 Activity.waitQuiet
        assertEqual "waitQuiet returned while it sleeps" (Just ()) quietWhileAsleep
        tryReadMVar woke >>= assertEqual "still asleep" Nothing
        dispose ui
        Activity.activity >>= assertEqual "gone once disposed" (0, 0, 0) . since before

    it "counts the same fiber sleeping in liftIO (threadDelay n) as waiting" $ do
      woke <- newEmptyMVar
      before <- Activity.activity
      withProgram (liftIO (threadDelay 60_000_000) >> mark woke) $ \_ -> do
        Activity.activity >>= assertEqual "waiting" (1, 0, 1) . since before
        quietEarly <- timeout 50_000 Activity.waitQuiet
        assertEqual "not quiet while it sleeps" Nothing quietEarly

    it "wakes, and runs what follows the pause, which is counted as any work is" $ do
      woke <- newEmptyMVar
      released <- newEmptyMVar
      before <- Activity.activity
      -- After the pause the program waits in liftIO: from then on it is
      -- waiting again, so waitQuiet does not return until it is released.
      withProgram (H.delay 0.05 >> mark woke >> liftIO (takeMVar released)) $ \_ -> do
        within "the pause to end" (readMVar woke)
        -- The mark is made on the loop, a step before the wait: the loop is
        -- still running the fiber when the mark is seen, and the fiber is
        -- waiting only once the loop has let it go.
        within "the loop to reach the wait" Activity.waitIdle
        Activity.activity >>= assertEqual "waiting after the pause" (1, 0, 1) . since before
        putMVar released ()
        within "quiet again" Activity.waitQuiet
        Activity.activity >>= assertEqual "quiet again" (1, 0, 0) . since before

    it "does not wake a killed fork's pause" $ do
      woke <- newEmptyMVar
      killed <- newEmptyMVar
      let program = do
            fid <- H.fork (H.delay pause >> mark woke)
            -- Killed while it sleeps: the fork has suspended before 'fork'
            -- returns.
            H.kill fid
            mark killed
      withProgram program $ \_ -> do
        within "the kill" (readMVar killed)
        markedWithin woke >>= assertEqual "the killed fork never woke" False

    it "does not wake the pause of a component that went away" $ do
      woke <- newEmptyMVar
      withProgram (void (H.fork (H.delay pause >> mark woke))) $ \ui -> do
        dispose ui
        markedWithin woke >>= assertEqual "the pause never woke" False

    it "leaves a program whose ticker pauses between ticks quiet between them" $ do
      ticks <- newIORef (0 :: Int)
      let tick = H.liftEffect (atomicModifyIORef' ticks (\n -> (n + 1, ())))
          ticked n = do
            t <- readIORef ticks
            if t >= n then pure () else threadDelay 1_000 >> ticked n
      before <- Activity.activity
      withProgram (void (H.fork (forever (H.delay 0.01 >> tick)))) $ \ui -> do
        within "three ticks" (ticked 3)
        -- Quiet between the ticks, again and again, while the ticker runs on:
        -- waitQuiet returns each time, and finds no fiber waiting.
        sequence_ $ replicate 5 $ do
          within "quiet between ticks" Activity.waitQuiet
          n <- readIORef ticks
          within "the next tick" (ticked (n + 1))
        Activity.activity >>= \a -> assertEqual "nothing waiting" 0 (a.waiting - before.waiting)
        dispose ui
        stopped <- readIORef ticks
        threadDelay 100_000
        readIORef ticks >>= assertEqual "no tick after the tree went" stopped
