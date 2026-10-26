-- | What the driver promises about when a component's code runs, tested
-- with handshakes rather than timing (see "Halogen.IO.Driver.Runtime").
--
-- * A tree's work runs one piece at a time: what arrives while it is busy
--   waits, in the order it came, and runs before the busy thread lets go.
-- * A program waits only in 'liftIO' (a suspension); meanwhile the rest of
--   the tree goes on, and the program resumes after what came before.
-- * A child's 'Receive' runs as a program of the child: until it waits, the
--   render waits for it; once it waits, the render goes on.
-- * A state change is rendered before the program goes on, and a state
--   operation that changes nothing renders nothing, in a component that
--   'H.hoist' moved to another monad too.
-- * A component counts as initialized once its @Initialize@ has run its
--   synchronous part (up to its first wait, or its end): its parent's runs
--   after that, and its outputs flow, while what it waits for holds up
--   nothing else (not the program whose render made it, nor 'runUI').
-- * A component that goes away takes its programs with it: none touches it
--   again, a query waiting on it answers 'Nothing', and what its finalizer
--   forked ends with the finalizer.
-- * A program stops the moment it is cancelled, by its own work too.
-- * Parallel branches end together: a failing one takes the others with it.
-- * A render pass that fails takes back the children it made, up to its
--   commit.
-- * Work the tree's own thread brings it (an emitter a synchronous effect
--   notifies) runs there and then.
-- * So does an event a synchronous effect dispatches, through a callback
--   the page calls (an emitter fed by a DOM listener): it is handled before
--   the effect goes on, on every backend.
-- * Work a program's worker brings it (an emitter a 'liftIO' notifies) is
--   not run by that worker, which may be killed by it; nor is it run inside
--   a synchronous effect under way, and neither is work any other thread
--   brings: both wait for the effect, and the action it is in, to end.
-- * A failing program ends alone: the rest of the tree goes on.
module Test.DriverContract (spec) where

import Control.Concurrent (forkIO, rtsSupportsBoundThreads, setNumCapabilities, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (ErrorCall (..), SomeException, finally, onException, throwIO)
import Control.Monad (forever, void, when)
import Control.Monad.Catch (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Parallel (parallel, sequential)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State.Class (get, modify, put, state)
import Data.Foldable (for_, traverse_)
import Data.IORef
import Data.Kind (Type)
import Data.NT (type (~>) (..))
import Data.Row (Empty, Row, type (.==))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Halogen as H
import Halogen.Component (ComponentSlot (..))
import Halogen.HTML qualified as HH
import Halogen.HTML.Core qualified as HC
import Halogen.IO.Driver qualified as AD
import Halogen.IO.Driver.State (RenderStateX (..))
import Halogen.JSBits (freeCallback, invokeCallback, jsNull, mkSyncCallback)
import Halogen.Query.Input (Input)
import Halogen.Subscription qualified as HS
import Halogen.VDom.Types (VDom (..), runGraft)
import Prelude
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual)

----------------------------------------------------------------------
-- A render spec with no DOM: it renders every slot and records the text.

data Rendered (s :: Type) (act :: Type) (ps :: Row Type) (o :: Type) = Rendered

walking :: IORef [Text] -> AD.RenderSpec IO Rendered
walking texts =
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
      -> (ComponentSlotBox ps IO act -> IO (RenderStateX Rendered))
      -> HC.HTML (ComponentSlot ps IO act) act
      -> Maybe (Rendered s act ps o)
      -> IO (Rendered s act ps o)
    render _ renderChild html _ = do
      let (slots, text) = walk (HC.unHTML html)
      traverse_ (\case ComponentSlot box -> void (renderChild box); ThunkSlot _ -> pure ()) slots
      modifyIORef' texts (<> [mconcat text])
      pure Rendered

walk :: VDom p w -> ([w], [Text])
walk = \case
  Text t -> ([], [t])
  Elem _ _ _ cs -> foldMap walk cs
  Keyed _ _ _ cs -> foldMap (walk . snd) cs
  Widget w -> ([w], [])
  Grafted g -> walk (runGraft g)

-- | A render spec that counts what is on the page: each rendering has a
-- number, live until it is removed. It refuses to remove the one it is told
-- to.
data Tracked (s :: Type) (act :: Type) (ps :: Row Type) (o :: Type) = Tracked Int

tracking :: IORef Int -> IORef [Int] -> IORef (Maybe Int) -> AD.RenderSpec IO Tracked
tracking fresh live refuse =
  AD.RenderSpec
    { AD.render = render
    , AD.renderChild = id
    , AD.removeChild = remove
    , AD.dispose = remove
    , AD.sameElement = \_ _ -> pure False
    , AD.rootNode = \_ -> pure Nothing
    }
  where
    render
      :: (Input act -> IO ())
      -> (ComponentSlotBox ps IO act -> IO (RenderStateX Tracked))
      -> HC.HTML (ComponentSlot ps IO act) act
      -> Maybe (Tracked s act ps o)
      -> IO (Tracked s act ps o)
    render _ renderChild html old = do
      traverse_ (\case ComponentSlot box -> void (renderChild box); ThunkSlot _ -> pure ()) (fst (walk (HC.unHTML html)))
      case old of
        Just r -> pure r
        Nothing -> do
          n <- atomicModifyIORef' fresh (\i -> (i + 1, i))
          modifyIORef' live (<> [n])
          pure (Tracked n)
    remove :: Tracked s act ps o -> IO ()
    remove (Tracked n) = do
      refused <- readIORef refuse
      when (refused == Just n) $ throwIO (ErrorCall "cannot remove")
      modifyIORef' live (filter (/= n))

ask :: AD.HalogenSocket q o IO -> q a -> IO (Maybe a)
ask AD.HalogenSocket {AD.query = q} = q

dispose :: AD.HalogenSocket q o IO -> IO ()
dispose AD.HalogenSocket {AD.dispose = d} = d

-- | Waits have a bound: a broken promise fails the test instead of hanging it.
within :: String -> IO a -> IO a
within what io = timeout 5_000_000 io >>= maybe (throwIO (ErrorCall ("timed out: " <> what))) pure

onCapabilities :: IO ()
onCapabilities = when rtsSupportsBoundThreads (setNumCapabilities 4)

----------------------------------------------------------------------
-- A component that logs, driven by actions sent through an emitter.

data Act
  = Note Text
  | -- | Holds the tree: signals, then waits in 'liftEffect'.
    Hold (MVar ()) (MVar ())
  | -- | Waits in 'liftIO' for the first, then signals the second.
    Slow (MVar ()) (MVar ())
  | -- | Waits in 'liftIO' for the second forever, signalling the first once
    -- it waits (a worker killed before it starts runs none of its handlers)
    -- and the third when killed.
    Stuck (MVar ()) (MVar ()) (MVar ())
  | Boom
  | -- | Forks a program that kills itself (and signals when it has).
    SelfKill (MVar ())
  | -- | Two branches that wait forever: each signals when it starts, and when
    -- it is killed.
    ParStuck (MVar ()) (MVar ()) (MVar ()) (MVar ())
  | -- | A branch that fails at once, and one that notes it started; goes on
    -- (past a wait) once it has caught the failure, and signals at the end.
    ParFailsAtOnce (MVar ())
  | -- | A branch that fails once let go, and one that waits (signalling when
    -- it waits, and when it is killed) and then changes the state; signals
    -- once it has caught the failure.
    ParFailsLater (MVar ()) (MVar ()) (MVar ()) (MVar ()) (MVar ())
  | Dispatch
  | -- | Forks a program that notifies 'Tick' from its 'liftIO'. The third
    -- 'Tick' kills the fork, signals the first, and once it has gone on,
    -- the second.
    TickFrom (MVar ()) (MVar ())
  | Tick
  | -- | In an effect, calls a callback as the page calls a DOM listener,
    -- which notifies 'Note', and notes once the call has returned.
    CallBack
  | -- | In an effect, starts a thread of its own that notifies 'Note' and
    -- signals, waits for the signal, and notes.
    FromThread
  | -- | Forks a program whose 'liftIO' notifies 'Note' and signals; then, in
    -- an effect, waits for the signal and notes.
    WorkerInEffect

data Query a
  = Bump (Text -> a)
  | Current (Int -> a)
  | Hang (MVar ()) a

logging :: IORef [Text] -> IORef [Text] -> IORef (Maybe (ForkId, MVar (), MVar ())) -> HS.Emitter IO Act -> HS.Listener IO Act -> H.Component Query () Void IO
logging texts logRef ticker acts self =
  H.component
    (\_ -> pure (0 :: Int))
    (\n -> HH.text (T.pack (show n)) :: H.ComponentHTML Act Empty IO)
    [H.onAction handleAction, H.onQuery handleQuery, H.onInitialize (handleAction (Note "init"))]
  where
    note t = H.liftEffect (note' t)
    note' t = modifyIORef' logRef (<> [t])
    handleAction = \case
      Note "init" -> void (H.subscribe acts)
      Note t -> note t
      Hold entered release -> do
        H.liftEffect (putMVar entered () >> takeMVar release)
        note "held"
      Slow gate done -> do
        note "slow start"
        liftIO (takeMVar gate)
        note "slow end"
        H.liftEffect (putMVar done ())
      Stuck started gate killed -> do
        note "stuck"
        liftIO ((putMVar started () >> takeMVar gate) `onException` putMVar killed ())
        modify (+ 1)
        note "after"
      Boom -> H.liftEffect (throwIO (ErrorCall "boom"))
      SelfKill done -> do
        me <- H.liftEffect newEmptyMVar
        fid <- H.fork $ do
          fid <- liftIO (readMVar me)
          H.liftEffect (putMVar done ())
          H.kill fid
          note "went on after its kill"
          modify (+ 1)
          liftIO (note' "waited after its kill")
        H.liftEffect (putMVar me fid)
      ParStuck started1 started2 killed1 killed2 ->
        sequential $
          (\() () -> ())
            -- The handler goes up before the branch says it started: a kill
            -- right after the signal is still seen.
            <$> parallel (liftIO ((putMVar started1 () >> forever (threadDelay 1_000_000)) `onException` putMVar killed1 ()) :: H.HalogenM Int Act Empty Void IO ())
            <*> parallel (liftIO ((putMVar started2 () >> forever (threadDelay 1_000_000)) `onException` putMVar killed2 ()) :: H.HalogenM Int Act Empty Void IO ())
      ParFailsAtOnce done -> do
        result <-
          try
            $ sequential
            $ (,)
              <$> parallel (H.liftEffect (throwIO (ErrorCall "first")) :: H.HalogenM Int Act Empty Void IO ())
              <*> parallel (note "second started")
        for_ (either (\(_ :: SomeException) -> Just ()) (const Nothing) result) $ \() -> note "caught"
        -- Still running, so that a sibling let go would run too.
        liftIO (pure ())
        note "went on"
        H.liftEffect (putMVar done ())
      ParFailsLater gate1 started2 gate2 killed2 caught -> do
        result <-
          try
            $ sequential
            $ (,)
              <$> parallel (liftIO (takeMVar gate1 >> throwIO (ErrorCall "first")) :: H.HalogenM Int Act Empty Void IO ())
              <*> parallel (liftIO ((putMVar started2 () >> takeMVar gate2) `onException` putMVar killed2 ()) >> modify (+ 1) >> note "sibling went on")
        for_ (either (\(_ :: SomeException) -> Just ()) (const Nothing) result) $ \() -> note "caught"
        H.liftEffect (putMVar caught ())
      Dispatch -> do
        H.liftEffect (HS.notify self (Note "handled while dispatched"))
        note "dispatched"
      CallBack -> H.liftEffect $ do
        callback <- mkSyncCallback (\_ -> HS.notify self (Note "handled in the call"))
        invokeCallback callback jsNull
        freeCallback callback
        note' "the effect went on"
      FromThread -> H.liftEffect $ do
        notified <- newEmptyMVar
        _ <- forkIO (HS.notify self (Note "from a thread") >> putMVar notified ())
        takeMVar notified
        note' "the effect went on"
      WorkerInEffect -> do
        notified <- H.liftEffect newEmptyMVar
        _ <- H.fork (liftIO (HS.notify self (Note "from the worker") >> putMVar notified ()))
        H.liftEffect (takeMVar notified >> note' "the effect went on")
      TickFrom killed wentOn -> do
        fid <- H.fork $ forever $ do
          liftIO (threadDelay 1_000)
          -- Notified from the fork's worker: that thread must not run the
          -- loop, since the work it brings may kill the fork, and it with it.
          liftIO (HS.notify self Tick)
        H.liftEffect (writeIORef ticker (Just (fid, killed, wentOn)))
      Tick -> do
        n <- state (\k -> (k + 1, k + 1))
        when (n == 3) $
          H.liftEffect (readIORef ticker)
            >>= traverse_
              ( \(fid, killed, wentOn) -> do
                  H.kill fid
                  -- Were this handler running on the fork's worker, the kill would
                  -- land here, in its wait.
                  H.liftEffect (putMVar killed () >> threadDelay 20_000)
                  note "went on after the kill"
                  H.liftEffect (putMVar wentOn ())
              )
    handleQuery :: Query a -> H.HalogenM Int Act Empty Void IO (Maybe a)
    handleQuery = \case
      Bump k -> do
        modify (+ 1)
        -- What was rendered by now.
        rendered <- H.liftEffect (readIORef texts)
        pure (Just (k (last rendered)))
      Current k -> Just . k <$> get
      Hang started a -> do
        H.liftEffect (putMVar started ())
        liftIO (newEmptyMVar >>= takeMVar :: IO ())
        pure (Just a)

withLogging :: (HS.Listener IO Act -> AD.HalogenSocket Query Void IO -> IORef [Text] -> IORef [Text] -> IO ()) -> IO ()
withLogging body = do
  onCapabilities
  texts <- newIORef []
  logRef <- newIORef []
  ticker <- newIORef Nothing
  HS.Subscribe {HS.listener, HS.emitter} <- HS.create
  socket <- AD.runUI (walking texts) (logging texts logRef ticker emitter listener) ()
  body listener socket texts logRef

busyTree :: IO ()
busyTree = withLogging $ \acts _ _ logRef -> do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  returned <- newEmptyMVar
  _ <- forkIO $ HS.notify acts (Hold entered release) >> putMVar returned ()
  within "the hold" (takeMVar entered)
  -- The tree is busy: these wait, and the calls return at once.
  HS.notify acts (Note "a")
  HS.notify acts (Note "b")
  readIORef logRef >>= assertEqual "nothing ran meanwhile" []
  putMVar release ()
  -- The thread that held the tree runs what came meanwhile before it lets go.
  within "the holder" (takeMVar returned)
  readIORef logRef >>= assertEqual "in the order it came" ["held", "a", "b"]

idleTree :: IO ()
idleTree = withLogging $ \acts _ _ logRef -> do
  HS.notify acts (Note "x")
  readIORef logRef >>= assertEqual "handled before the call returns" ["x"]

suspension :: IO ()
suspension = withLogging $ \acts _ _ logRef -> do
  gate <- newEmptyMVar
  done <- newEmptyMVar
  HS.notify acts (Slow gate done)
  HS.notify acts (Note "fast")
  putMVar gate ()
  within "the slow action" (takeMVar done)
  readIORef logRef >>= assertEqual "the waiting action resumed after the other" ["slow start", "fast", "slow end"]

renderedFirst :: IO ()
renderedFirst = withLogging $ \_ socket _ _ -> do
  seen <- ask socket (Bump id)
  assertEqual "the new state was rendered before the program read it" (Just "1") seen

goneWhileWaiting :: IO ()
goneWhileWaiting = withLogging $ \acts socket texts logRef -> do
  started <- newEmptyMVar
  gate <- newEmptyMVar
  killed <- newEmptyMVar
  HS.notify acts (Stuck started gate killed)
  within "the wait" (takeMVar started)
  rendersBefore <- length <$> readIORef texts
  dispose socket
  within "the cancelled wait" (takeMVar killed)
  _ <- tryPutMVar gate ()
  readIORef logRef >>= assertEqual "the action never went on" ["stuck"]
  (length <$> readIORef texts) >>= assertEqual "and nothing rendered" rendersBefore
  ask socket (Current id) >>= assertEqual "a disposed tree answers nothing" Nothing

queryWhileGoing :: IO ()
queryWhileGoing = withLogging $ \_ socket _ _ -> do
  started <- newEmptyMVar
  answer <- newEmptyMVar
  _ <- forkIO $ ask socket (Hang started ()) >>= putMVar answer
  within "the query" (takeMVar started)
  dispose socket
  within "the answer" (takeMVar answer) >>= assertEqual "a query whose component went away answers Nothing" Nothing

failureAlone :: IO ()
failureAlone = withLogging $ \acts _ _ logRef -> do
  HS.notify acts Boom
  HS.notify acts (Note "after")
  readIORef logRef >>= assertEqual "the tree went on" ["after"]

selfKill :: IO ()
selfKill = withLogging $ \acts socket _ logRef -> do
  killed <- newEmptyMVar
  HS.notify acts (SelfKill killed)
  within "the kill" (takeMVar killed)
  -- Queued behind whatever the fork still ran.
  ask socket (Current id) >>= assertEqual "the state is as it was" (Just 0)
  threadDelay 10_000
  readIORef logRef >>= assertEqual "the fork ran nothing after its kill" []

parallelDisposed :: IO ()
parallelDisposed = withLogging $ \acts socket _ _ -> do
  [started1, started2, killed1, killed2] <- sequence [newEmptyMVar, newEmptyMVar, newEmptyMVar, newEmptyMVar]
  HS.notify acts (ParStuck started1 started2 killed1 killed2)
  within "the first branch" (takeMVar started1)
  within "the second branch" (takeMVar started2)
  dispose socket
  within "the first branch's worker to go" (takeMVar killed1)
  within "the second branch's worker to go" (takeMVar killed2)

parallelFailsAtOnce :: IO ()
parallelFailsAtOnce = withLogging $ \acts _ _ logRef -> do
  done <- newEmptyMVar
  HS.notify acts (ParFailsAtOnce done)
  within "the action" (takeMVar done)
  readIORef logRef >>= assertEqual "the second branch never started" ["caught", "went on"]

parallelFailsLater :: IO ()
parallelFailsLater = withLogging $ \acts socket _ logRef -> do
  [gate1, started2, gate2, killed2, caught] <- sequence [newEmptyMVar, newEmptyMVar, newEmptyMVar, newEmptyMVar, newEmptyMVar]
  HS.notify acts (ParFailsLater gate1 started2 gate2 killed2 caught)
  within "the sibling's wait" (takeMVar started2)
  putMVar gate1 ()
  within "the failure to be caught" (takeMVar caught)
  within "the sibling's worker to go" (takeMVar killed2)
  _ <- tryPutMVar gate2 ()
  threadDelay 10_000
  ask socket (Current id) >>= assertEqual "the sibling changed nothing" (Just 0)
  readIORef logRef >>= assertEqual "and did not go on" ["caught"]

workerNotify :: IO ()
workerNotify = withLogging $ \acts _ _ logRef -> do
  killed <- newEmptyMVar
  wentOn <- newEmptyMVar
  HS.notify acts (TickFrom killed wentOn)
  within "the kill" (takeMVar killed)
  within "the handler to go on" (takeMVar wentOn)
  readIORef logRef >>= assertEqual "the handler that killed the fork finished" ["went on after the kill"]

nestedDispatch :: IO ()
nestedDispatch = withLogging $ \acts _ _ logRef -> do
  HS.notify acts Dispatch
  readIORef logRef >>= assertEqual "handled before the effect returned" ["handled while dispatched", "dispatched"]

-- | A callback the page calls in the middle of an effect: the work it
-- brings is the event's handler, nested in the effect. Natively the call
-- runs on the loop's own thread; in a browser on a thread of its own,
-- which is a sync callback's.
callbackDispatch :: IO ()
callbackDispatch = withLogging $ \acts _ _ logRef -> do
  HS.notify acts CallBack
  readIORef logRef >>= assertEqual "handled before the effect went on" ["handled in the call", "the effect went on"]

-- | A thread that is neither the loop's nor one the page called queues its
-- work while an effect is under way, though the effect's thread is blocked
-- (as it would be in a browser's dispatch): it is not under the effect.
threadDuringEffect :: IO ()
threadDuringEffect = withLogging $ \acts _ _ logRef -> do
  HS.notify acts FromThread
  readIORef logRef >>= assertEqual "handled after the effect's action" ["the effect went on", "from a thread"]

-- | So does a worker, which never runs the loop.
workerDuringEffect :: IO ()
workerDuringEffect = withLogging $ \acts _ _ logRef -> do
  HS.notify acts WorkerInEffect
  readIORef logRef >>= assertEqual "handled after the effect's action" ["the effect went on", "from the worker"]

----------------------------------------------------------------------
-- A child that tells its parent to remove it, and would go on.

data PokeAct = Watch | Poke

poked :: IORef [Text] -> HS.Emitter IO () -> H.Component H.VoidF () () IO
poked logRef pokes =
  H.component
    (\_ -> pure ())
    (\_ -> HH.text "child" :: H.ComponentHTML PokeAct Empty IO)
    [H.onAction handleAction, H.onInitialize (handleAction Watch)]
  where
    handleAction = \case
      Watch -> void (H.subscribe (Poke <$ pokes))
      Poke -> do
        -- The parent removes this component before 'raise' returns.
        H.raise ()
        H.liftEffect (modifyIORef' logRef (<> ["went on"]))
        liftIO (modifyIORef' logRef (<> ["waited"]))

remover :: IORef [Text] -> HS.Emitter IO () -> H.Component Query () Void IO
remover logRef pokes =
  H.component
    (\_ -> pure (0 :: Int))
    (\n -> HH.div_ [HH.slot "child" () (poked logRef pokes) () (const ()) | n == 0] :: H.ComponentHTML () ("child" .== H.Slot H.VoidF () ()) IO)
    [ H.onAction (\() -> modify (+ 1))
    , H.onQuery
        ( \case
            Current k -> Just . k <$> get
            _ -> pure Nothing
        )
    ]

removedWhileRunning :: IO ()
removedWhileRunning = do
  onCapabilities
  texts <- newIORef []
  logRef <- newIORef []
  HS.Subscribe {HS.listener, HS.emitter} <- HS.create
  socket <- AD.runUI (walking texts) (remover logRef emitter) ()
  HS.notify listener ()
  ask socket (Current id) >>= assertEqual "the child was removed" (Just 1)
  threadDelay 10_000
  readIORef logRef >>= assertEqual "the removed child ran nothing more" []

----------------------------------------------------------------------
-- A child whose Receive asks its parent and waits for the answer.

data ChildAct = Received Int

asker :: MVar () -> H.Component H.VoidF Int (MVar ()) IO
asker childDone =
  H.component
    pure
    (\_ -> HH.text "child" :: H.ComponentHTML ChildAct Empty IO)
    [H.onAction handleAction, H.onReceive (handleAction . Received)]
  where
    handleAction (Received n) = when (n > 0) $ do
      reply <- H.liftEffect newEmptyMVar
      H.raise reply
      liftIO (takeMVar reply)
      H.liftEffect (putMVar childDone ())

data ParentAct = Answer (MVar ())

data SetQuery a = Set Int a

askedParent :: MVar () -> H.Component SetQuery () Void IO
askedParent childDone =
  H.component
    (\_ -> pure (0 :: Int))
    (\n -> HH.div_ [HH.slot "child" () (asker childDone) n Answer] :: H.ComponentHTML ParentAct ("child" .== H.Slot H.VoidF (MVar ()) ()) IO)
    [ H.onAction (\(Answer reply) -> H.liftEffect (putMVar reply ()))
    , H.onQuery (\(Set n a) -> put n >> pure (Just a))
    ]

receiveAsksParent :: IO ()
receiveAsksParent = do
  onCapabilities
  texts <- newIORef []
  childDone <- newEmptyMVar
  socket <- AD.runUI (walking texts) (askedParent childDone) ()
  within "the render" (ask socket (H.mkTell (Set 1))) >>= assertEqual "the render finished" (Just ())
  within "the child's answer" (takeMVar childDone)

----------------------------------------------------------------------
-- Children counted in and out, made by a render that can fail.

data Counts = Counts {started :: IORef Int, stopped :: IORef Int, parentStarted :: IORef Int}

data Life = Start | Stop

counted :: Counts -> H.Component H.VoidF Int Void IO
counted counts =
  H.component
    (\k -> if k == 3 then throwIO (ErrorCall "no child 3") else pure k)
    (\_ -> HH.text "" :: H.ComponentHTML Life Empty IO)
    [H.onAction handleAction, H.onInitialize (handleAction Start), H.onFinalize (handleAction Stop)]
  where
    handleAction = \case
      Start -> H.liftEffect $ modifyIORef' counts.started (+ 1)
      Stop -> H.liftEffect $ modifyIORef' counts.stopped (+ 1)

rows :: Counts -> H.Component SetQuery () Void IO
rows counts =
  H.component
    (\_ -> pure (1 :: Int))
    (\n -> HH.div_ [HH.slot_ "row" k (counted counts) k | k <- [0 .. n]] :: H.ComponentHTML () ("row" .== H.Slot H.VoidF Void Int) IO)
    [ H.onQuery (\(Set n a) -> put n >> pure (Just a))
    , H.onInitialize (H.liftEffect $ modifyIORef' counts.parentStarted (+ 1))
    ]

failedPass :: IO ()
failedPass = do
  onCapabilities
  texts <- newIORef []
  counts <- Counts <$> newIORef 0 <*> newIORef 0 <*> newIORef 0
  socket <- AD.runUI (walking texts) (rows counts) ()
  readIORef counts.started >>= assertEqual "two rows" 2
  failed <- try (ask socket (H.mkTell (Set 3)))
  assertEqual "the render failed" True (either (\(_ :: SomeException) -> True) (const False) failed)
  -- Row 2 was made by the failed pass, and taken back without running.
  readIORef counts.started >>= assertEqual "no row started by the failed pass" 2
  void $ ask socket (H.mkTell (Set 2))
  readIORef counts.started >>= assertEqual "row 2 made once" 3
  readIORef counts.stopped >>= assertEqual "and none stopped" 0
  dispose socket
  readIORef counts.stopped >>= assertEqual "all stopped with the tree" 3

-- | One row, whose key is the state.
swapper :: Counts -> H.Component SetQuery () Void IO
swapper counts =
  H.component
    (\_ -> pure (0 :: Int))
    (\k -> HH.div_ [HH.slot_ "row" k (counted counts) k] :: H.ComponentHTML () ("row" .== H.Slot H.VoidF Void Int) IO)
    [H.onQuery (\(Set n a) -> put n >> pure (Just a))]

failedRemoval :: IO ()
failedRemoval = do
  onCapabilities
  fresh <- newIORef 0
  live <- newIORef []
  refuse <- newIORef Nothing
  counts <- Counts <$> newIORef 0 <*> newIORef 0 <*> newIORef 0
  socket <- AD.runUI (tracking fresh live refuse) (swapper counts) ()
  -- The row renders before the component around it.
  readIORef live >>= assertEqual "the row and its parent" [0, 1]
  writeIORef refuse (Just 0)
  -- Row 1 is made, then the old row cannot be taken off the page.
  failed <- try (ask socket (H.mkTell (Set 1)))
  assertEqual "the render failed" True (either (\(_ :: SomeException) -> True) (const False) failed)
  readIORef live >>= assertEqual "the row the failed pass made is gone" [0, 1]
  readIORef counts.started >>= assertEqual "and was never initialized" 1
  readIORef counts.stopped >>= assertEqual "nor finalized" 0
  writeIORef refuse Nothing
  -- Another key: the failed update did change the state, and on the
  -- JavaScript backend a 1 put again is the same value, not a new one.
  void $ ask socket (H.mkTell (Set 2))
  readIORef live >>= assertEqual "the new row replaced the old" [1, 3]
  readIORef counts.started >>= assertEqual "the new row started" 2
  readIORef counts.stopped >>= assertEqual "the old row stopped" 1
  dispose socket
  readIORef counts.stopped >>= assertEqual "all stopped with the tree" 2

----------------------------------------------------------------------
-- A failing initializer, a finalizer that forks.

data InitAct = Init | Fin

initFails :: IORef [Text] -> Bool -> H.Component H.VoidF () Void IO
initFails logRef bad =
  H.component
    (\_ -> pure ())
    (\_ -> HH.text "" :: H.ComponentHTML InitAct Empty IO)
    [H.onAction handleAction, H.onInitialize (handleAction Init)]
  where
    handleAction _ = H.liftEffect $ if bad then throwIO (ErrorCall "init") else modifyIORef' logRef (<> ["good child"])

initParent :: IORef [Text] -> MVar () -> H.Component H.VoidF () Void IO
initParent logRef forkKilled =
  H.component
    (\_ -> pure ())
    (\_ -> HH.div_ [HH.slot_ "c" False (initFails logRef False) (), HH.slot_ "c" True (initFails logRef True) ()] :: H.ComponentHTML InitAct ("c" .== H.Slot H.VoidF Void Bool) IO)
    [H.onAction handleAction, H.onInitialize (handleAction Init), H.onFinalize (handleAction Fin)]
  where
    handleAction = \case
      Init -> H.liftEffect $ modifyIORef' logRef (<> ["parent"])
      -- Forks work that outlives the finalizer: it ends with it.
      Fin -> do
        forkWaits <- H.liftEffect newEmptyMVar
        void $ H.fork $ liftIO $ (putMVar forkWaits () >> (newEmptyMVar >>= takeMVar :: IO ())) `onException` putMVar forkKilled ()
        liftIO (readMVar forkWaits)

lifecycle :: IO ()
lifecycle = do
  onCapabilities
  texts <- newIORef []
  logRef <- newIORef []
  forkKilled <- newEmptyMVar
  socket <- AD.runUI (walking texts) (initParent logRef forkKilled) ()
  readIORef logRef >>= assertEqual "a failing initializer held up no other" ["good child", "parent"]
  dispose socket
  within "the finalizer's fork" (takeMVar forkKilled)

----------------------------------------------------------------------
-- A child whose Initialize waits, under a parent whose does not.

-- | Where the island's Initialize waits, and what it signals: once it has
-- loaded, or once its wait was cancelled.
data Gate = Gate {open :: MVar (), loaded :: MVar (), cancelled :: MVar ()}

data IslandAct = Load | Poked

-- | An island: its Initialize subscribes to the pokes, then waits at the
-- gate (in 'liftIO'), and its state says whether it has loaded. It raises
-- each poke to its parent, while it waits too.
island :: IORef [Text] -> Gate -> HS.Emitter IO () -> H.Component Query () Text IO
island logRef gate pokes =
  H.component
    (\_ -> pure (0 :: Int))
    (\_ -> HH.text "island" :: H.ComponentHTML IslandAct Empty IO)
    [H.onAction handleAction, H.onQuery handleQuery, H.onInitialize (handleAction Load)]
  where
    note t = H.liftEffect (modifyIORef' logRef (<> [t]))
    handleAction = \case
      Load -> do
        void (H.subscribe (Poked <$ pokes))
        note "island waits"
        liftIO (readMVar gate.open `onException` tryPutMVar gate.cancelled ())
        put 1
        note "island loaded"
        H.liftEffect (putMVar gate.loaded ())
      Poked -> H.raise "poked"
    handleQuery :: Query a -> H.HalogenM Int IslandAct Empty Text IO (Maybe a)
    handleQuery = \case
      Current k -> Just . k <$> get
      _ -> pure Nothing

data HostAct = HostInit | Heard Text

data HostQuery a = Reveal a | Conceal a | AskIsland (Maybe Int -> a)

-- | The island's parent: it shows the island while its state says so.
host :: IORef [Text] -> Gate -> HS.Emitter IO () -> Bool -> H.Component HostQuery () Void IO
host logRef gate pokes shown =
  H.component
    (\_ -> pure shown)
    ( \visible ->
        HH.div_ [HH.slot "island" () (island logRef gate pokes) () Heard | visible]
          :: H.ComponentHTML HostAct ("island" .== H.Slot Query Text ()) IO
    )
    [H.onAction handleAction, H.onQuery handleQuery, H.onInitialize (handleAction HostInit)]
  where
    note t = H.liftEffect (modifyIORef' logRef (<> [t]))
    handleAction = \case
      HostInit -> note "host initialized"
      Heard t -> note ("host heard " <> t)
    handleQuery :: HostQuery a -> H.HalogenM Bool HostAct ("island" .== H.Slot Query Text ()) Void IO (Maybe a)
    handleQuery = \case
      Reveal a -> put True >> note "revealed" >> pure (Just a)
      Conceal a -> put False >> pure (Just a)
      AskIsland k -> Just . k <$> H.request "island" () Current

-- | The host, mounted (within the bound: a mount that waited for the
-- island would never return), showing the island or not. The gate is
-- opened afterwards, so that nothing is left waiting.
withIsland :: Bool -> (AD.HalogenSocket HostQuery Void IO -> IORef [Text] -> Gate -> HS.Listener IO () -> IO ()) -> IO ()
withIsland shown body = do
  onCapabilities
  texts <- newIORef []
  logRef <- newIORef []
  gate <- Gate <$> newEmptyMVar <*> newEmptyMVar <*> newEmptyMVar
  HS.Subscribe {HS.listener, HS.emitter} <- HS.create
  socket <- within "the mount" (AD.runUI (walking texts) (host logRef gate emitter shown) ())
  body socket logRef gate listener `finally` (tryPutMVar gate.open () >> dispose socket)

initializedAtFirstWait :: IO ()
initializedAtFirstWait = withIsland True $ \socket logRef gate _ -> do
  readIORef logRef >>= assertEqual "the host was initialized once the island's Initialize waited, and runUI returned" ["island waits", "host initialized"]
  putMVar gate.open ()
  within "the island to load" (takeMVar gate.loaded)
  within "the answer" (ask socket (H.mkRequest AskIsland)) >>= assertEqual "the island loaded afterwards" (Just (Just 1))
  readIORef logRef >>= assertEqual "in that order" ["island waits", "host initialized", "island loaded"]

renderGoesOn :: IO ()
renderGoesOn = withIsland False $ \socket logRef _ _ -> do
  within "the query that renders the island" (ask socket (H.mkTell Reveal)) >>= assertEqual "the program that made the island went on" (Just ())
  readIORef logRef >>= assertEqual "while the island waits" ["host initialized", "island waits", "revealed"]
  within "the answer" (ask socket (H.mkRequest AskIsland)) >>= assertEqual "the waiting island answered" (Just (Just 0))

outputWhileWaiting :: IO ()
outputWhileWaiting = withIsland False $ \socket logRef _ pokes -> do
  _ <- within "the query that renders the island" (ask socket (H.mkTell Reveal))
  HS.notify pokes ()
  readIORef logRef >>= assertEqual "the output reached the host at once" ["host initialized", "island waits", "revealed", "host heard poked"]

goneWhileInitializing :: IO ()
goneWhileInitializing = withIsland True $ \socket logRef gate _ -> do
  _ <- within "the query that removes the island" (ask socket (H.mkTell Conceal))
  within "the island's wait to be cancelled" (takeMVar gate.cancelled)
  putMVar gate.open ()
  threadDelay 10_000
  readIORef logRef >>= assertEqual "the island never loaded" ["island waits", "host initialized"]

data TouchQuery a
  = -- | Reads the state and leaves it as it is.
    Look a
  | Put Int a

-- | A component of another monad than the tree's, as one in an
-- application's @ReaderT@ (or a store's @StoreT@) is once the tree is
-- hoisted to the monad underneath.
looker :: H.Component TouchQuery () Void (ReaderT () IO)
looker =
  H.component
    (\_ -> pure (0 :: Int))
    (\n -> HH.text (T.pack (show n)) :: H.ComponentHTML () Empty (ReaderT () IO))
    [ H.onQuery $ \case
        Look a -> get >> pure (Just a)
        Put n a -> put n >> pure (Just a)
    ]

hoistedNoOp :: IO ()
hoistedNoOp = do
  texts <- newIORef []
  socket <- AD.runUI (walking texts) (H.hoist (NT (`runReaderT` ())) looker) ()
  _ <- ask socket (H.mkTell Look)
  _ <- ask socket (H.mkTell Look)
  _ <- ask socket (H.mkTell (Put 1))
  readIORef texts >>= assertEqual "rendered at first and for the one change" ["0", "1"]
  dispose socket

spec :: Spec
spec =
  describe "driver contract" $ do
    it "runs an event at once when the tree is idle" idleTree
    it "runs what arrives while the tree is busy afterwards, in order" busyTree
    it "lets other work run while a program waits, and resumes it after" suspension
    it "renders a state change before the program goes on" renderedFirst
    it "renders nothing for a get, in a hoisted component too" hoistedNoOp
    it "does not deadlock when a child's Receive waits for its parent" receiveAsksParent
    it "cancels a waiting action of a component that goes away" goneWhileWaiting
    it "answers Nothing to a query whose component goes away" queryWhileGoing
    it "ends a failing program alone" failureAlone
    it "runs nothing more of a program that kills itself" selfKill
    it "runs nothing more of a program whose component its own work removed" removedWhileRunning
    it "kills every waiting parallel branch when the tree goes" parallelDisposed
    it "never starts a parallel branch after its sibling failed" parallelFailsAtOnce
    it "cancels a parallel branch whose sibling failed later" parallelFailsLater
    it "runs work an effect brings the tree there and then" nestedDispatch
    it "finishes a handler that kills the fork whose worker brought it" workerNotify
    it "handles an event a callback brings in the middle of an effect before the effect goes on" callbackDispatch
    it "queues work another thread brings while an effect is under way" threadDuringEffect
    it "queues work a worker brings while an effect is under way" workerDuringEffect
    it "takes back the children made by a render pass that failed" failedPass
    it "takes back the children made by a render pass that failed removing others" failedRemoval
    it "initializes the rest when an initializer fails, and ends a finalizer's forks with it" lifecycle
    it "initializes a parent once its child's Initialize waits, and mounts the tree meanwhile" initializedAtFirstWait
    it "lets the program whose render made a child go on while the child's Initialize waits" renderGoesOn
    it "delivers a child's output while its Initialize waits" outputWhileWaiting
    it "cancels the rest of an Initialize whose component goes away while it waits" goneWhileInitializing
