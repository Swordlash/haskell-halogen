-- | What the driver promises about when a component's code runs, tested
-- with handshakes rather than timing (see "Halogen.IO.Driver.Runtime").
--
-- * A tree's work runs one piece at a time: what arrives while it is busy
--   waits, in the order it came, and runs before the busy thread lets go.
-- * A program waits only in 'liftIO' (a suspension); meanwhile the rest of
--   the tree goes on, and the program resumes after what came before.
-- * A child's 'Receive' runs as a program of the child: until it waits, the
--   render waits for it; once it waits, the render goes on.
-- * A state change is rendered before the program goes on.
-- * A component that goes away takes its programs with it: none touches it
--   again, a query waiting on it answers 'Nothing', and what its finalizer
--   forked ends with the finalizer.
-- * A program stops the moment it is cancelled, by its own work too.
-- * Parallel branches end together: a failing one takes the others with it.
-- * A render pass that fails takes back the children it made, up to its
--   commit.
-- * Work the tree's own thread brings it (an emitter a synchronous effect
--   notifies) runs there and then.
-- * A failing program ends alone: the rest of the tree goes on.
module Test.DriverContract (spec) where

import Control.Concurrent (forkIO, rtsSupportsBoundThreads, setNumCapabilities, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (ErrorCall (..), SomeException, onException, throwIO)
import Control.Monad (forever, void, when)
import Control.Monad.Catch (try)
import Control.Monad.Parallel (parallel, sequential)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get, modify, put)
import Data.Foldable (for_, traverse_)
import Data.IORef
import Data.Kind (Type)
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
  | Hold (MVar ()) (MVar ())
  -- ^ Holds the tree: signals, then waits in 'liftEffect'.
  | Slow (MVar ()) (MVar ())
  -- ^ Waits in 'liftIO' for the first, then signals the second.
  | Stuck (MVar ()) (MVar ()) (MVar ())
  -- ^ Waits in 'liftIO' for the second forever, signalling the first once
  -- it waits (a worker killed before it starts runs none of its handlers)
  -- and the third when killed.
  | Boom
  | SelfKill (MVar ())
  -- ^ Forks a program that kills itself (and signals when it has).
  | ParStuck (MVar ()) (MVar ()) (MVar ()) (MVar ())
  -- ^ Two branches that wait forever: each signals when it starts, and when
  -- it is killed.
  | ParFailsAtOnce (MVar ())
  -- ^ A branch that fails at once, and one that notes it started; goes on
  -- (past a wait) once it has caught the failure, and signals at the end.
  | ParFailsLater (MVar ()) (MVar ()) (MVar ()) (MVar ()) (MVar ())
  -- ^ A branch that fails once let go, and one that waits (signalling when
  -- it waits, and when it is killed) and then changes the state; signals
  -- once it has caught the failure.
  | Dispatch

data Query a
  = Bump (Text -> a)
  | Current (Int -> a)
  | Hang (MVar ()) a

logging :: IORef [Text] -> IORef [Text] -> HS.Emitter IO Act -> HS.Listener IO Act -> H.Component Query () Void IO
logging texts logRef acts self =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure (0 :: Int)
      , render = \n -> HH.text (T.pack (show n)) :: H.ComponentHTML Act Empty IO
      , eval = H.mkEval H.defaultEval {initialize = Just (Note "init"), handleAction, handleQuery}
      }
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
            <$> parallel (liftIO (putMVar started1 () >> forever (threadDelay 1_000_000) `onException` putMVar killed1 ()) :: H.HalogenM Int Act Empty Void IO ())
            <*> parallel (liftIO (putMVar started2 () >> forever (threadDelay 1_000_000) `onException` putMVar killed2 ()) :: H.HalogenM Int Act Empty Void IO ())
      ParFailsAtOnce done -> do
        result <-
          try $
            sequential $
              (,)
                <$> parallel (H.liftEffect (throwIO (ErrorCall "first")) :: H.HalogenM Int Act Empty Void IO ())
                <*> parallel (note "second started")
        for_ (either (\(_ :: SomeException) -> Just ()) (const Nothing) result) $ \() -> note "caught"
        -- Still running, so that a sibling let go would run too.
        liftIO (pure ())
        note "went on"
        H.liftEffect (putMVar done ())
      ParFailsLater gate1 started2 gate2 killed2 caught -> do
        result <-
          try $
            sequential $
              (,)
                <$> parallel (liftIO (takeMVar gate1 >> throwIO (ErrorCall "first")) :: H.HalogenM Int Act Empty Void IO ())
                <*> parallel (liftIO ((putMVar started2 () >> takeMVar gate2) `onException` putMVar killed2 ()) >> modify (+ 1) >> note "sibling went on")
        for_ (either (\(_ :: SomeException) -> Just ()) (const Nothing) result) $ \() -> note "caught"
        H.liftEffect (putMVar caught ())
      Dispatch -> do
        H.liftEffect (HS.notify self (Note "handled while dispatched"))
        note "dispatched"
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
  HS.Subscribe {HS.listener, HS.emitter} <- HS.create
  socket <- AD.runUI (walking texts) (logging texts logRef emitter listener) ()
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

nestedDispatch :: IO ()
nestedDispatch = withLogging $ \acts _ _ logRef -> do
  HS.notify acts Dispatch
  readIORef logRef >>= assertEqual "handled before the effect returned" ["handled while dispatched", "dispatched"]

----------------------------------------------------------------------
-- A child that tells its parent to remove it, and would go on.

data PokeAct = Watch | Poke

poked :: IORef [Text] -> HS.Emitter IO () -> H.Component H.VoidF () () IO
poked logRef pokes =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ()
      , render = \_ -> HH.text "child" :: H.ComponentHTML PokeAct Empty IO
      , eval =
          H.mkEval
            H.defaultEval
              { initialize = Just Watch
              , handleAction = \case
                  Watch -> void (H.subscribe (Poke <$ pokes))
                  Poke -> do
                    -- The parent removes this component before 'raise' returns.
                    H.raise ()
                    H.liftEffect (modifyIORef' logRef (<> ["went on"]))
                    liftIO (modifyIORef' logRef (<> ["waited"]))
              }
      }

remover :: IORef [Text] -> HS.Emitter IO () -> H.Component Query () Void IO
remover logRef pokes =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure (0 :: Int)
      , render = \n -> HH.div_ [HH.slot "child" () (poked logRef pokes) () (const ()) | n == 0] :: H.ComponentHTML () ("child" .== H.Slot H.VoidF () ()) IO
      , eval =
          H.mkEval
            H.defaultEval
              { handleAction = \() -> modify (+ 1)
              , handleQuery = \case
                  Current k -> Just . k <$> get
                  _ -> pure Nothing
              }
      }

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
  H.mkComponent
    H.ComponentSpec
      { initialState = pure
      , render = \_ -> HH.text "child" :: H.ComponentHTML ChildAct Empty IO
      , eval = H.mkEval H.defaultEval {receive = Just . Received, handleAction}
      }
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
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure (0 :: Int)
      , render = \n -> HH.div_ [HH.slot "child" () (asker childDone) n Answer] :: H.ComponentHTML ParentAct ("child" .== H.Slot H.VoidF (MVar ()) ()) IO
      , eval =
          H.mkEval
            H.defaultEval
              { handleAction = \(Answer reply) -> H.liftEffect (putMVar reply ())
              , handleQuery = \(Set n a) -> put n >> pure (Just a)
              }
      }

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
  H.mkComponent
    H.ComponentSpec
      { initialState = \k -> if k == 3 then throwIO (ErrorCall "no child 3") else pure k
      , render = \_ -> HH.text "" :: H.ComponentHTML Life Empty IO
      , eval =
          H.mkEval
            H.defaultEval
              { initialize = Just Start
              , finalize = Just Stop
              , handleAction = \case
                  Start -> H.liftEffect $ modifyIORef' counts.started (+ 1)
                  Stop -> H.liftEffect $ modifyIORef' counts.stopped (+ 1)
              }
      }

rows :: Counts -> H.Component SetQuery () Void IO
rows counts =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure (1 :: Int)
      , render = \n -> HH.div_ [HH.slot_ "row" k (counted counts) k | k <- [0 .. n]] :: H.ComponentHTML () ("row" .== H.Slot H.VoidF Void Int) IO
      , eval =
          H.mkEval
            H.defaultEval
              { initialize = Just ()
              , handleAction = \() -> H.liftEffect $ modifyIORef' counts.parentStarted (+ 1)
              , handleQuery = \(Set n a) -> put n >> pure (Just a)
              }
      }

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
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure (0 :: Int)
      , render = \k -> HH.div_ [HH.slot_ "row" k (counted counts) k] :: H.ComponentHTML () ("row" .== H.Slot H.VoidF Void Int) IO
      , eval = H.mkEval H.defaultEval {handleQuery = \(Set n a) -> put n >> pure (Just a)}
      }

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
  void $ ask socket (H.mkTell (Set 1))
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
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ()
      , render = \_ -> HH.text "" :: H.ComponentHTML InitAct Empty IO
      , eval =
          H.mkEval
            H.defaultEval
              { initialize = Just Init
              , handleAction = \_ -> H.liftEffect $ if bad then throwIO (ErrorCall "init") else modifyIORef' logRef (<> ["good child"])
              }
      }

initParent :: IORef [Text] -> MVar () -> H.Component H.VoidF () Void IO
initParent logRef forkKilled =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ()
      , render = \_ -> HH.div_ [HH.slot_ "c" False (initFails logRef False) (), HH.slot_ "c" True (initFails logRef True) ()] :: H.ComponentHTML InitAct ("c" .== H.Slot H.VoidF Void Bool) IO
      , eval =
          H.mkEval
            H.defaultEval
              { initialize = Just Init
              , finalize = Just Fin
              , handleAction = \case
                  Init -> H.liftEffect $ modifyIORef' logRef (<> ["parent"])
                  -- Forks work that outlives the finalizer: it ends with it.
                  Fin -> do
                    forkWaits <- H.liftEffect newEmptyMVar
                    void $ H.fork $ liftIO $ (putMVar forkWaits () >> (newEmptyMVar >>= takeMVar :: IO ())) `onException` putMVar forkKilled ()
                    liftIO (readMVar forkWaits)
              }
      }

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

spec :: Spec
spec =
  describe "driver contract" $ do
    it "runs an event at once when the tree is idle" idleTree
    it "runs what arrives while the tree is busy afterwards, in order" busyTree
    it "lets other work run while a program waits, and resumes it after" suspension
    it "renders a state change before the program goes on" renderedFirst
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
    it "takes back the children made by a render pass that failed" failedPass
    it "takes back the children made by a render pass that failed removing others" failedRemoval
    it "initializes the rest when an initializer fails, and ends a finalizer's forks with it" lifecycle
