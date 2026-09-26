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
-- * A render pass that fails takes back the children it made.
-- * A failing program ends alone: the rest of the tree goes on.
module Test.DriverContract (spec) where

import Control.Concurrent (forkIO, rtsSupportsBoundThreads, setNumCapabilities)
import Control.Concurrent.MVar
import Control.Exception (ErrorCall (..), SomeException, onException, throwIO, try)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get, modify, put)
import Data.Foldable (traverse_)
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
  | Stuck (MVar ()) (MVar ())
  -- ^ Waits in 'liftIO' forever; signals the second when killed.
  | Boom

data Query a
  = Bump (Text -> a)
  | Current (Int -> a)
  | Hang (MVar ()) a

logging :: IORef [Text] -> IORef [Text] -> HS.Emitter IO Act -> H.Component Query () Void IO
logging texts logRef acts =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure (0 :: Int)
      , render = \n -> HH.text (T.pack (show n)) :: H.ComponentHTML Act Empty IO
      , eval = H.mkEval H.defaultEval {initialize = Just (Note "init"), handleAction, handleQuery}
      }
  where
    note t = H.liftEffect (modifyIORef' logRef (<> [t]))
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
      Stuck gate killed -> do
        note "stuck"
        liftIO (takeMVar gate `onException` putMVar killed ())
        modify (+ 1)
        note "after"
      Boom -> H.liftEffect (throwIO (ErrorCall "boom"))
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
  socket <- AD.runUI (walking texts) (logging texts logRef emitter) ()
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
  gate <- newEmptyMVar
  killed <- newEmptyMVar
  HS.notify acts (Stuck gate killed)
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
                  Fin -> void $ H.fork $ liftIO $ (newEmptyMVar >>= takeMVar :: IO ()) `onException` putMVar forkKilled ()
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
    it "takes back the children made by a render pass that failed" failedPass
    it "initializes the rest when an initializer fails, and ends a finalizer's forks with it" lifecycle
