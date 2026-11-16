{-# LANGUAGE CPP #-}

-- | The two ways a program leaves its tree's loop: 'H.sync' runs in place,
-- 'H.async' and 'H.asyncWith' suspend. Then 'H.try', 'H.catch' and
-- 'H.finally', a slow 'H.sync' in "Halogen.Trace", and 'H.send'. The tests
-- use handshakes, not timing, except the slow 'H.sync', which is about time.
-- They run with a render spec that touches no DOM, on every backend.
-- "Halogen.Sync" is tested natively, on the harness ("Halogen.Test.Native").
module Test.Sync (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (ArithException, ErrorCall (..), onException, throwIO)
import Control.Monad (void)
import Control.Monad.Catch (throwM)
import Control.Monad.State.Class (get, modify)
import Data.Foldable (traverse_)
import Data.IORef
import Data.List (sort)
import Data.Kind (Type)
import Data.Row (Empty, Row, type (.==))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Halogen qualified as H
import Halogen.Component (ComponentSlot (..))
import Halogen.HTML qualified as HH
import Halogen.HTML.Core qualified as HC
import Halogen.IO.Driver qualified as AD
import Halogen.Query.Input (Input)
import Halogen.Subscription qualified as HS
import Halogen.Trace qualified as Trace
import Halogen.VDom.Types (VDom (..), runGraft)
import Prelude
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)
#if defined(HALOGEN_WASI) || (!defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH))
import Control.Monad.IO.Class (liftIO)
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Sync qualified as Sync
import Halogen.Test.Native qualified as T
import Halogen.VDom.DOM.Monad (MemDOM)
import Test.Hspec (shouldBe, shouldReturn)
#endif

----------------------------------------------------------------------
-- A render spec with no DOM: it renders every slot and does nothing else.

data Rendered (s :: Type) (act :: Type) (ps :: Row Type) (o :: Type) = Rendered

walking :: AD.RenderSpec IO Rendered
walking =
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
      traverse_ (\case ComponentSlot box -> void (renderChild box); ThunkSlot _ -> pure ()) (walk (HC.unHTML html))
      pure Rendered

walk :: VDom p w -> [w]
walk = \case
  Text _ -> []
  Elem _ _ _ cs -> foldMap walk cs
  Keyed _ _ _ cs -> foldMap (walk . snd) cs
  Widget w -> [w]
  Grafted g -> walk (runGraft g)

-- | Waits have a bound: a broken promise fails the test instead of hanging it.
within :: String -> IO a -> IO a
within what io = timeout 5_000_000 io >>= maybe (throwIO (ErrorCall ("timed out: " <> what))) pure

----------------------------------------------------------------------
-- A component that notes what it does, driven by actions sent through an
-- emitter. Its state is a count that 'Bump' moves.

data Act
  = Bump
  | -- | Holds the tree in a 'H.sync': signals, waits, then notes the count.
    InPlace (MVar ()) (MVar ())
  | -- | Waits in an 'H.async', then notes the count and signals.
    Suspended (MVar ()) (MVar ())
  | -- | Runs a program in an 'H.asyncWith', signals and waits, runs another
    -- that notes the count, and signals.
    Unlifted (MVar ()) (MVar ()) (MVar ())
  | -- | Catches what is thrown before and after an 'H.async', and signals.
    Catching (MVar ())
  | -- | 'H.finally' after an end and after a failure, and signals.
    Finally (MVar ())
  | -- | Forks a program in an 'H.finally' that signals and waits forever
    -- (signalling when its wait is killed), and keeps the fork.
    FinallyForked (MVar ()) (MVar ())
  | -- | Kills the kept fork.
    KillFork
  | -- | Waits forever in an 'H.finally', signalling when it waits and when
    -- its wait is killed.
    FinallyWaiting (MVar ()) (MVar ())
  | -- | A 'H.sync' that takes this many milliseconds.
    Busy Int

newtype Logged = Logged (IORef [Text])

note :: Logged -> Text -> H.HalogenM Int Act ps Void IO ()
note (Logged ref) t = H.sync (modifyIORef' ref (<> [t]))

noteCount :: Logged -> Text -> H.HalogenM Int Act ps Void IO ()
noteCount logged what = get >>= \n -> note logged (what <> " saw " <> T.pack (show n))

logging :: Logged -> IORef (Maybe H.ForkId) -> HS.Emitter IO Act -> H.Component H.VoidF () Void IO
logging logged kept acts =
  H.component
    (\_ -> pure (0 :: Int))
    (\n -> HH.text (T.pack (show n)) :: H.ComponentHTML Act Empty IO)
    [H.onAction handleAction, H.onInitialize (void (H.subscribe acts))]
  where
    handleAction = \case
      Bump -> modify (+ 1) >> note logged "bump"
      InPlace entered release -> do
        H.sync (putMVar entered () >> takeMVar release)
        noteCount logged "after the sync"
      Suspended gate done -> do
        H.async (takeMVar gate)
        noteCount logged "after the async"
        H.sync (putMVar done ())
      Unlifted between gate done -> do
        H.asyncWith $ \run -> do
          run (note logged "first run")
          putMVar between ()
          takeMVar gate
          run (noteCount logged "second run")
        H.sync (putMVar done ())
      Catching done -> do
        let shown :: Either ErrorCall () -> Text
            shown = either (\(ErrorCall m) -> "caught " <> T.pack m) (const "nothing")
        H.try (H.sync (throwIO (ErrorCall "before"))) >>= note logged . shown
        H.try (H.async (pure ()) >> H.sync (throwIO (ErrorCall "after"))) >>= note logged . shown
        H.try (H.async (throwIO (ErrorCall "in the async"))) >>= note logged . shown
        H.catch (H.async (pure ()) >> throwM (ErrorCall "thrown")) (\(ErrorCall m) -> note logged ("handled " <> T.pack m))
        -- A handler for another type lets the exception go on up.
        H.try (H.catch (H.sync (throwIO (ErrorCall "another type"))) (\(_ :: ArithException) -> note logged "wrong handler")) >>= note logged . shown
        H.sync (putMVar done ())
      Finally done -> do
        H.finally (note logged "body") (note logged "cleanup")
        result <- H.try (H.finally (H.async (pure ()) >> H.sync (throwIO (ErrorCall "failed"))) (note logged "cleanup after a failure"))
        note logged (either (\(ErrorCall m) -> "caught " <> T.pack m) (const "nothing") result)
        H.sync (putMVar done ())
      FinallyForked waiting killed -> do
        fid <- H.fork (H.finally (H.async (waitForever waiting killed)) (note logged "cleanup"))
        H.sync (writeIORef kept (Just fid))
      KillFork -> H.sync (readIORef kept) >>= traverse_ H.kill
      FinallyWaiting waiting killed ->
        H.finally (H.async (waitForever waiting killed)) (note logged "cleanup")
      Busy ms -> H.sync (threadDelay (ms * 1000))
    waitForever waiting killed = (putMVar waiting () >> (newEmptyMVar >>= takeMVar)) `onException` putMVar killed ()

withLogging :: (HS.Listener IO Act -> AD.HalogenSocket H.VoidF Void IO -> IORef [Text] -> IO ()) -> IO ()
withLogging body = do
  ref <- newIORef []
  kept <- newIORef Nothing
  HS.Subscribe {HS.listener, HS.emitter} <- HS.create
  socket <- AD.runUI walking (H.named "Test.Sync.logging" (logging (Logged ref) kept emitter)) ()
  body listener socket ref
  socket.dispose

----------------------------------------------------------------------
-- sync and async

inPlace :: IO ()
inPlace = withLogging $ \acts _ logRef -> do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  returned <- newEmptyMVar
  _ <- forkIO $ HS.notify acts (InPlace entered release) >> putMVar returned ()
  within "the sync" (takeMVar entered)
  -- The tree is held: the bump is queued, and the call returns at once.
  HS.notify acts Bump
  readIORef logRef >>= assertEqual "nothing ran meanwhile" []
  putMVar release ()
  within "the holder" (takeMVar returned)
  readIORef logRef >>= assertEqual "the bump ran after the program" ["after the sync saw 0", "bump"]

suspended :: IO ()
suspended = withLogging $ \acts _ logRef -> do
  gate <- newEmptyMVar
  done <- newEmptyMVar
  HS.notify acts (Suspended gate done)
  -- The program waits: the bump runs at once.
  HS.notify acts Bump
  readIORef logRef >>= assertEqual "the bump ran meanwhile" ["bump"]
  putMVar gate ()
  within "the program" (takeMVar done)
  readIORef logRef >>= assertEqual "the program saw the new state" ["bump", "after the async saw 1"]

unlifted :: IO ()
unlifted = withLogging $ \acts _ logRef -> do
  between <- newEmptyMVar
  gate <- newEmptyMVar
  done <- newEmptyMVar
  HS.notify acts (Unlifted between gate done)
  -- The body runs off the loop: the tree is free while it waits.
  within "the first run" (takeMVar between)
  HS.notify acts Bump
  readIORef logRef >>= assertEqual "the bump ran between the runs" ["first run", "bump"]
  putMVar gate ()
  within "the program" (takeMVar done)
  readIORef logRef >>= assertEqual "the second run saw the new state" ["first run", "bump", "second run saw 1"]

----------------------------------------------------------------------
-- try, catch and finally

catching :: IO ()
catching = withLogging $ \acts _ logRef -> do
  done <- newEmptyMVar
  HS.notify acts (Catching done)
  within "the program" (takeMVar done)
  readIORef logRef
    >>= assertEqual
      "every failure caught"
      ["caught before", "caught after", "caught in the async", "handled thrown", "caught another type"]

finallyEnds :: IO ()
finallyEnds = withLogging $ \acts _ logRef -> do
  done <- newEmptyMVar
  HS.notify acts (Finally done)
  within "the program" (takeMVar done)
  readIORef logRef
    >>= assertEqual
      "the cleanup ran after an end and after a failure, before the failure went on"
      ["body", "cleanup", "cleanup after a failure", "caught failed"]

-- | A killed fork runs nothing more, its 'H.finally' included.
finallyKilled :: IO ()
finallyKilled = withLogging $ \acts _ logRef -> do
  waiting <- newEmptyMVar
  killed <- newEmptyMVar
  HS.notify acts (FinallyForked waiting killed)
  within "the fork's wait" (takeMVar waiting)
  HS.notify acts KillFork
  within "the killed wait" (takeMVar killed)
  -- Anything the fork still ran would be queued on the loop: a bump goes
  -- behind it.
  HS.notify acts Bump
  threadDelay 10_000
  readIORef logRef >>= assertEqual "no cleanup" ["bump"]

-- | A program whose component goes away runs nothing more either.
finallyGone :: IO ()
finallyGone = do
  ref <- newIORef []
  kept <- newIORef Nothing
  HS.Subscribe {HS.listener, HS.emitter} <- HS.create
  socket <- AD.runUI walking (logging (Logged ref) kept emitter) ()
  waiting <- newEmptyMVar
  killed <- newEmptyMVar
  HS.notify listener (FinallyWaiting waiting killed)
  within "the wait" (takeMVar waiting)
  socket.dispose
  within "the killed wait" (takeMVar killed)
  threadDelay 10_000
  readIORef ref >>= assertEqual "no cleanup" []

----------------------------------------------------------------------
-- A slow sync in the trace

-- | The 'Trace.SlowSync' events of this module's components while the body
-- runs with a listener.
slowSyncs :: IO () -> IO [Double]
slowSyncs body = do
  heard <- newIORef []
  stop <- Trace.subscribe $ \event -> case event.what of
    Trace.SlowSync took | event.tag.label == "Test.Sync.logging" -> modifyIORef' heard (<> [took])
    _ -> pure ()
  body
  stop
  readIORef heard

slowSync :: IO ()
slowSync = withLogging $ \acts _ _ -> do
  let slow = round Trace.slowSyncThreshold + 30
  heard <- slowSyncs (HS.notify acts (Busy slow))
  case heard of
    [took] -> assertWith ("took at least " <> show slow <> " ms: " <> show took) (took >= fromIntegral slow)
    _ -> assertWith ("one slow sync, heard " <> show heard) False

fastSync :: IO ()
fastSync = withLogging $ \acts _ _ -> do
  heard <- slowSyncs (HS.notify acts (Busy 0))
  assertEqual "nothing reported" [] heard

unheardSync :: IO ()
unheardSync = withLogging $ \acts _ _ -> do
  heard <- newIORef (0 :: Int)
  stop <- Trace.subscribe (\_ -> modifyIORef' heard (+ 1))
  stop
  Trace.listening >>= assertEqual "nobody listens" False
  HS.notify acts (Busy (round Trace.slowSyncThreshold + 30))
  readIORef heard >>= assertEqual "nothing reported" 0

----------------------------------------------------------------------
-- send

data ChildQuery a
  = -- | Notes that it starts, waits for the gate if it has one, and notes
    -- that it ends.
    Hello Text (Maybe (MVar ())) a

child :: Logged -> H.Component ChildQuery () Void IO
child logged =
  H.component
    (\_ -> pure ())
    (\_ -> HH.text "child" :: H.ComponentHTML () Empty IO)
    [ H.onQuery $ \(Hello name gate a) -> do
        noteChild ("child starts " <> name)
        traverse_ (H.async . takeMVar) gate
        noteChild ("child ends " <> name)
        pure (Just a)
    ]
  where
    noteChild t = let Logged ref = logged in H.sync (modifyIORef' ref (<> [t]))

data SendAct = SendAll (MVar ()) (MVar ())

sender :: Logged -> HS.Emitter IO SendAct -> H.Component H.VoidF () Void IO
sender logged acts =
  H.component
    (\_ -> pure (0 :: Int))
    (\_ -> HH.div_ [HH.slot_ "child" () (child logged) ()] :: H.ComponentHTML SendAct ("child" .== H.Slot ChildQuery Void ()) IO)
    [ H.onInitialize (void (H.subscribe acts))
    , H.onAction $ \(SendAll one two) -> do
        H.send "child" () (Hello "one" (Just one))
        note' "parent after one"
        H.send "child" () (Hello "two" (Just two))
        note' "parent after two"
        H.send "child" () (Hello "three" Nothing)
        note' "parent after three"
    ]
  where
    note' t = let Logged ref = logged in H.sync (modifyIORef' ref (<> [t]))

sends :: IO ()
sends = do
  ref <- newIORef []
  HS.Subscribe {HS.listener, HS.emitter} <- HS.create
  socket <- AD.runUI walking (sender (Logged ref) emitter) ()
  one <- newEmptyMVar
  two <- newEmptyMVar
  HS.notify listener (SendAll one two)
  -- Each send returned once the child's handler had reached its wait, or
  -- its end; the parent did not wait for either.
  readIORef ref
    >>= assertEqual
      "the parent went on, after each handler's first part"
      ["child starts one", "parent after one", "child starts two", "parent after two", "child starts three", "child ends three", "parent after three"]
  putMVar two ()
  putMVar one ()
  let ended = do
        logged <- readIORef ref
        if length logged == 9 then pure logged else threadDelay 1_000 >> ended
  logged <- within "the handlers" ended
  assertEqual "both handlers ended" ["child ends one", "child ends two"] (sort (drop 7 logged))
  socket.dispose

#if defined(HALOGEN_WASI) || (!defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH))
----------------------------------------------------------------------
-- Halogen.Sync, on the harness

-- | A store that saves what it is told to, through the test's gate.
data StoreQuery a = Save Text a

store :: T.Gate Text () -> H.Component StoreQuery () Void MemDOM
store gate =
  H.component
    (\_ -> pure "nothing")
    (\saved -> HH.p [HP.class_ (HH.ClassName "saved")] [HH.text saved] :: H.ComponentHTML () Empty MemDOM)
    [ H.onQuery $ \(Save draft a) -> do
        H.async (liftIO (T.hold gate ("save " <> draft)))
        modify (const draft)
        pure (Just a)
    ]

type Slots = ("store" .== H.Slot StoreQuery Void ())

-- | A step's effects, carried out by 'perform'.
data Effect = Announce Text | Store Text | Count

data Editor = Editor {sent :: Int}

data EditorAction = Submit

-- | Written against the class: it cannot suspend.
perform :: (Sync.MonadSync Editor EditorAction Slots Text m h) => Effect -> h ()
perform = \case
  Announce message -> Sync.raise message
  Store draft -> Sync.send "store" () (Save draft)
  Count -> modify (\s -> s {sent = s.sent + 1})

editor :: T.Gate Text () -> H.Component H.VoidF () Text MemDOM
editor gate =
  H.component
    (\_ -> pure Editor {sent = 0})
    render
    [H.onAction $ \Submit -> traverse_ perform [Store "hello", Announce "stored hello", Count]]
  where
    render :: Editor -> H.ComponentHTML EditorAction Slots MemDOM
    render s =
      HH.div_
        [ HH.button [HP.class_ (HH.ClassName "submit"), HE.onClick (const Submit)] [HH.text "submit"]
        , HH.p [HP.class_ (HH.ClassName "sent")] [HH.text (T.pack (show s.sent))]
        , HH.slot_ "store" () (store gate) ()
        ]

performs :: IO ()
performs = do
  gate <- T.newGate
  T.withView id (editor gate) () $ \view -> do
    T.click view "button.submit"
    T.within (T.html view) (T.settleHolding view (T.holds gate))
    -- The editor went on while the store waits for its save.
    T.textOf view ".sent" `shouldReturn` "1"
    T.outputs view `shouldReturn` ["stored hello"]
    T.textOf view ".saved" `shouldReturn` "nothing"
    asks <- T.held gate
    case filter (\h -> h.asked == "save hello") asks of
      [save] -> T.answer gate save ()
      _ -> fail ("expected the save, held: " <> show (map (.asked) asks))
    T.within (T.html view) (T.settle view)
    T.textOf view ".saved" `shouldReturn` "hello"
    length asks `shouldBe` 1
#endif

spec :: Spec
spec = describe "sync and async" $ do
  describe "sync and async" $ do
    it "runs a sync in place: an event that arrives meanwhile is handled after" inPlace
    it "suspends at an async: an event that arrives meanwhile is handled first" suspended
    it "runs the body of an asyncWith off the loop, each run a step of its own" unlifted
  describe "try, catch and finally" $ do
    it "catches a failure before and after an async" catching
    it "runs the second program of finally after an end and after a failure" finallyEnds
    it "does not run the second program of finally when the fork is killed" finallyKilled
    it "does not run the second program of finally when the component goes away" finallyGone
  describe "a slow sync" $ do
    it "is reported when it takes longer than the threshold" slowSync
    it "is not reported when it is fast" fastSync
    it "is not reported while nobody listens" unheardSync
  describe "send" $
    it "goes on without waiting for the child, after its handler's first part, in order" sends
#if defined(HALOGEN_WASI) || (!defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH))
  describe "Halogen.Sync" $
    it "carries out effects written against the class, in a real component" performs
#endif
