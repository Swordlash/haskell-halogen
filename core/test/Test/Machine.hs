{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}

-- | "Halogen.Machine": a component whose logic is a pure step. The tests of
-- the runner use a render spec that touches no DOM ("Test.Sync"'s
-- 'walking'), and run on every backend. The tests on the harness
-- ("Halogen.Test.Native") run natively. Asks wait on what the test hands
-- them, and the tests wait for the program's activity, never for a clock.
module Test.Machine (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (ErrorCall (..), SomeException, displayException, throwIO)
import Control.Monad (forM, forM_, void, when)
import Control.Monad.State.Class (get, gets, modify, put)
import Data.IORef
import Data.IntMap.Strict qualified as IntMap
import Data.Row (type (.==))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Halogen qualified as H
import Halogen.Activity (Activity (..))
import Halogen.Activity.Control (advance, withTestClock)
import Halogen.HTML qualified as HH
import Halogen.IO.Driver qualified as AD
import Halogen.Machine qualified as M
import Halogen.Subscription qualified as HS
import Halogen.Sync qualified as Sync
import Prelude
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Sync (walking, within)
import Test.Utils (assertEqual)
#if defined(HALOGEN_WASI) || (!defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Text qualified as Text
import Data.Foldable (traverse_)
import Data.Row (Empty)
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Latest (Ask, Asks, newAsk, noAsks, supersede, takes)
import Halogen.Test.Native qualified as T
import Halogen.VDom.DOM.Monad (MemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Test.Hspec (shouldReturn)
#else
import Test.Hspec (xdescribe)
#endif

----------------------------------------------------------------------
-- A machine run by a script: each event carries the effects its step
-- returns. The model is the list of events stepped. Every step notes that
-- it stepped first, so the test's log shows steps and effects in order.

data Fx
  = -- | Note this in the test's log.
    Note Text
  | -- | Signal the first, and wait for the second, in place.
    Hold (MVar ()) (MVar ())
  | -- | Bring this event to the machine in place, from inside the effect:
    -- the component's emitter is notified on the loop's thread.
    Poke Ev
  | -- | Throw, in place.
    Fail Text
  | -- | Ask, under a key or not. The answer's step returns the effects.
    Fetch (Maybe Text) Text (M.Work ScriptSlots IO Text) [Fx]
  | -- | Cancel the asks under a key.
    Stop Text
  | -- | Send the child a number.
    SendChild Int
  | -- | Ask the child for its number.
    AskChild [Fx]
  | -- | Raise an output.
    Raise Text
  | -- | Make a resource, and keep it in the state, in one effect.
    Acquire Int
  | -- | Release every resource the state keeps.
    ReleaseAll

data Ev
  = Ev Text [Fx]
  | Answer Text (Either SomeException Text) [Fx]

type Script = [Text]

stepScript :: Ev -> Script -> (Script, [Fx])
stepScript ev model = (model <> [label], Note ("stepped " <> label) : fx)
  where
    (label, fx) = case ev of
      Ev name effects -> (name, effects)
      Answer name answer effects ->
        ("answer " <> name <> " " <> either (const "failed") id answer <> " saw " <> T.pack (show (length model)), effects)

data ChildQuery a = SetNumber Int a | GetNumber (Int -> a)

numbered :: H.Component ChildQuery () Void IO
numbered =
  H.component
    (\_ -> pure 0)
    (\_ -> HH.text "")
    [ H.onQuery $ \case
        SetNumber n a -> Just a <$ put n
        GetNumber k -> gets (Just . k)
    ]

type ScriptSlots = ("child" .== H.Slot ChildQuery Void ())

data Scripted = Scripted {script :: M.Machine Text Ev Script, held :: [Int]}

data ScriptAct = Happen Ev | Trying Ev

-- | Where a scripted component notes what it does, and which resources
-- are open.
data Notes = Notes {logged :: IORef [Text], open :: IORef (Set Int)}

note :: Notes -> Text -> IO ()
note notes t = modifyIORef' notes.logged (<> [t])

-- | The scripted machine. Its events come through the emitter. In its
-- finalizer it steps the event given.
scripted :: Notes -> HS.Listener IO ScriptAct -> HS.Emitter IO ScriptAct -> Ev -> H.Component H.VoidF () Text IO
scripted notes listener acts final =
  H.component
    (\_ -> pure Scripted {script = M.new [], held = []})
    (\_ -> HH.div_ [HH.slot_ "child" () numbered ()] :: H.ComponentHTML ScriptAct ScriptSlots IO)
    [ H.onInitialize (void (H.subscribe acts))
    , H.onAction $ \case
        Happen ev -> happen ev
        Trying ev ->
          H.try (happen ev) >>= \case
            Left (e :: SomeException) -> H.sync (note notes ("caught " <> T.pack (displayException e)))
            Right () -> H.sync (note notes "nothing caught")
    , H.onFinalize (M.finish machine final)
    ]
  where
    happen = M.happen machine
    machine = M.Spec {M.step = stepScript, M.carry = carry, M.get = (.script), M.set = \m s -> s {script = m}}
    carry = \case
      Note t -> M.Now (Sync.sync (note notes t))
      Hold entered release -> M.Now (Sync.sync (putMVar entered () >> takeMVar release))
      Poke ev -> M.Now (Sync.sync (HS.notify listener (Happen ev)))
      Fail message -> M.Now (Sync.sync (throwIO (ErrorCall (T.unpack message))))
      Fetch Nothing name work fx -> M.Ask work (\r -> Answer name r fx)
      Fetch (Just key) name work fx -> M.AskUnder key work (\r -> Answer name r fx)
      Stop key -> M.Cancel key
      SendChild n -> M.Now (Sync.send "child" () (SetNumber n))
      AskChild fx -> M.Ask (M.child "child" () GetNumber) (\r -> Answer "child" (T.pack . show <$> r) fx)
      Raise t -> M.Now (Sync.raise t)
      Acquire n -> M.Now $ do
        Sync.sync (modifyIORef' notes.open (Set.insert n))
        modify (\s -> s {held = n : s.held})
      ReleaseAll -> M.Now $ do
        s <- get
        Sync.sync (modifyIORef' notes.open (\o -> foldr Set.delete o s.held))
        put s {held = []}

-- | What a test of the scripted machine works with.
data Run = Run
  { send :: Ev -> IO ()
  , trying :: Ev -> IO ()
  , logged :: IO [Text]
  , notes :: Notes
  , socket :: AD.HalogenSocket H.VoidF Text IO
  }

newNotes :: IO Notes
newNotes = Notes <$> newIORef [] <*> newIORef Set.empty

-- | Mount the scripted machine, run the body, and dispose of it.
withScripted :: Ev -> (Run -> IO a) -> IO a
withScripted final body = do
  notes <- newNotes
  HS.Subscribe {HS.listener, HS.emitter} <- HS.create
  socket <- AD.runUI walking (H.named "Test.Machine.scripted" (scripted notes listener emitter final)) ()
  a <-
    body
      Run
        { send = HS.notify listener . Happen
        , trying = HS.notify listener . Trying
        , logged = readIORef notes.logged
        , notes
        , socket
        }
  socket.dispose
  pure a

-- | Wait until the tree has done all it can while this many asks wait.
waitAsks :: AD.HalogenSocket q o IO -> Int -> IO ()
waitAsks AD.HalogenSocket {AD.waitActivity = w} n =
  within ("the tree, with " <> show n <> " asks out") (w (\a -> a.running == 0 && a.waiting == n))

-- | Work that waits for the test's answer.
gated :: MVar Text -> M.Work ScriptSlots IO Text
gated = M.run . takeMVar

----------------------------------------------------------------------
-- The runner

inOrder :: IO ()
inOrder = withScripted (Ev "finished" []) $ \run -> do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  returned <- newEmptyMVar
  _ <- forkIO $ run.send (Ev "go" [Note "first", Hold entered release, Note "second"]) >> putMVar returned ()
  within "the hold" (takeMVar entered)
  -- The loop is held: the event is queued, and the call returns.
  run.send (Ev "later" [])
  putMVar release ()
  within "the step" (takeMVar returned)
  waitAsks run.socket 0
  run.logged >>= assertEqual "the later event after the step's effects" ["stepped go", "first", "second", "stepped later"]

asks :: IO ()
asks = withScripted (Ev "finished" []) $ \run -> do
  gate <- newEmptyMVar
  run.send (Ev "fetch" [Fetch Nothing "x" (gated gate) [], Note "after the ask"])
  -- The work started in the step: its fiber waits.
  waitAsks run.socket 1
  run.send (Ev "bump" [])
  putMVar gate "v"
  waitAsks run.socket 0
  run.logged
    >>= assertEqual
      "the answer is a later step, and sees the event that came between"
      ["stepped fetch", "after the ask", "stepped bump", "stepped answer x v saw 2"]

toCompletion :: IO ()
toCompletion = withScripted (Ev "finished" []) $ \run -> do
  run.send (Ev "outer" [Note "outer one", Poke (Ev "inner" [Note "inner one"]), Note "outer two"])
  waitAsks run.socket 0
  run.logged
    >>= assertEqual
      "the event an effect brought is stepped after the step's effects"
      ["stepped outer", "outer one", "outer two", "stepped inner", "inner one"]

failingNow :: IO ()
failingNow = withScripted (Ev "finished" []) $ \run -> do
  run.trying (Ev "boom" [Poke (Ev "queued" []), Fail "broken", Note "dropped"])
  run.send (Ev "next" [])
  waitAsks run.socket 0
  run.logged
    >>= assertEqual
      "the rest of the step dropped, the queued event stepped, the failure gone up, the machine still steps"
      ["stepped boom", "stepped queued", "caught broken", "stepped next"]

failingAsk :: IO ()
failingAsk = withScripted (Ev "finished" []) $ \run -> do
  run.send (Ev "fetch" [Fetch Nothing "x" (M.run (throwIO (ErrorCall "no network"))) []])
  waitAsks run.socket 0
  run.logged >>= assertEqual "the failure is an answer" ["stepped fetch", "stepped answer x failed saw 1"]

cancelled :: IO ()
cancelled = withScripted (Ev "finished" []) $ \run -> do
  gate <- newEmptyMVar
  run.send (Ev "fetch" [Fetch (Just "k") "x" (gated gate) []])
  waitAsks run.socket 1
  run.send (Ev "stop" [Stop "k"])
  -- Its work was killed: nothing waits any more.
  waitAsks run.socket 0
  _ <- tryPutMVar gate "v"
  run.send (Ev "later" [])
  waitAsks run.socket 0
  run.logged >>= assertEqual "no answer" ["stepped fetch", "stepped stop", "stepped later"]

-- | The work ends while the loop is held, so its answer is queued on the
-- loop. The step that holds the loop then cancels the key.
cancelledAfterTheWork :: IO ()
cancelledAfterTheWork = withScripted (Ev "finished" []) $ \run -> do
  gate <- newEmptyMVar
  run.send (Ev "fetch" [Fetch (Just "k") "x" (gated gate) []])
  waitAsks run.socket 1
  entered <- newEmptyMVar
  release <- newEmptyMVar
  returned <- newEmptyMVar
  _ <- forkIO $ run.send (Ev "hold" [Hold entered release, Stop "k"]) >> putMVar returned ()
  within "the hold" (takeMVar entered)
  putMVar gate "v"
  -- The fiber stops waiting once its next step is queued on the loop.
  let AD.HalogenSocket {AD.waitActivity = w} = run.socket
  within "the answer queued" (w (\a -> a.waiting == 0))
  putMVar release ()
  within "the step" (takeMVar returned)
  waitAsks run.socket 0
  run.logged >>= assertEqual "no answer" ["stepped fetch", "stepped hold"]

cancelsEveryAskUnderTheKey :: IO ()
cancelsEveryAskUnderTheKey = withScripted (Ev "finished" []) $ \run -> do
  x <- newEmptyMVar
  y <- newEmptyMVar
  z <- newEmptyMVar
  w <- newEmptyMVar
  run.send
    ( Ev
        "fetch"
        [ Fetch (Just "k") "x" (gated x) []
        , Fetch (Just "k") "y" (gated y) []
        , Fetch Nothing "z" (gated z) []
        , Fetch (Just "other") "w" (gated w) []
        ]
    )
  waitAsks run.socket 4
  run.send (Ev "stop" [Stop "k"])
  waitAsks run.socket 2
  forM_ [x, y, z, w] $ \g -> tryPutMVar g "v"
  waitAsks run.socket 0
  logged <- run.logged
  -- The two answers can come in either order.
  let answered = Set.fromList (map (T.unwords . take 4 . T.words) (drop 2 logged))
  assertEqual "the asks under the key stopped" ["stepped fetch", "stepped stop"] (take 2 logged)
  assertEqual ("the others answered: " <> show logged) (4, Set.fromList ["stepped answer z v", "stepped answer w v"]) (length logged, answered)

-- | An answer's step cancels its own key. The ask left the map before its
-- answer was stepped, so the fork carrying the step out is not killed.
cancelsItsOwnKey :: IO ()
cancelsItsOwnKey = withScripted (Ev "finished" []) $ \run -> do
  x <- newEmptyMVar
  y <- newEmptyMVar
  run.send (Ev "fetch" [Fetch (Just "k") "x" (gated x) [Stop "k", Note "after the cancel"], Fetch (Just "k") "y" (gated y) []])
  waitAsks run.socket 2
  putMVar x "v"
  -- The answer's step cancelled y.
  waitAsks run.socket 0
  _ <- tryPutMVar y "v"
  run.send (Ev "later" [])
  waitAsks run.socket 0
  run.logged
    >>= assertEqual
      "the step ran to its end, and the other ask under the key stopped"
      ["stepped fetch", "stepped answer x v saw 1", "after the cancel", "stepped later"]

child :: IO ()
child = withScripted (Ev "finished" []) $ \run -> do
  run.send (Ev "tell" [SendChild 5, AskChild [Note "after the answer"]])
  waitAsks run.socket 0
  run.logged >>= assertEqual "the child was told, then asked" ["stepped tell", "stepped answer child Just 5 saw 1", "after the answer"]

resources :: IO ()
resources = do
  notes <- newNotes
  HS.Subscribe {HS.listener, HS.emitter} <- HS.create
  let final = Ev "finished" [ReleaseAll, Note "released", Fetch Nothing "late" (M.run (pure "v")) []]
  socket <- AD.runUI walking (scripted notes listener emitter final) ()
  HS.notify listener (Happen (Ev "acquire" [Acquire 1, Acquire 2]))
  readIORef notes.open >>= assertEqual "both open" (Set.fromList [1, 2])
  socket.dispose
  let AD.HalogenSocket {AD.waitActivity = w} = socket
  within "the finalizer" (w (\a -> a.running == 0 && a.waiting == 0))
  readIORef notes.open >>= assertEqual "both released" Set.empty
  readIORef notes.logged
    >>= assertEqual
      "the finalizer's effects ran, and its ask was never answered"
      ["stepped acquire", "stepped finished", "released"]

-- | A child whose step raises an output that makes the parent remove it.
-- The step is cut short there. The child's finalizer still steps its event.
newtype ParentAct = Said Text

cutShort :: IO ()
cutShort = do
  notes <- newNotes
  HS.Subscribe {HS.listener, HS.emitter} <- HS.create
  let child' = scripted notes listener emitter (Ev "finished" [Note "finished"])
      parent :: H.Component H.VoidF () Void IO
      parent =
        H.component
          (\_ -> pure True)
          (\shown -> HH.div_ [HH.slot "machine" () child' () Said | shown] :: H.ComponentHTML ParentAct ("machine" .== H.Slot H.VoidF Text ()) IO)
          [H.onAction $ \(Said t) -> when (t == "bye") (put False)]
  socket <- AD.runUI walking parent ()
  HS.notify listener (Happen (Ev "leave" [Raise "bye", Note "after the raise"]))
  waitAsks socket 0
  readIORef notes.logged >>= assertEqual "the finalizer's event stepped" ["stepped leave", "stepped finished", "finished"]
  socket.dispose

-- | A debounce: each event cancels the pause under the key and asks for a
-- new one. The pauses sleep on the test's clock.
debounced :: IO ()
debounced = withTestClock $ \clock -> withScripted (Ev "finished" []) $ \run -> do
  let pause name = Fetch (Just "debounce") name ("fired" <$ M.delayCounted 0.3) []
  run.send (Ev "type a" [Stop "debounce", pause "a"])
  -- A counted pause waits.
  waitAsks run.socket 1
  advance clock 0.2
  run.send (Ev "type b" [Stop "debounce", pause "b"])
  waitAsks run.socket 1
  advance clock 0.2
  run.logged >>= assertEqual "nothing fired yet" ["stepped type a", "stepped type b"]
  advance clock 0.1
  waitAsks run.socket 0
  advance clock 1
  waitAsks run.socket 0
  run.logged >>= assertEqual "only the last pause fired, once" ["stepped type a", "stepped type b", "stepped answer b fired saw 2"]

-- | A pause that is not counted: the tree reads as quiet while it sleeps.
notCounted :: IO ()
notCounted = withTestClock $ \clock -> withScripted (Ev "finished" []) $ \run -> do
  run.send (Ev "notice" [Fetch (Just "notice") "hide" ("hidden" <$ M.delay 3) []])
  waitAsks run.socket 0
  run.logged >>= assertEqual "not fired yet" ["stepped notice"]
  advance clock 3
  waitAsks run.socket 0
  run.logged >>= assertEqual "fired" ["stepped notice", "stepped answer hide hidden saw 1"]

----------------------------------------------------------------------
-- Every order of a few events, answers and cancels, on a real component

data PEv = Tick | Fetch' | FetchFree | Stop' | Got Int
  deriving stock (Eq, Show)

data PModel = PModel {steps :: Int, count :: Int, asked :: Int, taken :: [Int]}
  deriving stock (Eq, Show)

data PFx = Mark Int Int | AskKeyed Int | AskFree Int | PokeTick | CancelKeyed

-- | Each step returns a first and a last mark, numbered by the step, with
-- its other effects between them.
pstep :: PEv -> PModel -> (PModel, [PFx])
pstep ev m = (m' {steps = m.steps + 1}, [Mark m.steps 0] <> middle <> [Mark m.steps 1])
  where
    (m', middle) = case ev of
      Tick -> (m {count = m.count + 1}, [])
      Fetch' -> (m {asked = m.asked + 1}, [AskKeyed (m.asked + 1), PokeTick])
      FetchFree -> (m {asked = m.asked + 1}, [AskFree (m.asked + 1)])
      Stop' -> (m, [CancelKeyed])
      Got n -> (m {taken = m.taken <> [n]}, [])

blank :: PModel
blank = PModel {steps = 0, count = 0, asked = 0, taken = []}

-- | What the test does: bring an event, or answer an ask by its number.
data Move = Send PEv | Release Int
  deriving stock (Eq, Show)

-- | Every run of up to five moves. Each ask is answered once or never,
-- after it is made.
moves :: [[Move]]
moves = go (5 :: Int) 0 []
  where
    go n made out =
      [[]]
        <> concat
          [ [Send e : rest | rest <- go (n - 1) made' out']
          | n > 0
          , (e, made', out') <-
              [ (Tick, made, out)
              , (Fetch', made + 1, out <> [made + 1])
              , (FetchFree, made + 1, out <> [made + 1])
              , (Stop', made, out)
              ]
          ]
        <> [Release k : rest | n > 0, k <- out, rest <- go (n - 1) made (filter (/= k) out)]

-- | The events the machine should step, in order, and how many asks wait
-- after each move.
expected :: [Move] -> ([PEv], [Int])
expected = walk 0 IntMap.empty [] []
  where
    -- Asks out by number: whether each is under the key, and whether it
    -- was cancelled.
    walk :: Int -> IntMap.IntMap (Bool, Bool) -> [PEv] -> [Int] -> [Move] -> ([PEv], [Int])
    walk made out evs counts = \case
      [] -> (evs, counts)
      Send e : rest ->
        let (made', out', stepped) = case e of
              Fetch' -> (made + 1, IntMap.insert (made + 1) (True, False) out, [Fetch', Tick])
              FetchFree -> (made + 1, IntMap.insert (made + 1) (False, False) out, [FetchFree])
              Stop' -> (made, IntMap.map (\(keyed, gone) -> (keyed, gone || keyed)) out, [Stop'])
              _ -> (made, out, [e])
         in walk made' out' (evs <> stepped) (counts <> [waitingIn out']) rest
      Release k : rest ->
        let gone = maybe True snd (IntMap.lookup k out)
            out' = IntMap.delete k out
         in walk made out' (evs <> [Got k | not gone]) (counts <> [waitingIn out']) rest
    waitingIn = IntMap.size . IntMap.filter (not . snd)

newtype PState = PState {counted :: M.Machine () PEv PModel}

data PQuery a = CurrentModel (PModel -> a)

-- | Run the moves on a real component. 'Nothing' when every rule held, or
-- what went wrong.
runMoves :: [Move] -> IO (Maybe String)
runMoves ms = do
  marks <- newIORef []
  gates <- newIORef IntMap.empty
  HS.Subscribe {HS.listener, HS.emitter} <- HS.create
  let gateOf :: Int -> IO (MVar Int)
      gateOf n = do
        fresh <- newEmptyMVar
        atomicModifyIORef' gates $ \g -> case IntMap.lookup n g of
          Just old -> (g, old)
          Nothing -> (IntMap.insert n fresh g, fresh)
      machine = M.Spec {M.step = pstep, M.carry = carry, M.get = (.counted), M.set = \m s -> s {counted = m}}
      carry = \case
        Mark s i -> M.Now (Sync.sync (modifyIORef' marks ((s, i) :)))
        AskKeyed n -> M.AskUnder () (M.run (gateOf n >>= takeMVar)) (Got . either (const (-1)) id)
        AskFree n -> M.Ask (M.run (gateOf n >>= takeMVar)) (Got . either (const (-1)) id)
        PokeTick -> M.Now (Sync.sync (HS.notify listener Tick))
        CancelKeyed -> M.Cancel ()
      component :: H.Component PQuery () Void IO
      component =
        H.component
          (\_ -> pure PState {counted = M.new blank})
          (\_ -> HH.text "")
          [ H.onInitialize (void (H.subscribe emitter))
          , H.onAction (M.happen machine)
          , H.onQuery $ \(CurrentModel k) -> gets (Just . k . M.current . (.counted))
          ]
  socket <- AD.runUI walking component ()
  let (evs, counts) = expected ms
  forM_ (zip ms counts) $ \(move, n) -> do
    case move of
      Send e -> HS.notify listener e
      Release k -> gateOf k >>= \g -> void (tryPutMVar g k)
    waitAsks socket n
  let AD.HalogenSocket {AD.query = queryTree} = socket
  model <- queryTree (CurrentModel id)
  socket.dispose
  stepsMarked <- reverse <$> readIORef marks
  let want = foldl' (\m e -> fst (pstep e m)) blank evs
      wantMarks = concatMap (\s -> [(s, 0), (s, 1)]) [0 .. length evs - 1]
  pure $
    if
      | model /= Just want -> Just ("the model is " <> show model <> ", not " <> show want)
      | stepsMarked /= wantMarks -> Just ("the effects ran as " <> show stepsMarked)
      | otherwise -> Nothing

everyOrder :: IO ()
everyOrder = do
  failures <- fmap concat $ forM moves $ \ms -> maybe [] (\why -> [(ms, why)]) <$> runMoves ms
  take 3 failures `shouldBe` []
  length moves `shouldBe` 3115

----------------------------------------------------------------------
-- On the harness

#if defined(HALOGEN_WASI) || (!defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH))

-- | A click focuses a field, whose focus handler brings another event: the
-- focus is dispatched from inside the click's step.
data FocusEv = Clicked | Focused

data FocusFx = Log Text | FocusField

data Focusing = Focusing {focus :: M.Machine () FocusEv (), log :: [Text], field :: H.RefLabel}

focusing :: H.Component H.VoidF () Void MemDOM
focusing =
  H.component
    (\_ -> (\field -> Focusing {focus = M.new (), log = [], field}) <$> H.newRefLabel "field")
    renderF
    [H.onAction (M.happen machine)]
  where
    machine = M.Spec {M.step = stepF, M.carry = carry, M.get = (.focus), M.set = \m s -> s {focus = m}}
    stepF ev () = case ev of
      Clicked -> ((), [Log "stepped click", Log "click one", FocusField, Log "click two"])
      Focused -> ((), [Log "stepped focus"])
    carry = \case
      Log t -> M.Now (modify (\s -> s {log = s.log <> [t]}))
      FocusField -> M.Now $ do
        field <- gets (.field)
        Sync.getRef field >>= traverse_ (\el -> Sync.sync (liftIO (T.focusNode (N.toNative el)) :: MemDOM ()))
    renderF :: Focusing -> H.ComponentHTML FocusEv Empty MemDOM
    renderF s =
      HH.div_
        [ HH.button [HP.class_ (HH.ClassName "go"), HE.onClick (const Clicked)] [HH.text "go"]
        , HH.input [HP.ref s.field, HE.onFocus (const Focused)]
        , HH.p [HP.class_ (HH.ClassName "log")] [HH.text (T.intercalate ", " s.log)]
        ]

dispatched :: IO ()
dispatched =
  T.withView id focusing () $ \view -> do
    T.click view "button.go"
    T.within (T.html view) (T.settle view)
    T.textOf view ".log" `shouldReturn` "stepped click, click one, click two, stepped focus"

-- The header's example, as printed there. The test's monad answers each
-- fetch and preview through the test's gate.

-- What the view asks of its monad.
class (Monad m) => Pages m where
  fetchPage :: Text -> m Text
  renderPreview :: Text -> m Text

-- The model and its step. Plain Haskell.
data Model = Model {slug :: Text, page :: Maybe Text, loads :: Asks}

data Event
  = Opened Text
  | Loaded Ask (Either SomeException Text)
  | Typed Text
  | Previewed (Either SomeException Text)

data Effect = Load Ask Text | Preview Text | StopPreview | Display Text | Announce Text

step :: Event -> Model -> (Model, [Effect])
step event model = case event of
  Opened slug ->
    -- Another page: no answer for the old one may show.
    let (ticket, loads) = newAsk (supersede model.loads)
     in (model {slug, page = Nothing, loads}, [StopPreview, Load ticket slug])
  Loaded ticket answer -> case takes ticket model.loads of
    Nothing -> (model, [])
    Just loads ->
      let page = either (Text.pack . displayException) id answer
       in (model {page = Just page, loads}, [Announce model.slug])
  Typed draft -> (model, [StopPreview, Preview draft])
  Previewed answer -> (model, [Display (either (const "") id answer)])

-- The component. It keeps the machine in its state.
data Key = Previewing
  deriving (Eq, Ord)

newtype State = State {machine :: M.Machine Key Event Model}

newtype Action = Type Text

type Slots = ("preview" .== H.Slot PreviewQuery Void ())

pageView :: (Pages m) => H.Component q Text Text m
pageView =
  H.component
    (\slug -> pure State {machine = M.new Model {slug, page = Nothing, loads = noAsks}})
    render
    [ H.onInitialize (gets (\s -> (M.current s.machine).slug) >>= happen . Opened)
    , H.onReceive (happen . Opened)
    , H.onAction (\(Type draft) -> happen (Typed draft))
    ]
  where
    happen = M.happen M.Spec {M.step = step, M.carry = carry, M.get = (.machine), M.set = \machine s -> s {machine}}
    carry = \case
      Load ticket slug -> M.Ask (M.run (fetchPage slug)) (Loaded ticket)
      Preview draft -> M.AskUnder Previewing (M.run (renderPreview draft)) Previewed
      StopPreview -> M.Cancel Previewing
      Display shown -> M.Now (Sync.send "preview" () (ShowPreview shown))
      Announce slug -> M.Now (Sync.raise slug)

instance Pages (ReaderT (T.Gate Text Text) MemDOM) where
  fetchPage slug = ask >>= \gate -> liftIO (T.hold gate ("load " <> slug))
  renderPreview draft = ask >>= \gate -> liftIO (T.hold gate ("preview " <> draft))

data PreviewQuery a = ShowPreview Text a

previewer :: (Monad m) => H.Component PreviewQuery () Void m
previewer =
  H.component
    (\_ -> pure "")
    (\shown -> HH.p [HP.class_ (HH.ClassName "preview")] [HH.text shown])
    [H.onQuery $ \(ShowPreview shown a) -> Just a <$ put shown]

render :: (Monad m) => State -> H.ComponentHTML Action Slots m
render s =
  HH.div_
    [ HH.p [HP.class_ (HH.ClassName "page")] [HH.text (maybe "loading" id (M.current s.machine).page)]
    , HH.input [HP.class_ (HH.ClassName "draft"), HE.onValueInput Type]
    , HH.slot_ "preview" () previewer ()
    ]

-- | The asks the gate holds, by what they ask.
asked :: T.Gate Text Text -> IO [Text]
asked gate = map (.asked) <$> T.held gate

answerAsk :: T.Gate Text Text -> Text -> Text -> IO ()
answerAsk gate what r =
  T.held gate >>= \hs -> case filter (\h -> h.asked == what) hs of
    [h] -> T.answer gate h r
    _ -> fail ("expected one ask for " <> show what <> ", held: " <> show (map (.asked) hs))

example :: IO ()
example = do
  gate <- T.newGate
  T.withView (`runReaderT` gate) (pageView :: H.Component H.VoidF Text Text (ReaderT (T.Gate Text Text) MemDOM)) "a" $ \view -> do
    let holding = T.within (T.html view) (T.settleHolding view (T.holds gate))
    holding
    T.setInput view "b"
    holding
    Set.fromList <$> asked gate `shouldReturn` Set.fromList ["load a", "load b"]
    -- The later load answers first, then the earlier one.
    answerAsk gate "load b" "page B"
    holding
    T.textOf view ".page" `shouldReturn` "page B"
    answerAsk gate "load a" "page A"
    holding
    T.textOf view ".page" `shouldReturn` "page B"
    T.outputs view `shouldReturn` ["b"]
    -- A new draft cancels the preview still out for the old one.
    T.typeInto view "input.draft" "x"
    holding
    T.typeInto view "input.draft" "xy"
    holding
    asked gate `shouldReturn` ["preview xy"]
    answerAsk gate "preview xy" "<xy>"
    T.within (T.html view) (T.settle view)
    T.textOf view ".preview" `shouldReturn` "<xy>"

harnessSpec :: Spec
harnessSpec = describe "on the harness" $ do
  it "steps an event dispatched from inside a step after the step's effects" dispatched
  it "runs the header's example: two loads answered in the other order, a preview cancelled" example

#else

harnessSpec :: Spec
harnessSpec = xdescribe "on the harness (native only)" $ pure ()

#endif

spec :: Spec
spec = describe "Halogen.Machine" $ do
  describe "a step" $ do
    it "runs its effects in order, in place: an event from another thread is stepped after them" inOrder
    it "starts an ask's work, whose answer is a later step that sees the events between" asks
    it "runs to completion: an event an effect brings is stepped after the step's effects" toCompletion
    it "drops the rest of a step whose effect fails, and still steps the next event" failingNow
    it "delivers a failing ask's failure as its answer" failingAsk
  describe "Cancel" $ do
    it "stops an ask: its answer is never stepped" cancelled
    it "stops an ask whose work ended but whose answer is not stepped yet" cancelledAfterTheWork
    it "stops every ask under the key, and no other" cancelsEveryAskUnderTheKey
    it "of its own key, in an answer's step, lets that step run to its end" cancelsItsOwnKey
  describe "a pause" $ do
    it "under a key, cancelled before it ends, never fires; the one that ends fires once" debounced
    it "that is not counted lets the tree read as quiet while it sleeps" notCounted
  describe "children and lifetimes" $ do
    it "sends to a child, and asks a child" child
    it "keeps a resource made in one step, and releases it in the finalizer" resources
    it "steps the finalizer's event when the component went away in the middle of a step" cutShort
  it "keeps the model and the order of effects in every order of up to five moves" everyOrder
  harnessSpec
