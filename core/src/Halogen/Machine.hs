-- | Run a component whose logic is a pure step.
--
-- Some views are easier to get right as a pure function. The function takes
-- an event and the model, and returns the new model and a list of effects:
--
-- > step :: Event -> Model -> (Model, [Effect])
--
-- The step imports nothing of Halogen. A test can run it over any order of
-- events, with no component and no browser. The component only runs the
-- step and carries out the effects. This module is that runner: give it the
-- step and a way to carry out each effect, and call 'happen' with each event.
--
-- = The promise
--
-- The effects of one step do not interleave with later events. 'happen'
-- steps the model and carries out the step's effects, in the list's order,
-- with no other event of the machine in between. An effect is one of two
-- kinds:
--
-- * Done in place ('Now'): a change of the component's state, a raise, a
--   send to a child, an action of the component's monad that does not wait.
--   It is written against 'Halogen.Sync.MonadSync', so the compiler checks
--   that it cannot suspend.
--
-- * An ask ('Ask'): work that takes time, such as a request, a child's
--   answer or a timer. The work starts in the step. Its answer comes back
--   later as an event, and the step handles it like any other event. A
--   failure is an answer too. The work can only wait and answer: it cannot
--   change the component's state, raise or send ('Work').
--
-- A handler written by hand can break the promise. One effect that waits in
-- the middle of the list is enough: the effects after it run after later
-- events, on a model the step never saw. Here an effect cannot wait.
--
-- = An example
--
-- This view shows a page, and loads it again when its input changes. It
-- takes a ticket for each load ("Halogen.Latest"), so an older page never
-- replaces a newer one. It also shows a preview of what the reader types.
-- A new draft cancels the preview still out for the old one. The preview
-- goes to a child, and each page shown is raised to the parent.
--
-- > import Control.Exception (SomeException, displayException)
-- > import Data.Text (Text)
-- > import Data.Text qualified as Text
-- > import Halogen qualified as H
-- > import Halogen.Latest (Ask, Asks, newAsk, noAsks, supersede, takes)
-- > import Halogen.Machine qualified as M
-- > import Halogen.Sync qualified as Sync
-- >
-- > -- What the view asks of its monad.
-- > class (Monad m) => Pages m where
-- >   fetchPage :: Text -> m Text
-- >   renderPreview :: Text -> m Text
-- >
-- > -- The model and its step. Plain Haskell.
-- > data Model = Model {slug :: Text, page :: Maybe Text, loads :: Asks}
-- >
-- > data Event
-- >   = Opened Text
-- >   | Loaded Ask (Either SomeException Text)
-- >   | Typed Text
-- >   | Previewed (Either SomeException Text)
-- >
-- > data Effect = Load Ask Text | Preview Text | StopPreview | Display Text | Announce Text
-- >
-- > step :: Event -> Model -> (Model, [Effect])
-- > step event model = case event of
-- >   Opened slug ->
-- >     -- Another page: no answer for the old one may show.
-- >     let (ticket, loads) = newAsk (supersede model.loads)
-- >      in (model {slug, page = Nothing, loads}, [StopPreview, Load ticket slug])
-- >   Loaded ticket answer -> case takes ticket model.loads of
-- >     Nothing -> (model, [])
-- >     Just loads ->
-- >       let page = either (Text.pack . displayException) id answer
-- >        in (model {page = Just page, loads}, [Announce model.slug])
-- >   Typed draft -> (model, [StopPreview, Preview draft])
-- >   Previewed answer -> (model, [Display (either (const "") id answer)])
-- >
-- > -- The component. It keeps the machine in its state.
-- > data Key = Previewing
-- >   deriving (Eq, Ord)
-- >
-- > newtype State = State {machine :: M.Machine Key Event Model}
-- >
-- > newtype Action = Type Text
-- >
-- > type Slots = ("preview" .== H.Slot PreviewQuery Void ())
-- >
-- > pageView :: (Pages m) => H.Component q Text Text m
-- > pageView =
-- >   H.component
-- >     (\slug -> pure State {machine = M.new Model {slug, page = Nothing, loads = noAsks}})
-- >     render
-- >     [ H.onInitialize (gets (\s -> (M.current s.machine).slug) >>= happen . Opened)
-- >     , H.onReceive (happen . Opened)
-- >     , H.onAction (\(Type draft) -> happen (Typed draft))
-- >     ]
-- >   where
-- >     happen = M.happen M.Spec {M.step = step, M.carry = carry, M.get = (.machine), M.set = \machine s -> s {machine}}
-- >     carry = \case
-- >       Load ticket slug -> M.Ask (M.run (fetchPage slug)) (Loaded ticket)
-- >       Preview draft -> M.AskUnder Previewing (M.run (renderPreview draft)) Previewed
-- >       StopPreview -> M.Cancel Previewing
-- >       Display shown -> M.Now (Sync.send "preview" () (ShowPreview shown))
-- >       Announce slug -> M.Now (Sync.raise slug)
--
-- @render@ draws the page, an input whose @Type@ action brings each draft,
-- and a slot for @previewer@: a child that shows the text its query
-- @ShowPreview@ gives it.
--
-- The runner does not know tickets. The effect carries the ticket, and the
-- answer's event carries it back: @Load ticket slug@ becomes
-- @M.Ask … (Loaded ticket)@. Render with @M.current state.machine@.
--
-- An ask of a child is an ask like any other: its work is
-- @M.child "editor" () Editor.GetText@, and its answer is the child's
-- 'Maybe'. A send to a child does not wait, so it is done in place:
-- @M.Now (Sync.send "editor" () Editor.Save)@.
--
-- A timer is an ask too. A debounce asks for a pause under a key, and
-- cancels the key when the reader types again:
--
-- > Debounce -> M.AskUnder Debouncing (M.delayCounted 0.3) (const Fire)
-- > StopDebounce -> M.Cancel Debouncing
--
-- = Resources with a lifetime
--
-- A subscription or an observer is made and kept in one effect, under a
-- number the model chose. Then nothing can come between the two:
--
-- > Watch n -> M.Now $ do
-- >   sid <- Sync.subscribe changes
-- >   modify (\s -> s {watches = Map.insert n sid s.watches})
--
-- The model holds the number, and the component's state holds the handle.
-- Release what the model still holds when the component goes away: in
-- 'Halogen.Component.onFinalize', tell the model with 'finish', and have
-- its step return the effects that release each one.
--
-- = When not to use it
--
-- A view with one ask and no change of input is simpler as a plain
-- handler. Use this module when a view takes more than one ask, a push, or
-- a change of input, and its rules are hard to keep in a handler.
--
-- = Two lessons
--
-- Keep the state that effects write in the model. When an effect writes a
-- field of the component's state, the step cannot see what it overwrites,
-- and no test of the step can either. Make the field part of the model, and
-- make the reader's changes to it events.
--
-- An answer that is superseded can still carry something the model must
-- keep. A notification marked read stays read, whichever answer is taken.
-- So the runner delivers every answer, and the step decides what to take.
-- Only 'Cancel' stops an answer.
--
-- The names here are short, so import this module qualified.
module Halogen.Machine
  ( -- * The machine
    Machine
  , new
  , current

    -- * Running it
  , Spec (..)
  , Carry (..)

    -- * What an ask waits for
  , Work
  , run
  , child
  , delay
  , delayCounted
  , happen
  , finish
  )
where

import Control.Monad.Catch (throwM)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Row (HasType)
import Data.Sequence (ViewL (..), viewl, (|>))
import Data.Sequence qualified as Seq
import Data.Time (NominalDiffTime)
import HPrelude hiding (async, catch, finally, get, try)
import Halogen.Data.Slot (Slot)
import Halogen.Query (Request, request)
import Halogen.Query.HalogenM (ForkId, HalogenM, async, fork, kill, try)
import Halogen.Query.HalogenM qualified as HM
import Halogen.Sync (MonadSync)

-- | What a component keeps in its state for one machine: the model, and
-- what the runner needs. Make one with 'new', and read the model with
-- 'current'.
data Machine key ev model = Machine
  { modelNow :: model
  , stepping :: Bool
  -- ^ Whether a step's effects are being carried out.
  , waiting :: Seq (Arrived key ev)
  -- ^ What came while a step's effects were carried out, oldest first. It
  -- is stepped once those effects are done.
  , asksOut :: Map key (IntMap (Maybe ForkId))
  -- ^ The asks out under each key, by number. An ask's fork is 'Nothing'
  -- until 'fork' returns it.
  , next :: Int
  -- ^ The next ask's number.
  , finishing :: Bool
  -- ^ Whether 'finish' has been called: the component is going away. No
  -- ask is started any more, and no answer is stepped.
  }

-- | What comes to a machine to be stepped.
data Arrived key ev
  = -- | An event from a handler.
    Event ev
  | -- | An ask's answer. A keyed ask is named: its answer is stepped only if
    -- the ask is still out then, so that a 'Cancel' can stop an answer
    -- that has come and waits.
    Answer (Maybe (key, Int)) ev

-- | A machine with this model, before any event.
new :: model -> Machine key ev model
new model = Machine {modelNow = model, stepping = False, waiting = Seq.empty, asksOut = Map.empty, next = 0, finishing = False}

-- | The model as the last step left it. Render from it.
current :: Machine key ev model -> model
current m = m.modelNow

-- | How one effect of a step is carried out.
--
-- @key@ names a kind of ask that 'Cancel' can stop. A machine with no such
-- ask can use @()@.
data Carry key st act slots out m ev where
  -- | Done in place, before the next effect. It cannot suspend: it is
  -- written against 'MonadSync', whose only instance is 'HalogenM'. It may
  -- change the component's state, raise an output, send to a child, or run
  -- an action of the component's monad that does not wait
  -- ('Halogen.Sync.sync').
  Now :: (forall h. (MonadSync st act slots out m h) => h ()) -> Carry key st act slots out m ev
  -- | Started now, in a fork; its answer is stepped later, as the event the
  -- function makes. What the work throws is the answer too, as 'Left'.
  -- Nothing cancels this ask: a write cannot be taken back, so its answer
  -- always comes. Only the component going away stops it.
  Ask :: Work slots m a -> (Either SomeException a -> ev) -> Carry key st act slots out m ev
  -- | 'Ask', under a key, so that 'Cancel' can stop it. Several asks can be
  -- out under one key. A new ask does not stop the older ones: the step
  -- decides, with 'Cancel', or with tickets ("Halogen.Latest").
  AskUnder :: key -> Work slots m a -> (Either SomeException a -> ev) -> Carry key st act slots out m ev
  -- | Stop every ask out under this key. Their work is killed, and their
  -- answers are never stepped, even an answer that has come but has not
  -- been stepped yet. An ask whose answer is being stepped is not out any
  -- more, so a step can cancel its own key and still run to its end.
  Cancel :: key -> Carry key st act slots out m ev

-- | What an ask waits for. Make one with 'run', 'child', 'delay' or
-- 'delayCounted', and change its answer with 'fmap'.
--
-- The work can only wait and answer. It cannot change the component's
-- state, raise an output or send to a child: such a write would happen
-- outside every step, where the model cannot see it. Write it as an effect
-- of the step that handles the answer.
newtype Work slots m a = Work (forall st act out. HalogenM st act slots out m a)

instance Functor (Work slots m) where
  fmap f (Work w) = Work (fmap f w)

-- | Run an action of the component's monad, as 'Halogen.Query.HalogenM.async'
-- does: on a thread of its own, while the rest of the tree goes on.
run :: (Functor m) => m a -> Work slots m a
run io = Work (async io)

-- | Ask a child, as 'Halogen.Query.request' does. The answer is 'Nothing'
-- when the slot is empty, or the child goes away before it answers.
child
  :: forall label
  ->forall slots m query output slot a
   . (HasType label (Slot query output slot) slots, KnownSymbol label, Ord slot, Functor m)
  => slot
  -> Request query a
  -> Work slots m (Maybe a)
child label slot req = Work (request label slot req)

-- | Pause for this long, in seconds, as 'Halogen.Query.HalogenM.delay'
-- does. A test that waits for the program to be quiet does not wait for
-- it. Use it for a pause after which nothing is fetched, such as hiding a
-- notice.
delay :: (Functor m) => NominalDiffTime -> Work slots m ()
delay d = Work (HM.delay d)

-- | Pause for this long, in seconds, as
-- 'Halogen.Query.HalogenM.delayCounted' does. A test that waits for the
-- program to be quiet waits for it. Use it for a pause before work, such as
-- a debounce before a request.
--
-- Both pauses sleep on a test's clock in a tree mounted under one
-- ('Halogen.Activity.Control.withTestClock').
delayCounted :: (Functor m) => NominalDiffTime -> Work slots m ()
delayCounted d = Work (HM.delayCounted d)

-- | A machine, and where the component keeps it.
data Spec key st act slots out m ev fx model = Spec
  { step :: ev -> model -> (model, [fx])
  -- ^ The pure step.
  , carry :: fx -> Carry key st act slots out m ev
  -- ^ How each effect is carried out.
  , get :: st -> Machine key ev model
  -- ^ Read the machine from the component's state.
  , set :: Machine key ev model -> st -> st
  -- ^ Write it back.
  }

-- | Step the model with this event, and carry out the step's effects. Call
-- it from the component's handlers.
--
-- What it guarantees:
--
-- * The model is stepped, and the step's effects carried out, with no other
--   event of this machine in between. Effects run in the list's order.
--
-- * An event that comes while a step's effects are carried out is stepped
--   after them, in the order the events came. Such an event comes from an
--   effect: a 'Halogen.Sync.sync' that focuses an element whose handler
--   calls 'happen', or a raise that the parent answers with a query. When
--   'happen' returns, every event that came meanwhile has been stepped, and
--   its effects carried out.
--
-- * An ask's work starts in its step. Its answer is a later step.
--
-- What it does not guarantee:
--
-- * That a 'Now' is quick. An effect that blocks holds the whole tree, as
--   'Halogen.Query.HalogenM.sync' does. The order is kept. When someone
--   listens to "Halogen.Trace", an effect that takes longer than
--   'Halogen.Trace.slowSyncThreshold' is reported as
--   'Halogen.Trace.SlowSync'.
--
-- * That the component's other programs wait. Only the events of this
--   machine are ordered. A handler that changes the state outside the
--   machine is not.
--
-- When a 'Now' throws, the rest of that step's effects are dropped. The
-- events that came meanwhile are still stepped. Then the exception goes on
-- up from 'happen', and the machine steps the next event as usual. If more
-- than one step fails, the first failure goes up.
--
-- A step is cut short when the component goes away in the middle of it:
-- for instance, a raise whose parent removes the component at once. A
-- killed program runs nothing more, so the rest of the step's effects never
-- run. Use 'finish' in the finalizer: 'happen' would only queue its event
-- behind the step that never ends.
--
-- A step is also cut short when a handler that one of its effects caused
-- kills the fork that carries the step out. For instance, a 'Now' moves the
-- focus, and the blur handler kills a ticker's fork, which is in the middle
-- of that step. Then the machine stays in the step for ever: it queues
-- every later event, and nothing says so. So do not kill a fork that calls
-- 'happen' on a machine from a handler that an effect of the same machine
-- can cause. Bring the stop to the machine as an event instead, and let
-- its step 'Cancel' what must stop.
happen :: (Ord key, Functor m) => Spec key st act slots out m ev fx model -> ev -> HalogenM st act slots out m ()
happen spec = arrive spec False . Event

-- | 'happen', for 'Halogen.Component.onFinalize': it steps the event even
-- when a step was cut short because the component went away in the middle
-- of it. The events that step queued are stepped first.
--
-- > H.onFinalize (M.finish machine Finished)
--
-- In a finalizer, 'Now' effects run as usual: release resources there. An
-- ask is not started: its work would be killed as soon as the finalizer
-- ends. No answer is stepped any more either, not even one that had come
-- and was waiting. Asks started earlier were cancelled when the component
-- began to go away. So after 'finish' nothing can take a resource again.
finish :: (Ord key, Functor m) => Spec key st act slots out m ev fx model -> ev -> HalogenM st act slots out m ()
finish spec = arrive spec True . Event

-- | Bring an event or an answer to the machine. It joins the ones that
-- wait. If no step's effects are being carried out, they are stepped now.
-- With @finishing@ they are stepped even so, and the machine is finishing
-- from then on.
arrive
  :: (Ord key, Functor m)
  => Spec key st act slots out m ev fx model
  -> Bool
  -> Arrived key ev
  -> HalogenM st act slots out m ()
arrive spec finishing arrived = do
  carries <- state $ \st ->
    let before = spec.get st
        m = before {waiting = before.waiting |> arrived, finishing = before.finishing || finishing}
     in if m.stepping && not finishing then (Nothing, spec.set m st) else advance spec st m
  carryFrom spec Nothing carries

-- | Step what waits, up to the first step that has effects, and return
-- those effects. 'Nothing' when nothing waits: the machine is then idle.
advance :: (Ord key) => Spec key st act slots out m ev fx model -> st -> Machine key ev model -> (Maybe [Carry key st act slots out m ev], st)
advance spec st m = case viewl m.waiting of
  EmptyL -> (Nothing, spec.set m {stepping = False} st)
  arrived :< rest -> case admit arrived m {waiting = rest} of
    Nothing -> advance spec st m {waiting = rest}
    Just (ev, admitted) -> case spec.step ev admitted.modelNow of
      (model, []) -> advance spec st admitted {modelNow = model}
      (model, fx) -> (Just (map spec.carry fx), spec.set admitted {modelNow = model, stepping = True} st)

-- | The event to step, or 'Nothing' for an answer that is dropped: its ask
-- was cancelled, or the machine is finishing. A keyed ask leaves the map
-- here, in the same step that steps its answer. Then a 'Cancel' of its own
-- key in that step does not kill the fork that carries the step out.
admit :: (Ord key) => Arrived key ev -> Machine key ev model -> Maybe (ev, Machine key ev model)
admit arrived m = case arrived of
  Event ev -> Just (ev, m)
  Answer _ _ | m.finishing -> Nothing
  Answer Nothing ev -> Just (ev, m)
  Answer (Just (key, n)) ev -> (ev,) <$> leave key n m

-- | Carry out a step's effects, then step the events that came meanwhile,
-- until none is waiting. The first failure goes on up at the end.
carryFrom
  :: (Ord key, Functor m)
  => Spec key st act slots out m ev fx model
  -> Maybe SomeException
  -> Maybe [Carry key st act slots out m ev]
  -> HalogenM st act slots out m ()
carryFrom spec failed = \case
  Nothing -> traverse_ throwM failed
  Just carries -> do
    result <- try (traverse_ (carryOut spec) carries)
    next <- state (\st -> advance spec st (spec.get st))
    carryFrom spec (failed <|> leftToMaybe result) next

carryOut :: (Ord key, Functor m) => Spec key st act slots out m ev fx model -> Carry key st act slots out m ev -> HalogenM st act slots out m ()
carryOut spec = \case
  Now effect -> effect
  Ask (Work work) answer -> unlessFinishing $ void $ fork $ try work >>= arrive spec False . Answer Nothing . answer
  AskUnder key (Work work) answer -> unlessFinishing $ do
    n <- state $ \st ->
      let m = spec.get st
       in (m.next, spec.set m {next = m.next + 1, asksOut = Map.insertWith IntMap.union key (IntMap.singleton m.next Nothing) m.asksOut} st)
    fid <- fork $ try work >>= arrive spec False . Answer (Just (key, n)) . answer
    -- If the work did not suspend, its answer has come already and waits
    -- to be stepped. The ask is still out until then, so a 'Cancel' can
    -- still stop it.
    modify $ \st -> let m = spec.get st in spec.set m {asksOut = Map.adjust (IntMap.adjust (const (Just fid)) n) key m.asksOut} st
  Cancel key -> do
    forks <- state $ \st ->
      let m = spec.get st
       in (Map.findWithDefault IntMap.empty key m.asksOut, spec.set m {asksOut = Map.delete key m.asksOut} st)
    traverse_ kill (catMaybes (IntMap.elems forks))
  where
    unlessFinishing start = gets (\st -> (spec.get st).finishing) >>= \finishing -> unless finishing start

-- | Take an answered ask out of the map. 'Nothing' when it is not there:
-- it was cancelled.
leave :: (Ord key) => key -> Int -> Machine key ev model -> Maybe (Machine key ev model)
leave key n m = do
  under <- Map.lookup key m.asksOut
  guard (IntMap.member n under)
  let rest = IntMap.delete n under
  pure m {asksOut = if IntMap.null rest then Map.delete key m.asksOut else Map.insert key rest m.asksOut}
