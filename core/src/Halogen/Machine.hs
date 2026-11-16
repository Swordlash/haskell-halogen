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
-- * An ask ('Ask'): work that takes time, such as a request. The work
--   starts in the step. Its answer comes back later as an event, and the
--   step handles it like any other event. A failure is an answer too.
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
-- > import Halogen qualified as H
-- > import Halogen.Latest (Ask, Asks, newAsk, noAsks, supersede, takes)
-- > import Halogen.Machine qualified as M
-- > import Halogen.Sync qualified as Sync
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
-- >       let page = either (Text.pack . displayException) identity answer
-- >        in (model {page = Just page, loads}, [Announce model.slug])
-- >   Typed draft -> (model, [StopPreview, Preview draft])
-- >   Previewed answer -> (model, [Display (either (const "") identity answer)])
-- >
-- > -- The component. It keeps the machine in its state.
-- > data Key = Previewing
-- >   deriving (Eq, Ord)
-- >
-- > newtype State = State {machine :: M.Machine Key Event Model}
-- >
-- > data Action = Type Text
-- >
-- > type Slots = ("preview" .== H.Slot Preview.Query Void ())
-- >
-- > pageView :: (MonadIO m) => H.Component q Text Text m
-- > pageView =
-- >   H.component
-- >     (\slug -> pure State {machine = M.new Model {slug, page = Nothing, loads = noAsks}})
-- >     render
-- >     [ H.onInitialize (gets (\s -> (M.current s.machine).slug) >>= happen . Opened)
-- >     , H.onReceive (happen . Opened)
-- >     , H.onAction (\(Type draft) -> happen (Typed draft))
-- >     ]
-- >   where
-- >     happen = M.happen M.Spec {step, carry, get = (.machine), set = \machine s -> s {machine}}
-- >     carry = \case
-- >       Load ticket slug -> M.Ask (H.async (liftIO (fetchPage slug))) (Loaded ticket)
-- >       Preview draft -> M.AskUnder Previewing (H.async (liftIO (renderPreview draft))) Previewed
-- >       StopPreview -> M.Cancel Previewing
-- >       Display shown -> M.Now (Sync.send "preview" () (Preview.Show shown))
-- >       Announce slug -> M.Now (Sync.raise slug)
--
-- The runner does not know tickets. The effect carries the ticket, and the
-- answer's event carries it back: @Load ticket slug@ becomes
-- @M.Ask … (Loaded ticket)@. Render with @M.current state.machine@.
--
-- An ask of a child is an ask like any other: its work is
-- @H.request "editor" () Editor.GetText@, and its answer is the child's
-- 'Maybe'. A send to a child does not wait, so it is done in place:
-- @M.Now (Sync.send "editor" () Editor.Save)@.
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
  , happen
  , finish
  )
where

import Control.Monad.Catch (throwM)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Sequence (ViewL (..), viewl, (|>))
import Data.Sequence qualified as Seq
import HPrelude hiding (async, catch, finally, get, try)
import Halogen.Query.HalogenM (ForkId, HalogenM, fork, kill, try)
import Halogen.Sync (MonadSync)

-- | What a component keeps in its state for one machine: the model, and
-- what the runner needs. Make one with 'new', and read the model with
-- 'current'.
data Machine key ev model = Machine
  { modelNow :: model
  , stepping :: Bool
  -- ^ Whether a step's effects are being carried out.
  , waiting :: Seq ev
  -- ^ Events that came while a step's effects were carried out, oldest
  -- first. They are stepped once those effects are done.
  , asksOut :: Map key (IntMap (Maybe ForkId))
  -- ^ The asks out under each key, by number. An ask's fork is 'Nothing'
  -- until 'fork' returns it.
  , next :: Int
  -- ^ The next ask's number.
  }

-- | A machine with this model, before any event.
new :: model -> Machine key ev model
new model = Machine {modelNow = model, stepping = False, waiting = Seq.empty, asksOut = Map.empty, next = 0}

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
  -- function makes. The work is a program of the component: @H.async io@
  -- for an action of the component's monad, or @H.request@ for a child.
  -- What the work throws is the answer too, as 'Left'. Nothing cancels this
  -- ask: a write cannot be taken back, so its answer always comes. Only the
  -- component going away stops it.
  Ask :: HalogenM st act slots out m a -> (Either SomeException a -> ev) -> Carry key st act slots out m ev
  -- | 'Ask', under a key, so that 'Cancel' can stop it. Several asks can be
  -- out under one key. A new ask does not stop the older ones: the step
  -- decides, with 'Cancel', or with tickets ("Halogen.Latest").
  AskUnder :: key -> HalogenM st act slots out m a -> (Either SomeException a -> ev) -> Carry key st act slots out m ev
  -- | Stop every ask out under this key. Their work is killed, and their
  -- answers are never stepped, even an answer that has come but has not
  -- been stepped yet. An ask whose answer is being stepped is not out any
  -- more, so a step can cancel its own key and still run to its end.
  Cancel :: key -> Carry key st act slots out m ev

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
happen :: (Ord key, Functor m) => Spec key st act slots out m ev fx model -> ev -> HalogenM st act slots out m ()
happen spec = arrive spec False Just

-- | 'happen', for 'Halogen.Component.onFinalize': it steps the event even
-- when a step was cut short because the component went away in the middle
-- of it. The events that step queued are stepped first.
--
-- > H.onFinalize (M.finish machine Finished)
--
-- In a finalizer, 'Now' effects run as usual: release resources there. An
-- ask's work starts and is killed as soon as the finalizer ends. Its answer
-- is never stepped. Asks started earlier were cancelled when the component
-- began to go away, and their answers never come either.
finish :: (Ord key, Functor m) => Spec key st act slots out m ev fx model -> ev -> HalogenM st act slots out m ()
finish spec = arrive spec True Just

-- | Bring an event to the machine. @admit@ changes the machine first, in
-- the same step, or drops the event ('Nothing'). With @anyway@ the event is
-- stepped even while a step's effects are carried out.
arrive
  :: (Ord key, Functor m)
  => Spec key st act slots out m ev fx model
  -> Bool
  -> (Machine key ev model -> Maybe (Machine key ev model))
  -> ev
  -> HalogenM st act slots out m ()
arrive spec anyway admit ev = do
  carries <- state $ \st -> case admit (spec.get st) of
    Nothing -> (Nothing, st)
    Just m
      | m.stepping && not anyway -> (Nothing, spec.set m {waiting = m.waiting |> ev} st)
      | otherwise -> advance spec st m {waiting = m.waiting |> ev}
  carryFrom spec Nothing carries

-- | Step the waiting events up to the first whose step has effects, and
-- return those effects. 'Nothing' when no event is waiting: the machine is
-- then idle.
advance :: Spec key st act slots out m ev fx model -> st -> Machine key ev model -> (Maybe [Carry key st act slots out m ev], st)
advance spec st m = case viewl m.waiting of
  EmptyL -> (Nothing, spec.set m {stepping = False} st)
  ev :< rest -> case spec.step ev m.modelNow of
    (model, []) -> advance spec st m {modelNow = model, waiting = rest}
    (model, fx) -> (Just (map spec.carry fx), spec.set m {modelNow = model, waiting = rest, stepping = True} st)

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
  Ask work answer -> void $ fork $ try work >>= happen spec . answer
  AskUnder key work answer -> do
    n <- state $ \st ->
      let m = spec.get st
       in (m.next, spec.set m {next = m.next + 1, asksOut = Map.insertWith IntMap.union key (IntMap.singleton m.next Nothing) m.asksOut} st)
    -- The ask leaves the map before its answer is stepped, in the same
    -- step: then a 'Cancel' of its own key in that step does not kill the
    -- fork that carries the step out.
    fid <- fork $ try work >>= arrive spec False (leave key n) . answer
    -- The fork may have answered already, if its work did not suspend.
    -- Then it has left, and nothing is recorded.
    modify $ \st -> let m = spec.get st in spec.set m {asksOut = Map.adjust (IntMap.adjust (const (Just fid)) n) key m.asksOut} st
  Cancel key -> do
    forks <- state $ \st ->
      let m = spec.get st
       in (Map.findWithDefault IntMap.empty key m.asksOut, spec.set m {asksOut = Map.delete key m.asksOut} st)
    traverse_ kill (catMaybes (IntMap.elems forks))

-- | Take an answered ask out of the map. 'Nothing' when it is not there:
-- it was cancelled, and its answer is dropped.
leave :: (Ord key) => key -> Int -> Machine key ev model -> Maybe (Machine key ev model)
leave key n m = do
  under <- Map.lookup key m.asksOut
  guard (IntMap.member n under)
  let rest = IntMap.delete n under
  pure m {asksOut = if IntMap.null rest then Map.delete key m.asksOut else Map.insert key rest m.asksOut}
