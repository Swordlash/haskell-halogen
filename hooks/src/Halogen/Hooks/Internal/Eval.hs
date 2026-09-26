-- | Running a hook program as an ordinary Halogen component.
--
-- The shape of the thing: a hooks component's state is one record holding the
-- HTML of the last render, so @render@ is a field access, and everything that
-- actually varies lives in cells behind an 'IORef'. Rendering is not a pure
-- function of the state here — a hook program asks for effects as it goes —
-- so the program is run during @eval@ instead, and the HTML it produces is
-- written to the state, which is what asks the driver for a render.
--
-- After that render, the effects the pass collected are run, and if any of
-- them changed state the program is run again ('settle'), exactly as the
-- PureScript original does.
module Halogen.Hooks.Internal.Eval
  ( HookState (..)
  , Internal (..)
  , QueryHandler (..)
  , Eval
  , initialHookState
  , evalHook
  , interpretHookM
  )
where

import Control.Monad.Free.Church (foldF, liftF)
import Control.Monad.State.Class qualified as State
import Data.Foreign (unsafeRefEq)
import Data.Functor.Coyoneda (Coyoneda (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.NT (type (~>) (NT))
import Halogen.HTML.Core qualified as HC
import Halogen.Hooks.Internal.Cells
import Halogen.Hooks.Internal.Hook (Hook (..), HookFn)
import Halogen.Hooks.Internal.HookM (HookAction, HookF (..), HookHTML, HookM (..))
import Halogen.Hooks.Internal.Types (StateId (..))
import Halogen.Query.HalogenM qualified as HM
import Halogen.Query.HalogenQ (HalogenQ (..))
import Protolude hiding (get, gets, modify, put, state)
import Control.Monad.Catch qualified as Catch

-- | The handler a 'Halogen.Hooks.useQuery' installed, kept under a newtype
-- because it is a rank-2 function. Its @q@ is the component's own query
-- algebra: a hook program is indexed by the algebra it answers, so the handler
-- is stored and applied at one type, with no coercion in between.
newtype QueryHandler scope q slots output m = QueryHandler (forall a. q a -> HookM scope slots output m (Maybe a))

-- | Everything a hooks component knows that is not its rendered HTML.
--
-- Behind an 'IORef' rather than in the component state, because changing the
-- component state is how a render is requested: a write here (a state cell
-- changed, a query handler replaced) must not be one by itself.
data Internal scope q input slots output m hooks = Internal
  { hookFn :: HookFn scope q input slots output m hooks
  -- ^ The program itself, so that anything holding the state can run it
  -- again. A forked program is the reason it has to live here: it settles
  -- what it changed from a thread of its own, with nothing passed down to it.
  , input :: input
  , cells :: Maybe (Cells scope slots output m hooks)
  , queryHandler :: Maybe (QueryHandler scope q slots output m)
  , dirty :: Bool
  , running :: Bool
  -- ^ A render and its effects form one pass. Reentrant requests only mark
  -- another pass due, so an older effect cannot overwrite a newer cleanup.
  }

-- | The component state of a hooks component.
data HookState scope q input slots output m hooks = HookState
  { result :: HookHTML scope slots output m
  , internal :: IORef (Internal scope q input slots output m hooks)
  }

-- | The monad the interpreter works in — the component's own 'HM.HalogenM'.
type Eval scope q input slots output m hooks =
  HM.HalogenM (HookState scope q input slots output m hooks) (HookAction scope slots output m) slots output m

-- | The state a component starts in: no cells yet, and nothing rendered.
--
-- The driver renders once before it runs the initializer, and the hook program
-- cannot run before then, so there is one render of nothing at the start of
-- every hooks component's life.
initialHookState
  :: forall scope q input slots output m hooks
   . (MonadIO m)
  => HookFn scope q input slots output m hooks
  -> input
  -> m (HookState scope q input slots output m hooks)
initialHookState hookFn i = do
  ref <- liftIO $ newIORef Internal {hookFn, input = i, cells = Nothing, queryHandler = Nothing, dirty = False, running = False}
  pure HookState {result = HC.text "", internal = ref}

-- | The component's @eval@.
evalHook
  :: forall scope q input slots output m hooks
   . (MonadIO m)
  => HalogenQ q (HookAction scope slots output m) input ~> Eval scope q input slots output m hooks
evalHook = NT $ \case
  Initialize a -> do
    modifyInternal $ \int -> int {dirty = True}
    settle $> a
  Receive i a -> do
    modifyInternal $ \int -> int {input = i, dirty = True}
    settle $> a
  Action act a ->
    interpretHookM act *> settle $> a
  Query (Coyoneda req fct) f -> do
    int <- readInternal
    case int.queryHandler of
      Nothing -> pure (f ())
      Just (QueryHandler handler) -> do
        result <- interpretHookM (handler fct)
        settle
        pure $ maybe (f ()) req result
  Finalize a -> do
    int <- readInternal
    for_ int.cells $ \cs ->
      traverse_ interpretHookM =<< atOnce (cleanups cs)
    pure a

-- | Run the hook program: build or step the cells, render what it produced,
-- then run the effects it asked for. 'settle' owns the pass and decides whether
-- another is needed, including requests arriving while the effects run.
runHooks :: forall scope q input slots output m hooks. (MonadIO m) => Internal scope q input slots output m hooks -> Eval scope q input slots output m hooks ()
runHooks int = do
  (html, effects) <- case int.cells of
    Nothing -> do
      (html, mkCells, effects) <- buildHooks (int.hookFn int.input)
      modifyInternal $ \i -> i {cells = Just (mkCells CNil)}
      pure (html, effects)
    Just cs -> do
      (html, leftover, effects) <- stepHooks cs (int.hookFn int.input)
      case leftover of
        CNil -> pure (html, effects)
  State.modify $ \st -> st {result = html}
  sequence_ effects

-- | Run the program again if anything it or its effects did asked for it.
--
-- One pass at a time: a pass whose effects wait (in 'liftIO') lets the tree
-- go on, and a request made meanwhile marks the program dirty for the pass
-- in progress to run again rather than start one of its own.
settle :: forall scope q input slots output m hooks. (MonadIO m) => Eval scope q input slots output m hooks ()
settle = do
  st <- State.get
  firstPass <- atOnce $ atomicModifyIORef' st.internal $ \int ->
    if int.running || not int.dirty
      then (int, Nothing)
      else (int {running = True, dirty = False}, Just int)
  let loop int = do
        runHooks int
        -- Release ownership and check for another request in the same atomic
        -- operation: a fork must not lose a write at the end of a pass.
        next <- atOnce $ atomicModifyIORef' st.internal $ \current ->
          if current.dirty
            then (current {dirty = False}, Just current)
            else (current {running = False}, Nothing)
        traverse_ loop next
  -- A pass that fails gives the program up; what was asked for meanwhile
  -- gets a pass of its own, and the failure goes on to be reported.
  for_ firstPass $ \int ->
    loop int `Catch.onException` do
      pending <- atOnce $ atomicModifyIORef' st.internal $ \current -> (current {running = False}, current.dirty)
      when pending $ void $ HM.fork settle

-- | The first pass: the cells do not exist yet, so each hook makes its own.
--
-- The cells are returned as a function from what follows the program to what
-- the program leaves — a difference list, so that a sequence of hooks composes
-- as function composition while every cell keeps its own type.
buildHooks
  :: forall scope q input slots output m hooks i o a
   . (MonadIO m)
  => Hook scope q slots output m i o a
  -> Eval scope q input slots output m hooks (a, Cells scope slots output m o -> Cells scope slots output m i, [Eval scope q input slots output m hooks ()])
buildHooks = \case
  HPure a -> pure (a, identity, [])
  HBind ma k -> do
    (a, cellsA, effectsA) <- buildHooks ma
    (b, cellsB, effectsB) <- buildHooks (k a)
    pure (b, cellsA . cellsB, effectsA <> effectsB)
  HState s -> do
    ref <- atOnce $ newIORef s
    pure ((s, StateId ref), CState ref, [])
  HEffect _same deps effect -> do
    ref <- atOnce $ newIORef EffectCell {deps, cleanup = Nothing}
    pure ((), CEffect ref, [runEffect ref deps effect])
  HMemo _same deps f -> do
    let a = f deps
    ref <- atOnce $ newIORef (deps, a)
    pure (a, CMemo ref, [])
  HRef a -> do
    ref <- atOnce $ newIORef a
    pure ((a, ref), CRef ref, [])
  HQuery handler -> do
    modifyInternal $ \i -> i {queryHandler = Just (QueryHandler handler)}
    pure ((), CQuery, [])

-- | Every later pass: the cells exist, and the program walks them in step.
--
-- Which cell belongs to which hook is settled by the type — the program and
-- the store are indexed by the same list — so there is no cursor to keep and
-- nothing to check at runtime.
stepHooks
  :: forall scope q input slots output m hooks i o a
   . (MonadIO m)
  => Cells scope slots output m i
  -> Hook scope q slots output m i o a
  -> Eval scope q input slots output m hooks (a, Cells scope slots output m o, [Eval scope q input slots output m hooks ()])
stepHooks cs = \case
  HPure a -> pure (a, cs, [])
  HBind ma k -> do
    (a, cs', effectsA) <- stepHooks cs ma
    (b, cs'', effectsB) <- stepHooks cs' (k a)
    pure (b, cs'', effectsA <> effectsB)
  HState _ -> case cs of
    CState ref rest -> do
      s <- atOnce $ readIORef ref
      pure ((s, StateId ref), rest, [])
  -- The comparison comes from this render's hook, not the cell: what a cell
  -- remembers is the dependencies it last ran on.
  HEffect same deps effect -> case cs of
    CEffect ref rest -> do
      cell <- atOnce $ readIORef ref
      pure ((), rest, [runEffect ref deps effect | not (same cell.deps deps)])
  HMemo same deps f -> case cs of
    CMemo ref rest -> do
      (deps', a') <- atOnce $ readIORef ref
      if same deps' deps
        then pure (a', rest, [])
        else do
          let a = f deps
          atOnce $ writeIORef ref (deps, a)
          pure (a, rest, [])
  HRef _ -> case cs of
    CRef ref rest -> do
      a <- atOnce $ readIORef ref
      pure ((a, ref), rest, [])
  HQuery handler -> case cs of
    CQuery rest -> do
      modifyInternal $ \i -> i {queryHandler = Just (QueryHandler handler)}
      pure ((), rest, [])

-- | Clean up after the previous run of an effect, run it, and keep whatever
-- cleanup it left for next time.
runEffect
  :: forall scope q input slots output m hooks deps
   . (MonadIO m)
  => IORef (EffectCell scope deps slots output m)
  -> deps
  -> HookM scope slots output m (Maybe (HookAction scope slots output m))
  -> Eval scope q input slots output m hooks ()
runEffect ref deps effect = do
  cell <- atOnce $ readIORef ref
  traverse_ interpretHookM cell.cleanup
  cleanup <- interpretHookM effect
  atOnce $ writeIORef ref EffectCell {deps, cleanup}

-- | Interpret a hook program's instructions as the component's @HalogenM@.
--
-- Nearly everything is the corresponding @HalogenM@ instruction; the one hook
-- idea is state, which lives in the cell the 'StateId' points at rather than
-- in the component state, and so has to say for itself that a render is due.
interpretHookM :: forall scope q input slots output m hooks a. (MonadIO m) => HookM scope slots output m a -> Eval scope q input slots output m hooks a
interpretHookM = interpretHookMWith False

-- Ordinary actions batch their writes until they return. Forks can live
-- forever, so each of their state writes must request a pass immediately.
interpretHookMWith :: forall scope q input slots output m hooks a. (MonadIO m) => Bool -> HookM scope slots output m a -> Eval scope q input slots output m hooks a
interpretHookMWith settleWrites (HookM program) = foldF go program
  where
    go :: forall x. HookF scope slots output m x -> Eval scope q input slots output m hooks x
    go = \case
      Lift mx k -> map k (lift mx)
      LiftEffect mx k -> map k (HM.liftEffect mx)
      State (StateId ref) f k -> do
        (x, changed) <- atOnce $ atomicModifyIORef' ref $ \s ->
          let (x, s') = f s in (s', (x, not (unsafeRefEq s s')))
        when changed $ do
          modifyInternal $ \i -> i {dirty = True}
          when settleWrites settle
        pure (k x)
      Raise o a -> HM.raise o $> a
      ChildQuery cq -> HM.HalogenM $ liftF $ HM.ChildQuery cq
      Subscribe esc k -> HM.HalogenM $ liftF $ HM.Subscribe esc k
      Unsubscribe sid a -> HM.unsubscribe sid $> a
      -- A fork runs on its own, so nothing else is going to notice what it
      -- changed: it has to run the program again itself.
      Fork hm k -> map k $ HM.fork $ interpretHookMWith True hm
      Kill fid a -> HM.kill fid $> a
      GetRef label k -> map k $ HM.getRef label

readInternal :: forall scope q input slots output m hooks. (MonadIO m) => Eval scope q input slots output m hooks (Internal scope q input slots output m hooks)
readInternal = do
  st <- State.get
  atOnce $ readIORef st.internal

modifyInternal
  :: forall scope q input slots output m hooks
   . (MonadIO m)
  => (Internal scope q input slots output m hooks -> Internal scope q input slots output m hooks)
  -> Eval scope q input slots output m hooks ()
modifyInternal f = do
  st <- State.get
  atOnce $ atomicModifyIORef' st.internal (\int -> (f int, ()))

-- | The hooks' own bookkeeping: at once, not a place where the program may
-- wait (see 'HM.liftEffect').
atOnce :: forall state action slots output m a. (MonadIO m) => IO a -> HM.HalogenM state action slots output m a
atOnce = HM.liftEffect . liftIO
