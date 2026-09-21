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
  , HookFn
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
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.NT (type (~>) (NT))
import Halogen.HTML.Core qualified as HC
import Halogen.Hooks.Hook (Hook (..))
import Halogen.Hooks.HookM (HookAction, HookF (..), HookHTML, HookM (..))
import Halogen.Hooks.Internal.Cells
import Halogen.Hooks.Types (StateId (..))
import Halogen.Query.HalogenM qualified as HM
import Halogen.Query.HalogenQ (HalogenQ (..))
import Protolude hiding (get, gets, modify, put, state)

-- | The handler a 'Halogen.Hooks.useQuery' installed, kept under a newtype
-- because it is a rank-2 function. Its @q@ is the component's own query
-- algebra: a hook program is indexed by the algebra it answers, so the handler
-- is stored and applied at one type, with no coercion in between.
newtype QueryHandler q slots output m = QueryHandler (forall a. q a -> HookM slots output m (Maybe a))

-- | Everything a hooks component knows that is not its rendered HTML.
--
-- Behind an 'IORef' rather than in the component state, because changing the
-- component state is how a render is requested: a write here (a state cell
-- changed, a query handler replaced) must not be one by itself.
data Internal q input slots output m hooks = Internal
  { input :: input
  , cells :: Maybe (Cells slots output m hooks)
  , queryHandler :: Maybe (QueryHandler q slots output m)
  , dirty :: Bool
  }

-- | The component state of a hooks component.
data HookState q input slots output m hooks = HookState
  { result :: HookHTML slots output m
  , internal :: IORef (Internal q input slots output m hooks)
  }

-- | What a hooks component is written as: one function from input to HTML,
-- consuming exactly the hooks it declares.
type HookFn q input slots output m hooks =
  input -> Hook q slots output m hooks '[] (HookHTML slots output m)

-- | The monad the interpreter works in — the component's own 'HM.HalogenM'.
type Eval q input slots output m hooks =
  HM.HalogenM (HookState q input slots output m hooks) (HookAction slots output m) slots output m

-- | The state a component starts in: no cells yet, and nothing rendered.
--
-- The driver renders once before it runs the initializer, and the hook program
-- cannot run before then, so there is one render of nothing at the start of
-- every hooks component's life.
initialHookState :: forall q input slots output m hooks. (MonadIO m) => input -> m (HookState q input slots output m hooks)
initialHookState i = do
  ref <- liftIO $ newIORef Internal {input = i, cells = Nothing, queryHandler = Nothing, dirty = False}
  pure HookState {result = HC.text "", internal = ref}

-- | The component's @eval@.
evalHook
  :: forall q input slots output m hooks
   . (MonadIO m)
  => HookFn q input slots output m hooks
  -> HalogenQ q (HookAction slots output m) input ~> Eval q input slots output m hooks
evalHook hookFn = NT $ \case
  Initialize a ->
    runHooks hookFn $> a
  Receive i a -> do
    modifyInternal $ \int -> int {input = i}
    runHooks hookFn $> a
  Action act a ->
    interpretHookM act *> settle hookFn $> a
  Query (Coyoneda req fct) f -> do
    int <- readInternal
    case int.queryHandler of
      Nothing -> pure (f ())
      Just (QueryHandler handler) -> do
        result <- interpretHookM (handler fct)
        settle hookFn
        pure $ maybe (f ()) req result
  Finalize a -> do
    int <- readInternal
    for_ int.cells $ \cs ->
      traverse_ interpretHookM =<< liftIO (cleanups cs)
    pure a

-- | Run the hook program: build or step the cells, render what it produced,
-- then run the effects it asked for and settle whatever they changed.
runHooks :: forall q input slots output m hooks. (MonadIO m) => HookFn q input slots output m hooks -> Eval q input slots output m hooks ()
runHooks hookFn = do
  int <- readInternal
  modifyInternal $ \i -> i {dirty = False}
  (html, effects) <- case int.cells of
    Nothing -> do
      (html, mkCells, effects) <- buildHooks (hookFn int.input)
      modifyInternal $ \i -> i {cells = Just (mkCells CNil)}
      pure (html, effects)
    Just cs -> do
      (html, leftover, effects) <- stepHooks cs (hookFn int.input)
      case leftover of
        CNil -> pure (html, effects)
  State.modify $ \st -> st {result = html}
  sequence_ effects
  settle hookFn

-- | Run the program again if anything it or its effects did asked for it.
settle :: forall q input slots output m hooks. (MonadIO m) => HookFn q input slots output m hooks -> Eval q input slots output m hooks ()
settle hookFn = do
  int <- readInternal
  when int.dirty $ runHooks hookFn

-- | The first pass: the cells do not exist yet, so each hook makes its own.
--
-- The cells are returned as a function from what follows the program to what
-- the program leaves — a difference list, so that a sequence of hooks composes
-- as function composition while every cell keeps its own type.
buildHooks
  :: forall q input slots output m hooks i o a
   . (MonadIO m)
  => Hook q slots output m i o a
  -> Eval q input slots output m hooks (a, Cells slots output m o -> Cells slots output m i, [Eval q input slots output m hooks ()])
buildHooks = \case
  HPure a -> pure (a, identity, [])
  HBind ma k -> do
    (a, cellsA, effectsA) <- buildHooks ma
    (b, cellsB, effectsB) <- buildHooks (k a)
    pure (b, cellsA . cellsB, effectsA <> effectsB)
  HState s -> do
    ref <- liftIO $ newIORef s
    pure ((s, StateId ref), CState ref, [])
  HEffect deps effect -> do
    ref <- liftIO $ newIORef EffectCell {deps, cleanup = Nothing}
    pure ((), CEffect ref, [runEffect ref deps effect])
  HMemo deps f -> do
    let a = f deps
    ref <- liftIO $ newIORef (deps, a)
    pure (a, CMemo ref, [])
  HRef a -> do
    ref <- liftIO $ newIORef a
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
  :: forall q input slots output m hooks i o a
   . (MonadIO m)
  => Cells slots output m i
  -> Hook q slots output m i o a
  -> Eval q input slots output m hooks (a, Cells slots output m o, [Eval q input slots output m hooks ()])
stepHooks cs = \case
  HPure a -> pure (a, cs, [])
  HBind ma k -> do
    (a, cs', effectsA) <- stepHooks cs ma
    (b, cs'', effectsB) <- stepHooks cs' (k a)
    pure (b, cs'', effectsA <> effectsB)
  HState _ -> case cs of
    CState ref rest -> do
      s <- liftIO $ readIORef ref
      pure ((s, StateId ref), rest, [])
  HEffect deps effect -> case cs of
    CEffect ref rest -> do
      cell <- liftIO $ readIORef ref
      pure ((), rest, [runEffect ref deps effect | cell.deps /= deps])
  HMemo deps f -> case cs of
    CMemo ref rest -> do
      (deps', a') <- liftIO $ readIORef ref
      if deps' == deps
        then pure (a', rest, [])
        else do
          let a = f deps
          liftIO $ writeIORef ref (deps, a)
          pure (a, rest, [])
  HRef _ -> case cs of
    CRef ref rest -> do
      a <- liftIO $ readIORef ref
      pure ((a, ref), rest, [])
  HQuery handler -> case cs of
    CQuery rest -> do
      modifyInternal $ \i -> i {queryHandler = Just (QueryHandler handler)}
      pure ((), rest, [])

-- | Clean up after the previous run of an effect, run it, and keep whatever
-- cleanup it left for next time.
runEffect
  :: forall q input slots output m hooks deps
   . (MonadIO m)
  => IORef (EffectCell deps slots output m)
  -> deps
  -> HookM slots output m (Maybe (HookAction slots output m))
  -> Eval q input slots output m hooks ()
runEffect ref deps effect = do
  cell <- liftIO $ readIORef ref
  traverse_ interpretHookM cell.cleanup
  cleanup <- interpretHookM effect
  liftIO $ writeIORef ref EffectCell {deps, cleanup}

-- | Interpret a hook program's instructions as the component's @HalogenM@.
--
-- Nearly everything is the corresponding @HalogenM@ instruction; the one hook
-- idea is state, which lives in the cell the 'StateId' points at rather than
-- in the component state, and so has to say for itself that a render is due.
interpretHookM :: forall q input slots output m hooks a. (MonadIO m) => HookM slots output m a -> Eval q input slots output m hooks a
interpretHookM (HookM program) = foldF go program
  where
    go :: forall x. HookF slots output m x -> Eval q input slots output m hooks x
    go = \case
      Lift mx k -> map k (lift mx)
      State (StateId ref) f k -> do
        (x, changed) <- liftIO $ atomicModifyIORef' ref $ \s ->
          let (x, s') = f s in (s', (x, not (unsafeRefEq s s')))
        when changed $ modifyInternal $ \i -> i {dirty = True}
        pure (k x)
      Raise o a -> HM.raise o $> a
      ChildQuery cq -> HM.HalogenM $ liftF $ HM.ChildQuery cq
      Subscribe esc k -> HM.HalogenM $ liftF $ HM.Subscribe esc k
      Unsubscribe sid a -> HM.unsubscribe sid $> a
      Fork hm k -> map k $ HM.fork $ interpretHookM hm
      Kill fid a -> HM.kill fid $> a
      GetRef label k -> map k $ HM.getRef label

readInternal :: forall q input slots output m hooks. (MonadIO m) => Eval q input slots output m hooks (Internal q input slots output m hooks)
readInternal = do
  st <- State.get
  liftIO $ readIORef st.internal

modifyInternal
  :: forall q input slots output m hooks
   . (MonadIO m)
  => (Internal q input slots output m hooks -> Internal q input slots output m hooks)
  -> Eval q input slots output m hooks ()
modifyInternal f = do
  st <- State.get
  liftIO $ modifyIORef' st.internal f
