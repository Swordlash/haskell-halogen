module Halogen.IO.Driver
  ( RenderSpec (..)
  , runUI
  , HalogenSocket (..)
  )
where

import Control.Exception.Safe
import Control.Monad.Fork
import Control.Monad.Parallel
import Control.Monad.UUID
import Data.NT
import Data.Row
import HPrelude hiding (get)
import Halogen.Component
import Halogen.Data.Slot qualified as Slot
import Halogen.HTML.Core qualified as HC
import Halogen.IO.Driver.Eval qualified as Eval
import Halogen.IO.Driver.State
import Halogen.Query.HalogenQ qualified as HQ
import Halogen.Query.Input
import Halogen.Query.Input qualified as Input
import Halogen.Subscription qualified as HS
import Unsafe.Coerce (unsafeCoerce)

data HalogenSocket query output m = HalogenSocket
  { query :: forall a. query a -> m (Maybe a)
  , messages :: HS.Emitter m output
  , dispose :: m ()
  }

data RenderSpec (m :: Type -> Type) (r :: Type -> Type -> Row Type -> Type -> Type) = RenderSpec
  { render
      :: forall s act ps o
       . (Input act -> m ())
      -> (ComponentSlotBox ps m act -> m (RenderStateX r))
      -> HC.HTML (ComponentSlot ps m act) act
      -> Maybe (r s act ps o)
      -> m (r s act ps o)
  , renderChild :: forall s act ps o. r s act ps o -> r s act ps o
  , removeChild :: forall s act ps o. r s act ps o -> m ()
  , dispose :: forall s act ps o. r s act ps o -> m ()
  }

{-# SPECIALIZE runUI :: RenderSpec IO r -> Component f i o IO -> i -> IO (HalogenSocket f o IO) #-}
runUI
  :: forall m r f i o
   . (MonadUnliftIO m, MonadFork m, MonadKill m, MonadParallel m, MonadMask m, MonadUUID m)
  => RenderSpec m r
  -> Component f i o m
  -> i
  -> m (HalogenSocket f o m)
runUI RenderSpec {..} c i = do
  lchs <- newLifecycleHandlers
  disposed <- newIORef False
  Eval.handleLifecycle lchs $ do
    sio <- HS.create
    dsx@(DriverStateX st) <- readDriverStateRef =<< runComponent lchs (HS.notify sio.listener) i c
    pure
      $ HalogenSocket
        { query = evalDriver disposed st.selfRef
        , messages = sio.emitter
        , dispose = dispose' disposed lchs dsx
        }
  where
    evalDriver
      :: forall s f' act ps i' o'
       . IORef Bool
      -> IORef (DriverState m r s f' act ps i' o')
      -> (forall a. f' a -> m (Maybe a))
    evalDriver disposed ref q =
      readIORef disposed >>= \case
        True -> pure Nothing
        False -> Eval.evalQ render' ref q

    runComponent
      :: forall f' i' o'
       . IORef (LifecycleHandlers m)
      -> (o' -> m ())
      -> i'
      -> Component f' i' o' m
      -> m (DriverStateRef m r f' o')
    runComponent lchs handler j (Component cs) = do
      lchs' <- newLifecycleHandlers
      st <- initDriverState cs j handler lchs'
      preInits <- atomicModifyIORef' lchs $ \handlers ->
        (handlers {initializers = []}, handlers.initializers)
      render' lchs st.selfRef
      squashChildInitializers lchs preInits (DriverStateX st)
      pure $ DriverStateRef st.selfRef

    render'
      :: forall s f' act ps i' o'
       . IORef (LifecycleHandlers m)
      -> IORef (DriverState m r s f' act ps i' o')
      -> m ()
    render' lchs var =
      readIORef var >>= \ds -> do
        shouldProcessHandlers <- isNothing <$> readIORef ds.pendingHandlers
        if not shouldProcessHandlers
          then
            -- WARN: This implementation diverges from PureScript's.
            -- Re-entrancy guard. A render is already in progress on THIS
            -- DriverState (pendingHandlers is non-empty). Under the GHC-JS
            -- cooperative scheduler the in-progress render yields at the reuse
            -- path's `evalM (Receive)` below, letting a state-change- or
            -- fork-triggered re-render re-enter here. Running the render body
            -- now would walk the VDom against a half-consumed slot set and
            -- re-mint still-live children (re-creating their DOM + restarting
            -- their timers). Instead just flag the in-progress render to run
            -- another pass: it re-reads the latest state, so the requested
            -- render is never lost. PureScript's Aff driver never preempts
            -- mid-render and so needs no such guard; the GHC-JS scheduler does.
            atomicWriteIORef ds.renderDirty True
          else do
            -- Render passes repeat until no re-entrant render was requested.
            -- Each pass holds the pendingHandlers lock (Just …) across the
            -- whole walk + handler drain, so any re-entrant render observes the
            -- lock and only sets renderDirty rather than corrupting this walk.
            let renderPass = do
                  atomicWriteIORef ds.pendingHandlers (Just [])
                  atomicWriteIORef ds.renderDirty False
                  -- Re-read for the latest state / children / rendering each pass.
                  cur <- readIORef var
                  -- Per-render scratch storage, local to this pass; never shared
                  -- DriverState fields (a re-entrant pass would clobber them).
                  childrenInRef <- newIORef cur.children
                  childrenOutRef <- newIORef Slot.empty

                  -- A ref is recorded as soon as its element is created or
                  -- removed, not queued with the actions: queued handlers are
                  -- forked below, and a forked thread only starts when the
                  -- scheduler gets to it, while initialisers run on this one.
                  -- An initialiser that looks up a ref of its own component
                  -- (as MDC components do, to attach to their root element)
                  -- could otherwise run first and find nothing. Recording a
                  -- ref runs no component code, so doing it mid-render is safe;
                  -- purescript-halogen gets the same order from Aff's fork,
                  -- which runs a fiber at once.
                  let handler :: Input act -> m ()
                      handler = \case
                        input@(Input.RefUpdate _ _) -> void $ Eval.evalF render' ds.selfRef input
                        input -> Eval.queueOrRun ds.pendingHandlers . void $ Eval.evalF render' ds.selfRef input

                      childHandler :: act -> m ()
                      childHandler = Eval.queueOrRun ds.pendingQueries . handler . Input.Action

                  rendering <-
                    render
                      handler
                      (renderChild' lchs childHandler childrenInRef childrenOutRef)
                      (cur.component.render cur.state)
                      cur.rendering

                  children <- readIORef childrenOutRef
                  childrenIn <- readIORef childrenInRef

                  Slot.foreachSlot childrenIn $ \(DriverStateRef childVar) -> do
                    childDS <- DriverStateX <$> readIORef childVar
                    renderStateX_ removeChild childDS
                    finalize lchs childDS

                  atomicModifyIORef'_ ds.selfRef $ \ds' ->
                    ds' {rendering = Just rendering, children = children}

                  flip loopM () $ \_ -> do
                    handlers <- readIORef ds.pendingHandlers
                    atomicWriteIORef ds.pendingHandlers (Just [])
                    traverse_ (traverse_ fork . reverse) handlers
                    mmore <- readIORef ds.pendingHandlers
                    if maybe False null mmore
                      then atomicWriteIORef ds.pendingHandlers Nothing $> Right ()
                      else pure $ Left ()

                  dirty <- readIORef ds.renderDirty
                  when dirty renderPass
            renderPass

    renderChild'
      :: forall ps act
       . IORef (LifecycleHandlers m)
      -> (act -> m ())
      -> IORef (Slot.SlotStorage ps (DriverStateRef m r))
      -> IORef (Slot.SlotStorage ps (DriverStateRef m r))
      -> ComponentSlotBox ps m act
      -> m (RenderStateX r)
    renderChild' lchs handler childrenInRef childrenOutRef ComponentSlotBox {..} = do
      childrenIn <- pop <$> readIORef childrenInRef
      var <- case childrenIn of
        Just (existing, childrenIn') -> do
          atomicWriteIORef childrenInRef childrenIn'
          DriverStateX st <- readDriverStateRef existing
          atomicWriteIORef st.handlerRef $ maybe pass handler . output
          -- FIXME
          void $ Eval.evalM render' st.selfRef (runNT (unsafeCoerce st.component.eval) (HQ.Receive input ()))
          pure existing
        Nothing ->
          runComponent lchs (maybe pass handler . output) input component
      isDuplicate <- isJust . get <$> readIORef childrenOutRef
      when isDuplicate
        $ traceM "Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"
      atomicModifyIORef'_ childrenOutRef (set var)
      (readDriverStateRef var >>=) $ renderStateX $ \case
        Nothing -> throwString "Halogen internal error: child was not initialized in renderChild"
        Just r -> pure (renderChild r)

    squashChildInitializers
      :: forall f' o'
       . IORef (LifecycleHandlers m)
      -> [m ()]
      -> DriverStateX m r f' o'
      -> m ()
    squashChildInitializers lchs preInits (DriverStateX st) = do
      let parentInitializer = Eval.evalM render' st.selfRef (runNT st.component.eval (HQ.Initialize ()))
      atomicModifyIORef'_ lchs $ \handlers ->
        handlers
          { initializers =
              ( do
                  parSequence_ (reverse handlers.initializers)
                  parentInitializer
                  handlePending st.pendingQueries
                  handlePending st.pendingOuts
              )
                : preInits
          , finalizers = handlers.finalizers
          }

    finalize
      :: forall f' o'
       . IORef (LifecycleHandlers m)
      -> DriverStateX m r f' o'
      -> m ()
    finalize lchs (DriverStateX DriverState {selfRef}) = do
      st <- readIORef selfRef
      cleanupSubscriptionsAndForks st
      let f = Eval.evalM render' st.selfRef (runNT st.component.eval (HQ.Finalize ()))
      atomicModifyIORef'_ lchs $ \handlers ->
        handlers
          { initializers = handlers.initializers
          , finalizers = f : handlers.finalizers
          }
      Slot.foreachSlot st.children $ \(DriverStateRef ref) -> do
        ds <- DriverStateX <$> readIORef ref
        finalize lchs ds

    dispose'
      :: forall f' o'
       . IORef Bool
      -> IORef (LifecycleHandlers m)
      -> DriverStateX m r f' o'
      -> m ()
    dispose' disposed lchs dsx@(DriverStateX DriverState {selfRef}) = Eval.handleLifecycle lchs $ do
      readIORef disposed >>= \case
        True -> pass
        False -> do
          atomicWriteIORef disposed True
          finalize lchs dsx
          ds <- readIORef selfRef
          for_ ds.rendering dispose

{-# INLINE newLifecycleHandlers #-}
newLifecycleHandlers :: (MonadIO m) => m (IORef (LifecycleHandlers m))
newLifecycleHandlers = newIORef $ LifecycleHandlers {initializers = [], finalizers = [], nesting = 0}

{-# SPECIALIZE handlePending :: IORef (Maybe [IO ()]) -> IO () #-}
handlePending :: (MonadIO m, MonadFork m) => IORef (Maybe [m ()]) -> m ()
handlePending ref = do
  queue <- readIORef ref
  atomicWriteIORef ref Nothing
  for_ queue (traverse_ fork . reverse)

{-# SPECIALIZE cleanupSubscriptionsAndForks :: DriverState IO r s f act ps i o -> IO () #-}
cleanupSubscriptionsAndForks
  :: (MonadIO m, MonadKill m)
  => DriverState m r s f act ps i o
  -> m ()
cleanupSubscriptionsAndForks ds = do
  traverse_ (traverse_ HS.unsubscribe) =<< readIORef ds.subscriptions
  atomicWriteIORef ds.subscriptions Nothing
  traverse_ (kill AsyncCancelled) =<< readIORef ds.forks
  atomicWriteIORef ds.forks mempty
