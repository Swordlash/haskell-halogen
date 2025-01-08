module Halogen.IO.Driver
  ( RenderSpec (..)
  , runUI
  , HalogenSocket (..)
  )
where

import Control.Exception.Safe
import Control.Monad.Extra
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
      -> m (DriverStateRef m r f' i' o')
    runComponent lchs handler j (Component cs) = do
      lchs' <- newLifecycleHandlers
      st <- initDriverState cs j handler lchs'
      pre <- readIORef lchs
      atomicWriteIORef lchs $ LifecycleHandlers {initializers = [], finalizers = pre.finalizers}
      render' lchs st.selfRef
      squashChildInitializers lchs pre.initializers (DriverStateX st)
      pure $ DriverStateRef st.selfRef

    render'
      :: forall s f' act ps i' o'
       . IORef (LifecycleHandlers m)
      -> IORef (DriverState m r s f' act ps i' o')
      -> m ()
    render' lchs var =
      readIORef var >>= \ds -> do
        shouldProcessHandlers <- isNothing <$> readIORef ds.pendingHandlers
        when shouldProcessHandlers $ atomicWriteIORef ds.pendingHandlers (Just [])
        atomicWriteIORef ds.childrenOut Slot.empty
        atomicWriteIORef ds.childrenIn ds.children

        let -- The following 3 defs are working around a capture bug, see #586
            -- pendingHandlers = identity ds.pendingHandlers
            -- pendingQueries = identity ds.pendingQueries
            -- selfRef = identity ds.selfRef

            handler :: Input act -> m ()
            handler = Eval.queueOrRun ds.pendingHandlers . void . Eval.evalF render' ds.selfRef

            childHandler :: act -> m ()
            childHandler = Eval.queueOrRun ds.pendingQueries . handler . Input.Action

        rendering <-
          render
            handler
            (renderChild' lchs childHandler ds.childrenIn ds.childrenOut)
            (ds.component.render ds.state)
            ds.rendering

        children <- readIORef ds.childrenOut
        childrenIn <- readIORef ds.childrenIn

        Slot.foreachSlot childrenIn $ \(DriverStateRef childVar) -> do
          childDS <- DriverStateX <$> readIORef childVar
          renderStateX_ removeChild childDS
          finalize lchs childDS

        atomicModifyIORef'_ ds.selfRef $ \ds' ->
          ds' {rendering = Just rendering, children = children}

        when shouldProcessHandlers $ do
          flip loopM () $ \_ -> do
            handlers <- readIORef ds.pendingHandlers
            atomicWriteIORef ds.pendingHandlers (Just [])
            traverse_ (traverse_ fork . reverse) handlers
            mmore <- readIORef ds.pendingHandlers
            if maybe False null mmore
              then atomicWriteIORef ds.pendingHandlers Nothing $> Right ()
              else pure $ Left ()

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
          -- forgive me gods but it just doesnt typecheck with input
          void $ Eval.evalM render' st.selfRef (runNT st.component.eval (HQ.Receive input ()))
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
      :: forall f' i' o'
       . IORef (LifecycleHandlers m)
      -> [m ()]
      -> DriverStateX m r f' i' o'
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
      :: forall f' i' o'
       . IORef (LifecycleHandlers m)
      -> DriverStateX m r f' i' o'
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
      :: forall f' i' o'
       . IORef Bool
      -> IORef (LifecycleHandlers m)
      -> DriverStateX m r f' i' o'
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
newLifecycleHandlers = newIORef $ LifecycleHandlers {initializers = [], finalizers = []}

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
