module Halogen.Aff.Driver
  ( RenderSpec (..)
  , runUI
  , HalogenIO (..)
  )
where

import Control.Exception.Safe
import Control.Monad.Extra
import Control.Monad.Fork
import Control.Monad.Parallel
import Control.Monad.Primitive
import Control.Monad.UUID
import Data.MutVarF
import Data.Primitive
import Data.Row
import Halogen.Aff.Driver.Eval qualified as Eval
import Halogen.Aff.Driver.State
import Halogen.Component
import Halogen.Data.Slot qualified as Slot
import Halogen.HTML.Core qualified as HC
import Halogen.Query.HalogenQ qualified as HQ
import Halogen.Query.Input
import Halogen.Query.Input qualified as Input
import Halogen.Subscription qualified as HS
import Protolude hiding (get)
import UnliftIO (AsyncCancelled (AsyncCancelled))
import Unsafe.Coerce

data HalogenIO query output m = HalogenIO
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

runUI
  :: forall m r f i o
   . (PrimMonad m, MonadFork Async m, MonadKill Async m, MonadParallel m, MonadMask m, MonadUUID m)
  => RenderSpec m r
  -> Component f i o m
  -> i
  -> m (HalogenIO f o m)
runUI RenderSpec {render = renderS, renderChild = renderChildS, removeChild = removeChildS, dispose = disposeS} component i = do
  lchs <- newLifecycleHandlers
  disposed <- newMutVar False
  Eval.handleLifecycle lchs $ do
    sio <- HS.create
    dsx@(DriverStateX st) <- readDriverStateRef =<< runComponent lchs (HS.notify sio.listener) i component
    pure $
      HalogenIO
        { query = evalDriver disposed st.selfRef
        , messages = sio.emitter
        , dispose = dispose disposed lchs dsx
        }
  where
    evalDriver
      :: forall s f' act ps i' o'
       . MutVar (PrimState m) Bool
      -> MutVar (PrimState m) (DriverState m r s f' act ps i' o')
      -> (forall a. f' a -> m (Maybe a))
    evalDriver disposed ref q =
      readMutVar disposed >>= \case
        True -> pure Nothing
        False -> Eval.evalQ render ref q

    runComponent
      :: forall f' i' o'
       . MutVar (PrimState m) (LifecycleHandlers m)
      -> (o' -> m ())
      -> i'
      -> Component f' i' o' m
      -> m (DriverStateRef m r f' o')
    runComponent lchs handler j (Component c) = do
      lchs' <- newLifecycleHandlers
      st <- initDriverState c j handler lchs'
      pre <- readMutVar lchs
      atomicWriteMutVar lchs $ LifecycleHandlers {initializers = [], finalizers = pre.finalizers}
      render lchs st.selfRef
      squashChildInitializers lchs pre.initializers (DriverStateX st)
      pure $ DriverStateRef st.selfRef

    render
      :: forall s f' act ps i' o'
       . MutVar (PrimState m) (LifecycleHandlers m)
      -> MutVar (PrimState m) (DriverState m r s f' act ps i' o')
      -> m ()
    render lchs var =
      readMutVar var >>= \ds -> do
        shouldProcessHandlers <- isNothing <$> readMutVar ds.pendingHandlers
        when shouldProcessHandlers $ atomicWriteMutVar ds.pendingHandlers (Just [])
        atomicWriteMutVar ds.childrenOut Slot.empty
        atomicWriteMutVar ds.childrenIn ds.children

        let -- The following 3 defs are working around a capture bug, see #586
            -- pendingHandlers = identity ds.pendingHandlers
            -- pendingQueries = identity ds.pendingQueries
            -- selfRef = identity ds.selfRef

            handler :: Input act -> m ()
            handler = Eval.queueOrRun ds.pendingHandlers . void . Eval.evalF render ds.selfRef

            childHandler :: act -> m ()
            childHandler = Eval.queueOrRun ds.pendingQueries . handler . Input.Action

        rendering <-
          renderS
            handler
            (renderChild lchs childHandler ds.childrenIn ds.childrenOut)
            (ds.component.render ds.state)
            ds.rendering

        children <- readMutVar ds.childrenOut
        childrenIn <- readMutVar ds.childrenIn

        Slot.foreachSlot childrenIn $ \(DriverStateRef childVar) -> do
          childDS <- DriverStateX <$> readMutVar childVar
          renderStateX_ removeChildS childDS
          finalize lchs childDS

        atomicModifyMutVar'_ ds.selfRef $ \ds' ->
          ds' {rendering = Just rendering, children = children}

        when shouldProcessHandlers $ do
          flip loopM () $ \_ -> do
            handlers <- readMutVar ds.pendingHandlers
            atomicWriteMutVar ds.pendingHandlers (Just [])
            traverse_ (traverse_ fork . reverse) handlers
            mmore <- readMutVar ds.pendingHandlers
            if maybe False null mmore
              then atomicWriteMutVar ds.pendingHandlers Nothing $> Right ()
              else pure $ Left ()

    renderChild
      :: forall ps act
       . MutVar (PrimState m) (LifecycleHandlers m)
      -> (act -> m ())
      -> MutVar (PrimState m) (Slot.SlotStorage ps (DriverStateRef m r))
      -> MutVar (PrimState m) (Slot.SlotStorage ps (DriverStateRef m r))
      -> ComponentSlotBox ps m act
      -> m (RenderStateX r)
    renderChild lchs handler childrenInRef childrenOutRef ComponentSlotBox {pop, output, input, component = c, get, set} = do
      childrenIn <- pop <$> readMutVar childrenInRef
      var <- case childrenIn of
        Just (existing, childrenIn') -> do
          atomicWriteMutVar childrenInRef childrenIn'
          DriverStateX st <- readDriverStateRef existing
          atomicWriteMutVar st.handlerRef $ maybe pass handler . output
          -- forgive me gods but it just doesnt typecheck with input
          void $ Eval.evalM render st.selfRef (runNT (unsafeCoerce st.component.eval) (HQ.Receive input ()))
          pure existing
        Nothing ->
          runComponent lchs (maybe pass handler . output) input c
      isDuplicate <- isJust . get <$> readMutVar childrenOutRef
      when isDuplicate $
        traceM "Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"
      atomicModifyMutVar'_ childrenOutRef (set var)
      (readDriverStateRef var >>=) $ renderStateX $ \case
        Nothing -> throwString "Halogen internal error: child was not initialized in renderChild"
        Just r -> pure (renderChildS r)

    squashChildInitializers
      :: forall f' o'
       . MutVar (PrimState m) (LifecycleHandlers m)
      -> [m ()]
      -> DriverStateX m r f' o'
      -> m ()
    squashChildInitializers lchs preInits (DriverStateX st) = do
      let parentInitializer = Eval.evalM render st.selfRef (runNT st.component.eval (HQ.Initialize ()))
      atomicModifyMutVar'_ lchs $ \handlers ->
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
       . MutVar (PrimState m) (LifecycleHandlers m)
      -> DriverStateX m r f' o'
      -> m ()
    finalize lchs (DriverStateX DriverState {selfRef}) = do
      st <- readMutVar selfRef
      cleanupSubscriptionsAndForks st
      let f = Eval.evalM render st.selfRef (runNT st.component.eval (HQ.Finalize ()))
      atomicModifyMutVar'_ lchs $ \handlers ->
        handlers
          { initializers = handlers.initializers
          , finalizers = f : handlers.finalizers
          }
      Slot.foreachSlot st.children $ \(DriverStateRef ref) -> do
        ds <- DriverStateX <$> readMutVar ref
        finalize lchs ds

    dispose
      :: forall f' o'
       . MutVar (PrimState m) Bool
      -> MutVar (PrimState m) (LifecycleHandlers m)
      -> DriverStateX m r f' o'
      -> m ()
    dispose disposed lchs dsx@(DriverStateX DriverState {selfRef}) = Eval.handleLifecycle lchs $ do
      readMutVar disposed >>= \case
        True -> pass
        False -> do
          atomicWriteMutVar disposed True
          finalize lchs dsx
          ds <- readMutVar selfRef
          for_ ds.rendering disposeS

newLifecycleHandlers :: (PrimMonad m) => m (MutVar (PrimState m) (LifecycleHandlers m))
newLifecycleHandlers = newMutVar $ LifecycleHandlers {initializers = [], finalizers = []}

handlePending :: (PrimMonad m, MonadFork f m) => MutVar (PrimState m) (Maybe [m ()]) -> m ()
handlePending ref = do
  queue <- readMutVar ref
  atomicWriteMutVar ref Nothing
  for_ queue (traverse_ fork . reverse)

cleanupSubscriptionsAndForks
  :: (PrimMonad m, MonadKill Async m)
  => DriverState m r s f act ps i o
  -> m ()
cleanupSubscriptionsAndForks ds = do
  traverse_ (traverse_ HS.unsubscribe) =<< readMutVar ds.subscriptions
  atomicWriteMutVar ds.subscriptions Nothing
  traverse_ (kill AsyncCancelled) =<< readMutVar ds.forks
  atomicWriteMutVar ds.forks mempty
