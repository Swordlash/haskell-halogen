module Halogen.Aff.Driver.Eval
  ( Renderer
  , evalF
  , evalQ
  , evalM
  , handleLifecycle
  , queueOrRun
  -- , handleAff
  )
where

import Control.Applicative.Free.Fast
import Control.Exception.Safe (MonadMask, finally)
import Control.Monad.Fork
import Control.Monad.Free.Church (foldF)
import Control.Monad.Parallel
import Control.Monad.Primitive
import Data.Foreign
import Data.Functor.Coyoneda
import Data.Map.Strict qualified as M
import Data.MutVarF
import Data.NT
import Data.Primitive
import Halogen.Aff.Driver.State
import Halogen.Component
import Halogen.Query.ChildQuery qualified as CQ
import Halogen.Query.HalogenM hiding (fork, join, kill, query, unsubscribe)
import Halogen.Query.HalogenQ qualified as HQ
import Halogen.Query.Input
import Halogen.Query.Input qualified as Input
import Halogen.Subscription qualified as HS
import Protolude hiding (Concurrently, finally, join, runConcurrently, state)
import UnliftIO (MonadUnliftIO (..))
import UnliftIO.Async (AsyncCancelled (..))

type Renderer m r =
  forall s f act ps i o
   . MutVar (PrimState m) (LifecycleHandlers m)
  -> MutVar (PrimState m) (DriverState m r s f act ps i o)
  -> m ()

evalF
  :: (PrimMonad m, MonadUnliftIO m, MonadParallel m, MonadMask m, MonadFork m, MonadKill m)
  => Renderer m r
  -> MutVar (PrimState m) (DriverState m r s f act ps i o)
  -> Input act
  -> m ()
evalF render ref = \case
  Input.RefUpdate (Input.RefLabel p) el -> do
    atomicModifyMutVar'_ ref $ \st ->
      st {refs = M.alter (const el) p st.refs}
  Input.Action act -> do
    st <- readMutVar ref
    evalM render ref (runNT st.component.eval (HQ.Action act ()))

evalQ
  :: (PrimMonad m, MonadUnliftIO m, MonadParallel m, MonadMask m, MonadFork m, MonadKill m)
  => Renderer m r
  -> MutVar (PrimState m) (DriverState m r s f act ps i o)
  -> f a
  -> m (Maybe a)
evalQ render ref q = do
  st <- readMutVar ref
  evalM render ref (runNT st.component.eval (HQ.Query (Just <$> liftCoyoneda q) (const Nothing)))

evalM
  :: forall m r s f act ps i o a
   . (PrimMonad m, MonadUnliftIO m, MonadParallel m, MonadMask m, MonadFork m, MonadKill m)
  => Renderer m r
  -> MutVar (PrimState m) (DriverState m r s f act ps i o)
  -> HalogenM s act ps o m a
  -> m a
evalM render initRef (HalogenM hm) = foldF (go initRef) hm
  where
    go
      :: forall x
       . MutVar (PrimState m) (DriverState m r s f act ps i o)
      -> HalogenF s act ps o m x
      -> m x
    go ref = \case
      State f -> do
        st@DriverState {state, lifecycleHandlers} <- readMutVar ref
        case f state of
          (a, state')
            | unsafeRefEq state state' -> pure a
            | otherwise -> do
                atomicWriteMutVar ref (st {state = state'})
                handleLifecycle lifecycleHandlers (render lifecycleHandlers ref)
                pure a
      Subscribe fes k -> do
        sid <- fresh SubscriptionId ref
        finalize <- fmap (HS.transSubscription (NT liftIO)) $ withRunInIO $ \runInIO -> HS.subscribe (fes sid) $ \act ->
          runInIO $ evalF render ref (Input.Action act)
        DriverState {subscriptions} <- readMutVar ref
        atomicModifyMutVar'_ subscriptions (map (M.insert sid finalize))
        pure (k sid)
      Unsubscribe sid next -> do
        unsubscribe sid ref
        pure next
      Lift aff ->
        aff
      ChildQuery cq ->
        evalChildQuery ref cq
      Raise o a -> do
        DriverState {handlerRef, pendingOuts} <- readMutVar ref
        handler <- readMutVar handlerRef
        queueOrRun pendingOuts (handler o)
        pure a
      Par (HalogenAp p) -> sequential $ retractAp $ hoistAp (parallel . evalM render ref) p
      Fork hmu k -> do
        fid <- fresh ForkId ref
        DriverState {forks} <- readMutVar ref
        doneRef <- newMutVar False
        fiber <-
          fork $
            finally
              ( do
                  atomicModifyMutVar'_ forks (M.delete fid)
                  atomicWriteMutVar doneRef True
              )
              (evalM render ref hmu)
        unlessM (readMutVar doneRef) $ do
          atomicModifyMutVar'_ forks (M.insert fid fiber)
        pure (k fid)
      Join fid a -> do
        DriverState {forks} <- readMutVar ref
        forkMap <- readMutVar forks
        traverse_ join (M.lookup fid forkMap)
        pure a
      Kill fid a -> do
        DriverState {forks} <- readMutVar ref
        forkMap <- readMutVar forks
        traverse_ (kill AsyncCancelled) (M.lookup fid forkMap)
        pure a
      GetRef (Input.RefLabel p) k -> do
        DriverState {refs} <- readMutVar ref
        pure $ k $ M.lookup p refs

    evalChildQuery
      :: MutVar (PrimState m) (DriverState m r s f act ps i o)
      -> CQ.ChildQuery ps x
      -> m x
    evalChildQuery ref (CQ.ChildQuery unpack query reply) = do
      st <- readMutVar ref
      let evalChild (DriverStateRef var) = parallel $ do
            dsx <- readMutVar var
            evalQ render dsx.selfRef query
      reply <$> sequential (unpack evalChild st.children)

unsubscribe
  :: (PrimMonad m)
  => SubscriptionId
  -> MutVar (PrimState m) (DriverState m r s' f' act' ps' i' o')
  -> m ()
unsubscribe sid ref = do
  DriverState {subscriptions} <- readMutVar ref
  subs <- readMutVar subscriptions
  traverse_ HS.unsubscribe (M.lookup sid =<< subs)

handleLifecycle :: (PrimMonad m, MonadParallel m, MonadFork m) => MutVar (PrimState m) (LifecycleHandlers m) -> m a -> m a
handleLifecycle lchs f = do
  atomicWriteMutVar lchs $ LifecycleHandlers {initializers = [], finalizers = []}
  result <- f
  LifecycleHandlers {initializers, finalizers} <- readMutVar lchs
  traverse_ fork finalizers
  parSequence_ initializers
  pure result

fresh
  :: (PrimMonad m)
  => (Int -> a)
  -> MutVar (PrimState m) (DriverState m r s f act ps i o)
  -> m a
fresh f ref = do
  st <- readMutVar ref
  atomicModifyMutVar' st.fresh (\i -> (i + 1, f i))

queueOrRun
  :: (PrimMonad m)
  => MutVar (PrimState m) (Maybe [m ()])
  -> m ()
  -> m ()
queueOrRun ref au =
  readMutVar ref >>= \case
    Nothing -> au
    Just p -> atomicWriteMutVar ref (Just (au : p))
