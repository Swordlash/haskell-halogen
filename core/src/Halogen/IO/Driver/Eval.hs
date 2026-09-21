module Halogen.IO.Driver.Eval
  ( Renderer
  , evalF
  , evalQ
  , evalM
  , handleLifecycle
  , queueOrRun
  -- , handleIO
  )
where

import Control.Applicative.Free.Fast
import Control.Exception.Safe qualified as Safe
import Control.Monad.Fork
import Control.Monad.Free.Church (foldF)
import Control.Monad.Parallel
import Data.Foreign
import Data.Functor.Coyoneda
import Data.Map.Strict qualified as M
import Data.NT
import HPrelude hiding (Concurrently, finally, join, runConcurrently, state)
import Halogen.Component
import Halogen.IO.Driver.State
import Halogen.Query.ChildQuery qualified as CQ
import Halogen.Query.HalogenM hiding (fork, join, kill, query, unsubscribe)
import Halogen.Query.HalogenQ qualified as HQ
import Halogen.Query.Input
import Halogen.Query.Input qualified as Input
import Halogen.Subscription qualified as HS

type Renderer m r =
  forall s f act ps i o
   . IORef (LifecycleHandlers m)
  -> IORef (DriverState m r s f act ps i o)
  -> m ()

{-# SPECIALIZE evalF :: Renderer IO r -> IORef (DriverState IO r s f act ps i o) -> Input act -> IO () #-}
evalF
  :: (MonadUnliftIO m, MonadParallel m, MonadMask m, MonadFork m, MonadKill m)
  => Renderer m r
  -> IORef (DriverState m r s f act ps i o)
  -> Input act
  -> m ()
evalF render ref = \case
  Input.RefUpdate (Input.RefLabel p) el -> do
    atomicModifyIORef'_ ref $ \st ->
      st {refs = M.alter (const el) p st.refs}
  Input.Action act -> do
    st <- readIORef ref
    evalM render ref (runNT st.component.eval (HQ.Action act ()))

{-# SPECIALIZE evalQ :: Renderer IO r -> IORef (DriverState IO r s f act ps i o) -> f a -> IO (Maybe a) #-}
evalQ
  :: (MonadUnliftIO m, MonadParallel m, MonadMask m, MonadFork m, MonadKill m)
  => Renderer m r
  -> IORef (DriverState m r s f act ps i o)
  -> f a
  -> m (Maybe a)
evalQ render ref q = do
  st <- readIORef ref
  evalM render ref (runNT st.component.eval (HQ.Query (Just <$> liftCoyoneda q) (const Nothing)))

{-# SPECIALIZE evalM :: Renderer IO r -> IORef (DriverState IO r s f act ps i o) -> HalogenM s act ps o IO a -> IO a #-}
evalM
  :: forall m r s f act ps i o a
   . (MonadUnliftIO m, MonadParallel m, MonadMask m, MonadFork m, MonadKill m)
  => Renderer m r
  -> IORef (DriverState m r s f act ps i o)
  -> HalogenM s act ps o m a
  -> m a
evalM render initRef (HalogenM hm) = foldF (go initRef) hm
  where
    go
      :: forall x
       . IORef (DriverState m r s f act ps i o)
      -> HalogenF s act ps o m x
      -> m x
    go ref = \case
      State f -> do
        st@DriverState {state, lifecycleHandlers} <- readIORef ref
        case f state of
          (a, state')
            | unsafeRefEq state state' -> pure a
            | otherwise -> do
                atomicWriteIORef ref (st {state = state'})
                handleLifecycle lifecycleHandlers (render lifecycleHandlers ref)
                pure a
      Subscribe fes k -> do
        sid <- fresh SubscriptionId ref
        finalize <- fmap (HS.hoistSubscription (NT liftIO)) $ withRunInIO $ \runInIO -> HS.subscribe (fes sid) $ \act ->
          runInIO $ evalF render ref (Input.Action act)
        DriverState {subscriptions} <- readIORef ref
        atomicModifyIORef'_ subscriptions (map (M.insert sid finalize))
        pure (k sid)
      Unsubscribe sid next -> do
        unsubscribe sid ref
        pure next
      Lift aff ->
        aff
      Unlift q -> withRunInIO $ \runInIO -> q (UnliftIO $ runInIO . evalM render initRef)
      ChildQuery cq ->
        evalChildQuery ref cq
      Raise o a -> do
        DriverState {handlerRef, pendingOuts} <- readIORef ref
        handler <- readIORef handlerRef
        queueOrRun pendingOuts (handler o)
        pure a
      Par (HalogenAp p) -> sequential $ retractAp $ hoistAp (parallel . evalM render ref) p
      Fork hmu k -> do
        fid <- fresh ForkId ref
        DriverState {forks} <- readIORef ref
        doneRef <- newIORef False
        -- The bookkeeping is the finalizer, not the action: a fork has to stay
        -- in `forks` for as long as it runs, because that map is what `Join`,
        -- `Kill` and finalization look it up in. With the two the other way
        -- round the fork struck itself off the register before doing any work,
        -- which left `kill` and `join` as no-ops and let a component's forks
        -- outlive it.
        fiber <-
          fork
            $ Safe.finally
              (evalM render ref hmu)
              ( do
                  atomicModifyIORef'_ forks (M.delete fid)
                  atomicWriteIORef doneRef True
              )
        -- Already finished, so there is nothing to register: the finalizer has
        -- run and would not remove an entry added now.
        unlessM (readIORef doneRef) $ do
          atomicModifyIORef'_ forks (M.insert fid fiber)
          -- It can also finish in the gap between that check and this insert,
          -- in which case its own removal ran before there was anything to
          -- remove and this entry would sit in the map until the component was
          -- disposed of. doneRef is written after the removal, so reading it
          -- once more here catches exactly the entry we have just orphaned.
          whenM (readIORef doneRef) $ atomicModifyIORef'_ forks (M.delete fid)
        pure (k fid)
      Join fid a -> do
        DriverState {forks} <- readIORef ref
        forkMap <- readIORef forks
        traverse_ join (M.lookup fid forkMap)
        pure a
      Kill fid a -> do
        DriverState {forks} <- readIORef ref
        forkMap <- readIORef forks
        traverse_ (kill AsyncCancelled) (M.lookup fid forkMap)
        pure a
      GetRef (Input.RefLabel p) k -> do
        DriverState {refs} <- readIORef ref
        pure $ k $ M.lookup p refs

    evalChildQuery
      :: IORef (DriverState m r s f act ps i o)
      -> CQ.ChildQuery ps x
      -> m x
    evalChildQuery ref (CQ.ChildQuery unpack query reply) = do
      st <- readIORef ref
      let evalChild (DriverStateRef var) = parallel $ do
            dsx <- readIORef var
            evalQ render dsx.selfRef query
      reply <$> sequential (unpack evalChild st.children)

{-# SPECIALIZE unsubscribe :: SubscriptionId -> IORef (DriverState IO r s f act ps i o) -> IO () #-}
unsubscribe
  :: (MonadIO m)
  => SubscriptionId
  -> IORef (DriverState m r s' f' act' ps' i' o')
  -> m ()
unsubscribe sid ref = do
  DriverState {subscriptions} <- readIORef ref
  subs <- readIORef subscriptions
  traverse_ HS.unsubscribe (M.lookup sid =<< subs)

{-# SPECIALIZE handleLifecycle :: IORef (LifecycleHandlers IO) -> IO a -> IO a #-}
handleLifecycle :: (MonadIO m, MonadParallel m, MonadFork m, MonadMask m) => IORef (LifecycleHandlers m) -> m a -> m a
handleLifecycle lchs f = Safe.mask $ \restore -> do
  atomicModifyIORef'_ lchs $ \handlers ->
    if handlers.nesting == 0
      then LifecycleHandlers {initializers = [], finalizers = [], nesting = 1}
      else handlers {nesting = handlers.nesting + 1}

  let leave keepHandlers = atomicModifyIORef' lchs $ \handlers ->
        if handlers.nesting == 1
          then
            ( LifecycleHandlers {initializers = [], finalizers = [], nesting = 0}
            , if keepHandlers then Just handlers else Nothing
            )
          else (handlers {nesting = handlers.nesting - 1}, Nothing)

  result <- restore f `Safe.onException` void (leave False)
  ready <- leave True
  restore $ for_ ready $ \LifecycleHandlers {initializers, finalizers} -> do
    traverse_ fork finalizers
    parSequence_ initializers
  pure result

{-# SPECIALIZE fresh :: (Int -> a) -> IORef (DriverState IO r s f act ps i o) -> IO a #-}
fresh
  :: (MonadIO m)
  => (Int -> a)
  -> IORef (DriverState m r s f act ps i o)
  -> m a
fresh f ref = do
  st <- readIORef ref
  atomicModifyIORef' st.fresh (\i -> (i + 1, f i))

{-# SPECIALIZE queueOrRun :: IORef (Maybe [IO ()]) -> IO () -> IO () #-}
queueOrRun
  :: (MonadIO m)
  => IORef (Maybe [m ()])
  -> m ()
  -> m ()
queueOrRun ref au =
  readIORef ref >>= \case
    Nothing -> au
    Just p -> atomicWriteIORef ref (Just (au : p))
