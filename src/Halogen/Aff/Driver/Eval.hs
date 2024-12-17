module Halogen.Aff.Driver.Eval
  ( Renderer
  , evalF
  , evalQ
  , evalM
  , handleLifecycle
  , queueOrRun
  --, handleAff
  ) where

import Protolude hiding (join, finally, runConcurrently, Concurrently, state)
import Data.Primitive
import Control.Monad.Primitive
import Halogen.Aff.Driver.State
import Halogen.Query.Input
import qualified Halogen.Query.Input as Input
import qualified Data.Map.Strict as M
import Halogen.Query.HalogenM
import qualified Halogen.Query.ChildQuery as CQ
import qualified Halogen.Query.HalogenQ as HQ
import Control.Monad.Free.Church (foldF)
import Data.Foreign
import qualified Halogen.Subscription as HS
import Control.Applicative.Free.Fast
import Control.Monad.Parallel
import Control.Monad.Fork
import UnliftIO.Async (AsyncCancelled(..))
import Control.Exception.Safe (finally, MonadMask)
import Halogen.Component (ComponentSpec(..))
import Data.Functor.Coyoneda
import Data.MutVarF


type Renderer m r =
  forall s f act ps i o
   . MutVar (PrimState m) (LifecycleHandlers m)
  -> MutVar (PrimState m) (DriverState m r s f act ps i o)
  -> m ()

evalF
  :: (PrimMonad m, Parallel f' m, MonadMask m, MonadFork Async m, MonadKill Async m)
  => Renderer m r
  -> MutVar (PrimState m) (DriverState m r s f act ps i o)
  -> Input act
  -> m ()
evalF render ref = \case
  Input.RefUpdate (Input.RefLabel p) el -> do
    atomicModifyMutVar'_ ref $ \st ->
      st { refs = M.alter (const el) p st.refs }
  Input.Action act -> do
    st <- readMutVar ref
    evalM render ref (runNT st.component.eval (HQ.Message act ()))

evalQ
  :: (PrimMonad m, Parallel f' m, MonadMask m, MonadFork Async m, MonadKill Async m)
  => Renderer m r
  -> MutVar (PrimState m) (DriverState m r s f act ps i o)
  -> f a
  -> m (Maybe a)
evalQ render ref q = do
  st <- readMutVar ref
  evalM render ref (runNT st.component.eval (HQ.Query (Just <$> liftCoyoneda q) (const Nothing)))

evalM
  :: forall m r s f f' act ps i o a
   . (PrimMonad m, Parallel f' m, MonadMask m, MonadFork Async m, MonadKill Async m)
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
      st@DriverState{ state, lifecycleHandlers } <- readMutVar ref
      case f state of
        (a, state')
          | unsafeRefEq state state' -> pure a
          | otherwise -> do
              atomicWriteMutVar ref (st { state = state' })
              handleLifecycle lifecycleHandlers (render lifecycleHandlers ref)
              pure a
    Subscribe fes k -> do
      sid <- fresh SubscriptionId ref
      finalize <- HS.subscribe (fes sid) $ \act ->
        evalF render ref (Input.Action act)
      DriverState{ subscriptions } <- readMutVar ref
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
      DriverState { handlerRef, pendingOuts } <- readMutVar ref
      handler <- readMutVar handlerRef
      queueOrRun pendingOuts (handler o)
      pure a
    Par (HalogenAp p) -> sequential $ retractAp $ hoistAp (parallel . evalM render ref) p
    Fork hmu k -> do
      fid <- fresh ForkId ref
      DriverState{ forks } <- readMutVar ref
      doneRef <- newMutVar False
      fiber <- fork $ finally
        ( do
            atomicModifyMutVar'_ forks (M.delete fid)
            atomicWriteMutVar doneRef True
        )
        (evalM render ref hmu)
      unlessM (readMutVar doneRef) $ do
        atomicModifyMutVar'_ forks (M.insert fid fiber)
      pure (k fid)
    Join fid a -> do
      DriverState { forks } <- readMutVar ref
      forkMap <- readMutVar forks
      traverse_ join (M.lookup fid forkMap)
      pure a
    Kill fid a -> do
      DriverState{ forks } <- readMutVar ref
      forkMap <- readMutVar forks
      traverse_ (kill AsyncCancelled) (M.lookup fid forkMap)
      pure a
    GetRef (Input.RefLabel p) k -> do
      DriverState { refs } <- readMutVar ref
      pure $ k $ M.lookup p refs

  evalChildQuery
    :: MutVar (PrimState m) (DriverState m r s f act ps i o)
    -> CQ.ChildQuery ps x
    -> m x
  evalChildQuery ref (CQ.ChildQuery unpack query reply) = do
    st <- readMutVar ref
    let
      evalChild (DriverStateRef var) = parallel $ do
        dsx <- readMutVar var
        evalQ render dsx.selfRef query
    reply <$> sequential (unpack evalChild st.children)

unsubscribe
  :: PrimMonad m 
  => SubscriptionId
  -> MutVar (PrimState m) (DriverState m r s' f' act' ps' i' o')
  -> m ()
unsubscribe sid ref = do
  DriverState{ subscriptions } <- readMutVar ref
  subs <- readMutVar subscriptions
  traverse_ HS.unsubscribe (M.lookup sid =<< subs)

handleLifecycle :: (PrimMonad m, Parallel f m, MonadFork f' m) => MutVar (PrimState m) (LifecycleHandlers m) -> m a -> m a
handleLifecycle lchs f = do
  atomicWriteMutVar lchs $ LifecycleHandlers { initializers = [], finalizers = [] }
  result <- f
  LifecycleHandlers { initializers, finalizers } <- readMutVar lchs
  traverse_ fork finalizers
  parSequence_ initializers
  pure result

fresh
  :: PrimMonad m 
  => (Int -> a)
  -> MutVar (PrimState m) (DriverState m r s f act ps i o)
  -> m a
fresh f ref = do
  st <- readMutVar ref
  atomicModifyMutVar' st.fresh (\i -> (i + 1, f i))

queueOrRun
  :: PrimMonad m 
  => MutVar (PrimState m) (Maybe [m ()])
  -> m ()
  -> m ()
queueOrRun ref au =
  readMutVar ref >>= \case
    Nothing -> au
    Just p -> atomicWriteMutVar ref (Just (au : p))