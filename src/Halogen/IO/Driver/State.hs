module Halogen.IO.Driver.State
  ( LifecycleHandlers (..)
  , DriverState (..)
  , DriverStateRef (..)
  , DriverStateX (..)
  , unDriverStateX
  -- , mkDriverStateXRef
  , readDriverStateRef
  , RenderStateX (..)
  , renderStateX
  , renderStateX_
  -- , unRenderStateX
  , initDriverState
  )
where

import Control.Monad.Fork
import Data.Row
import HPrelude hiding (state)
import Halogen.Component
import Halogen.Data.Slot as SlotStorage
import Halogen.Query.HalogenM
import Halogen.Subscription qualified as HS
import Web.DOM.Element (Element)

data LifecycleHandlers m = LifecycleHandlers
  { initializers :: [m ()]
  , finalizers :: [m ()]
  }

data DriverState m r s f act ps i o = DriverState
  { component :: ComponentSpec s f act ps i o m
  , state :: s
  , refs :: Map Text Element
  , children :: SlotStorage ps (DriverStateRef m r)
  , childrenIn :: IORef (SlotStorage ps (DriverStateRef m r))
  , childrenOut :: IORef (SlotStorage ps (DriverStateRef m r))
  , selfRef :: IORef (DriverState m r s f act ps i o)
  , handlerRef :: IORef (o -> m ())
  , pendingQueries :: IORef (Maybe [m ()])
  , pendingOuts :: IORef (Maybe [m ()])
  , pendingHandlers :: IORef (Maybe [m ()])
  , rendering :: Maybe (r s act ps o)
  , fresh :: IORef Int
  , subscriptions :: IORef (Maybe (Map SubscriptionId (HS.Subscription m)))
  , forks :: IORef (Map ForkId (Fork m ()))
  , lifecycleHandlers :: IORef (LifecycleHandlers m)
  }

data DriverStateX m r f i o = forall s act ps. DriverStateX (DriverState m r s f act ps i o)

data DriverStateRef m r f i o = forall s act ps. DriverStateRef (IORef (DriverState m r s f act ps i o))

{-# INLINE readDriverStateRef #-}
readDriverStateRef :: (MonadIO m) => DriverStateRef m r f i o -> m (DriverStateX m r f i o)
readDriverStateRef (DriverStateRef ref) = DriverStateX <$> readIORef ref

data RenderStateX (r :: Type -> Type -> Row Type -> Type -> Type) = forall s act ps o. RenderStateX (r s act ps o)

{-# INLINE renderStateX #-}
renderStateX
  :: (Functor m)
  => (forall s act ps. Maybe (r s act ps o) -> m (r s act ps o))
  -> DriverStateX m r f i o
  -> m (RenderStateX r)
renderStateX f = unDriverStateX $ \st ->
  RenderStateX <$> f st.rendering

{-# INLINE renderStateX_ #-}
renderStateX_
  :: (Applicative m)
  => (forall s act ps. r s act ps o -> m ())
  -> DriverStateX m r f i o
  -> m ()
renderStateX_ f = unDriverStateX $ \st ->
  traverse_ f st.rendering

{-# INLINE unDriverStateX #-}
unDriverStateX :: (forall s act ps. DriverState m r s f act ps i o -> a) -> DriverStateX m r f i o -> a
unDriverStateX f (DriverStateX st) = f st

{-# SPECIALIZE initDriverState :: ComponentSpec s f act ps i o IO -> i -> (o -> IO ()) -> IORef (LifecycleHandlers IO) -> IO (DriverState IO r s f act ps i o) #-}
initDriverState
  :: (MonadIO m)
  => ComponentSpec s f act ps i o m
  -> i
  -> (o -> m ())
  -> IORef (LifecycleHandlers m)
  -> m (DriverState m r s f act ps i o)
initDriverState component input handler lchs = do
  selfRef <- newIORef (fix identity)
  childrenIn <- newIORef SlotStorage.empty
  childrenOut <- newIORef SlotStorage.empty
  handlerRef <- newIORef handler
  pendingQueries <- newIORef (Just [])
  pendingOuts <- newIORef (Just [])
  pendingHandlers <- newIORef Nothing
  fresh <- newIORef 1
  subscriptions <- newIORef (Just mempty)
  forks <- newIORef mempty
  state <- component.initialState input
  let ds =
        DriverState
          { component
          , state
          , refs = mempty
          , children = SlotStorage.empty
          , childrenIn
          , childrenOut
          , selfRef
          , handlerRef
          , pendingQueries
          , pendingOuts
          , pendingHandlers
          , rendering = Nothing
          , fresh
          , subscriptions
          , forks
          , lifecycleHandlers = lchs
          }
  atomicWriteIORef selfRef ds
  pure ds
