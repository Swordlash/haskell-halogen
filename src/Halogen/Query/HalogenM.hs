module Halogen.Query.HalogenM where

import Control.Applicative.Free.Fast
import Control.Monad.Free.Church
import Halogen.Query.ChildQuery
import Halogen.Query.Input
import Halogen.Subscription
import Protolude hiding (Ap)
import Web.DOM.Element
import Control.Monad.Parallel

newtype SubscriptionId = SubscriptionId Int
  deriving newtype (Eq, Ord, Show)

newtype ForkId = ForkId Int
  deriving newtype (Eq, Ord, Show)

data HalogenF state action slots output m a
  = State (state -> (a, state))
  | Subscribe (SubscriptionId -> Emitter m action) (SubscriptionId -> a)
  | Unsubscribe SubscriptionId a
  | Lift (m a)
  | ChildQuery (ChildQuery slots a)
  | Raise output a
  | Par (HalogenAp state action slots output m a)
  | Fork (HalogenM state action slots output m ()) (ForkId -> a)
  | Join ForkId a
  | Kill ForkId a
  | GetRef RefLabel (Maybe Element -> a)
  deriving (Functor)

newtype HalogenM state action slots output m a
  = HalogenM (F (HalogenF state action slots output m) a)
  deriving (Functor, Applicative, Monad)

newtype HalogenAp state action slots output m a
  = HalogenAp (Ap (HalogenM state action slots output m) a)
  deriving (Functor, Applicative)

getM :: (Functor m) => HalogenM state _ _ _ m state
getM = HalogenM $ liftF $ State $ \s -> (s, s)

putM :: (Functor m) => state -> HalogenM state _ _ _ m ()
putM s = HalogenM $ liftF $ State (const ((), s))

instance Functor m => MonadParallel (HalogenM state action slots output m) where
  type Parallel (HalogenM state action slots output m) = HalogenAp state action slots output m
  parallel = HalogenAp . liftAp
  sequential = HalogenM . liftF . Par