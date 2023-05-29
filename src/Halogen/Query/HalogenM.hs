module Halogen.Query.HalogenM where

import Control.Applicative.Free
import Control.Monad.Free.Church
import Halogen.Query.ChildQuery
import Halogen.Query.Input
import Halogen.Subscription
import Halogen.VDom.DOM.Monad
import Protolude hiding (Ap)

newtype SubscriptionId = SubscriptionId Int

newtype ForkId = ForkId Int

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
  | GetRef RefLabel (Maybe (Element m) -> a)
  deriving (Functor)

newtype HalogenM state action slots output m a
  = HalogenM (F (HalogenF state action slots output m) a)
  deriving (Functor, Applicative, Monad)

newtype HalogenAp state action slots output m a
  = HalogenAp (Ap (HalogenF state action slots output m) a)
  deriving (Functor, Applicative)

getM :: Functor m => HalogenM state _ _ _ m state
getM = HalogenM $ liftF $ State $ \s -> (s, s)

putM :: Functor m => state -> HalogenM state _ _ _ m ()
putM s = HalogenM $ liftF $ State (const ((), s))
