module Hascript.Monad where

import Protolude hiding (Ap)
import Control.Monad.Free.Church
import Control.Applicative.Free
import Hascript.Subscription
import Web.DOM.Monad
import Hascript.Query

newtype SubscriptionId = SubscriptionId Int

newtype ForkId = ForkId Int

data HascriptF state action slots output m a
  = State (state -> (a, state))
  | Subscribe (SubscriptionId -> Emitter action) (SubscriptionId -> a)
  | Unsubscribe SubscriptionId a
  | Lift (m a)
  | ChildQuery (ChildQuery slots a)
  | Raise output a
  | Par (HascriptAp state action slots output m a)
  | Fork (HascriptM state action slots output m ()) (ForkId -> a)
  | Join ForkId a
  | Kill ForkId a
  | GetRef RefLabel (Maybe (Element m) -> a)
  deriving Functor

newtype HascriptM state action slots output m a 
  = HascriptM (F (HascriptF state action slots output m) a)
  deriving (Functor, Applicative, Monad)

newtype HascriptAp state action slots output m a 
  = HascriptAp (Ap (HascriptF state action slots output m) a)
  deriving (Functor, Applicative)


getM :: Functor m => HascriptM state _ _ _ m state
getM = HascriptM $ liftF $ State $ \s -> (s, s)

putM :: Functor m => state -> HascriptM state _ _ _ m ()
putM s = HascriptM $ liftF $ State (const ((), s))

