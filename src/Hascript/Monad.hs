module Hascript.Monad where

import Protolude
import Control.Monad.Free

data HascriptF s a where
  GetState :: (s -> a) -> HascriptF s a
  PutState :: s -> a   -> HascriptF s a
  LiftIO   :: IO a     -> HascriptF s a
  deriving Functor

newtype HascriptM s a = HascriptM (Free (HascriptF s) a)
  deriving (Functor, Applicative, Monad, MonadFree (HascriptF s))

instance MonadState s (HascriptM s) where
  put s = liftF (PutState s ())
  get   = liftF (GetState identity)

instance MonadIO (HascriptM s) where
  liftIO = liftF . LiftIO