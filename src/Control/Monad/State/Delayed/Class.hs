module Control.Monad.State.Delayed.Class where

import HPrelude

class (MonadState s m) => MonadDelayedState s m | m -> s where
  commit :: m ()
