module Halogen.VDom.Machine where

type Machine m a b = a -> m (Step m a b)

data Step m a b = forall s. Step b s (s -> a -> m (Step m a b)) (s -> m ())

extract :: Step m a b -> b
extract (Step r _ _ _) = r

step :: Step m a b -> a -> m (Step m a b)
step (Step _ s stp _) = stp s

halt :: Step m a b -> m ()
halt (Step _ s _ h) = h s
