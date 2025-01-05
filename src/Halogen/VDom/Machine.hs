module Halogen.VDom.Machine
  ( Machine
  , Step (..)
  , extract
  , step
  , halt
  )
where

type Machine m a b = a -> m (Step m a b)

data Step m a b = forall s. Step b s (s -> a -> m (Step m a b)) (s -> m ())

{-# INLINE extract #-}
extract :: Step m a b -> b
extract (Step r _ _ _) = r

{-# INLINE step #-}
step :: Step m a b -> a -> m (Step m a b)
step (Step _ s stp _) = stp s

{-# INLINE halt #-}
halt :: Step m a b -> m ()
halt (Step _ s _ h) = h s
