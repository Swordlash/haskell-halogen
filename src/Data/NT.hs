-- {-# LANGUAGE FieldSelectors #-}
module Data.NT where

newtype m ~> n = NT {runNT :: forall a. m a -> n a}

runNT :: (m ~> n) -> m a -> n a
runNT (NT f) = f
