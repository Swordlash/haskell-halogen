-- {-# LANGUAGE FieldSelectors #-}
module Data.NT where

-- Natural transformation
newtype m ~> n = NT {runNT :: forall a. m a -> n a}

runNT :: (m ~> n) -> m a -> n a
runNT (NT f) = f
