module Data.MutVarF where

import Control.Monad.Primitive
import Data.Primitive
import Protolude

atomicModifyMutVar'_ :: (PrimMonad m) => MutVar (PrimState m) a -> (a -> a) -> m ()
atomicModifyMutVar'_ var f = atomicModifyMutVar' var ((,()) . f)

atomicWriteMutVar :: (PrimMonad m) => MutVar (PrimState m) a -> a -> m ()
atomicWriteMutVar var v = atomicModifyMutVar'_ var (const v)
