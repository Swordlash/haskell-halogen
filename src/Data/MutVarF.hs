module Data.MutVarF where

import Protolude
import Data.Primitive
import Control.Monad.Primitive


atomicModifyMutVar'_ :: PrimMonad m => MutVar (PrimState m) a -> (a -> a) -> m ()
atomicModifyMutVar'_ var f = atomicModifyMutVar' var ((,()) . f)

atomicWriteMutVar :: PrimMonad m => MutVar (PrimState m) a -> a -> m ()
atomicWriteMutVar var v = atomicModifyMutVar'_ var (const v)