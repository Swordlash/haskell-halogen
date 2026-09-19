-- | The monad an in-memory document is spoken in.
--
-- The point of it is that the VDom machinery can be exercised under @cabal
-- test@ with no cross-compiler and no browser, and that a rendered tree can
-- be serialised to HTML.
--
-- The newtype is always in scope; the instance is not. It lives in
-- "Halogen.VDom.DOM.Monad.Native", alongside the tree it manipulates, and the
-- cabal file compiles that module only on native - so on the JavaScript and
-- wasm backends this monad exists but cannot be run. Those backends have a
-- real document to render into, which is what their tests use instead.
module Halogen.VDom.DOM.Monad.Mem
  ( MemDOM (..)
  , runMemDOM
  )
where

import Control.Monad.Primitive (PrimMonad (..))
import HPrelude

newtype MemDOM a = MemDOM (IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance PrimMonad MemDOM where
  type PrimState MemDOM = PrimState IO
  primitive = MemDOM . primitive

runMemDOM :: MemDOM a -> IO a
runMemDOM (MemDOM io) = io
