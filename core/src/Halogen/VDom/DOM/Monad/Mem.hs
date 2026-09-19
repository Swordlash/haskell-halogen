-- | The monad an in-memory document is spoken in.
--
-- Unlike "Halogen.VDom.DOM.Monad.Browser" this one compiles on every
-- architecture, which is the point: it lets the VDom machinery be exercised
-- under @cabal test@ with no cross-compiler, and on the web backends too,
-- rather than only where the browser instance happens to be absent.
--
-- The instance lives in "Halogen.VDom.DOM.Monad.Native" alongside the tree it
-- manipulates.
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
