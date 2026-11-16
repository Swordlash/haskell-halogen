-- | The monad an in-memory document is spoken in.
--
-- The point of it is that the VDom machinery can be exercised under @cabal
-- test@ with no cross-compiler and no browser, and that a rendered tree can
-- be serialised to HTML.
--
-- A 'MemDOM' program runs in a page. A page has a document, which is also
-- its window, and its own storage. 'runMemDOM' uses the ambient page, which
-- is one for the process. To use another page, create it with
-- 'Halogen.VDom.DOM.Monad.Native.newPage' and run the program with
-- 'Halogen.VDom.DOM.Monad.Native.runMemDOMIn'. A tree stays in the page it
-- was mounted in. Its listeners, fibers and finalizers all run there.
--
-- The newtype is always in scope; the instance is not. It lives in
-- "Halogen.VDom.DOM.Monad.Native", alongside the tree it manipulates, and the
-- cabal file compiles that module natively and with the @wasi@ flag.
-- Browser builds use the real document instead.
module Halogen.VDom.DOM.Monad.Mem
  ( MemDOM (..)
  , runMemDOM
  )
where

import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad.Fork (MonadFork (..), MonadKill (..))
import Control.Monad.Parallel (MonadParallel (..))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.UUID (MonadUUID (..))
import HPrelude
import Halogen.VDom.DOM.Monad.Mem.Internal (Page, ambientPage)

-- | Everything the driver asks of a component monad is derived from 'IO',
-- so a backend can be the component monad directly and an application monad
-- can be a transformer stack over one. The monad reads its page and never
-- changes it.
newtype MemDOM a = MemDOM (ReaderT Page IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

-- | A forked program runs in the same page as its parent.
instance MonadFork MemDOM where
  type Fork MemDOM = Async
  fork (MemDOM m) = MemDOM (ReaderT (async . runReaderT m))
  join = liftIO . wait

instance MonadKill MemDOM where
  kill e f = liftIO (cancelWith f e)

-- | Every branch runs in the same page as the program that starts it.
instance MonadParallel MemDOM where
  type Parallel MemDOM = ReaderT Page (Concurrently IO)
  parallel (MemDOM m) = ReaderT (Concurrently . runReaderT m)
  sequential p = MemDOM (ReaderT (runConcurrently . runReaderT p))

instance MonadUUID MemDOM where
  generateV4 = liftIO generateV4

instance PrimMonad MemDOM where
  type PrimState MemDOM = PrimState IO
  primitive = liftIO . primitive

-- | Run a program in the ambient page.
runMemDOM :: MemDOM a -> IO a
runMemDOM (MemDOM m) = runReaderT m ambientPage
