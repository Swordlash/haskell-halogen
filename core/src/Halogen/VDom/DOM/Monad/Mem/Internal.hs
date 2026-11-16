-- | The page a 'Halogen.VDom.DOM.Monad.Mem.MemDOM' program runs in.
--
-- A page holds what a browser tab has: a document, which is also its
-- window, local storage, session storage and IndexedDB. Two pages share
-- none of them.
--
-- This module is compiled on every backend, like
-- "Halogen.VDom.DOM.Monad.Mem". The document's tree is defined in
-- "Halogen.VDom.DOM.Monad.Native", which is compiled only natively and
-- with the @wasi@ flag. So the page holds its document as a 'Dynamic'.
module Halogen.VDom.DOM.Monad.Mem.Internal
  ( Page (..)
  , MemDatabase (..)
  , ambientPage
  , newPageWith
  )
where

import Data.Dynamic (Dynamic)
import HPrelude
import System.IO.Unsafe (unsafePerformIO)

-- | A page of the in-memory DOM.
data Page = Page
  { document :: Maybe Dynamic
  -- ^ The page's document, a @NativeNode@. 'Nothing' is the ambient
  -- document, which "Halogen.VDom.DOM.Monad.Native" keeps.
  , localStore :: IORef (Map Text Text)
  -- ^ Local storage.
  , sessionStore :: IORef (Map Text Text)
  -- ^ Session storage.
  , databases :: IORef (Map Text MemDatabase)
  -- ^ IndexedDB: every database, by name.
  }

-- | One database of the in-memory IndexedDB. It has a version and its
-- stores by name. Each store maps keys to values.
data MemDatabase = MemDatabase
  { version :: Int
  , stores :: Map Text (Map Text ByteString)
  }

-- | The page that 'Halogen.VDom.DOM.Monad.Mem.runMemDOM' uses. There is
-- one for the process. Its storage is never emptied, so a test can see data
-- persist from one program to the next.
{-# NOINLINE ambientPage #-}
ambientPage :: Page
ambientPage = unsafePerformIO (fresh Nothing)

-- | Create a page with this document and empty storage.
newPageWith :: Dynamic -> IO Page
newPageWith = fresh . Just

fresh :: Maybe Dynamic -> IO Page
fresh document = Page document <$> newIORef mempty <*> newIORef mempty <*> newIORef mempty
