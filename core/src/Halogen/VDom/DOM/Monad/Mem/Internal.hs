-- | What a 'Halogen.VDom.DOM.Monad.Mem.MemDOM' program runs in: its page.
--
-- A page is what a browser tab has one of: a document, which is also its
-- window, local and session storage, and IndexedDB. The in-memory DOM
-- keeps them here so that two pages share nothing.
--
-- This module is compiled on every backend, as
-- "Halogen.VDom.DOM.Monad.Mem" is. The document's tree is not: it is
-- in "Halogen.VDom.DOM.Monad.Native", compiled natively and with the
-- @wasi@ flag. So the page holds its document as a 'Dynamic', which that
-- module reads back.
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

-- | A database as the in-memory IndexedDB keeps it: its version, and its
-- stores by name, each with its keys and values.
data MemDatabase = MemDatabase
  { version :: Int
  , stores :: Map Text (Map Text ByteString)
  }

-- | The page 'Halogen.VDom.DOM.Monad.Mem.runMemDOM' runs in. There is one
-- for the process. Its stores are never emptied: that is how a test sees
-- something persist from one program to the next.
{-# NOINLINE ambientPage #-}
ambientPage :: Page
ambientPage = unsafePerformIO (fresh Nothing)

-- | A page with this document and nothing stored.
newPageWith :: Dynamic -> IO Page
newPageWith = fresh . Just

fresh :: Maybe Dynamic -> IO Page
fresh document = Page document <$> newIORef mempty <*> newIORef mempty <*> newIORef mempty
