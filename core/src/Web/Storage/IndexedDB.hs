{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A key-value store over the browser's IndexedDB: what a page keeps that
-- is too big, too binary or too much for "Web.Storage.Storage".
--
-- Web Storage holds a few megabytes of text, synchronously, under one key at
-- a time. IndexedDB holds as much as the browser lets the origin have (tens
-- of megabytes at least), keeps bytes as bytes, and can change several things
-- at once in a transaction; but it answers later, so every call here waits.
-- Call it from a component's @lift@ (which waits on a worker thread while the
-- rest of the tree goes on), never from @H.liftEffect@, which must not wait.
--
-- A 'Database' says what a program expects to find: a name, a version and
-- the stores in it. Stores can only be made while a database is being
-- upgraded to a new version, so adding a store is a version bump; a store
-- that is named without one is reported as 'StoreNotFound'. Each store maps
-- keys of text to bytes, kept as a @Uint8Array@ (never as base64, as Web
-- Storage has to), and 'getItem' and 'setItem' read and write a value through
-- "Web.Storage.Serialize" as "Web.Storage.Storage" does. A database's name is
-- its namespace, so unlike Web Storage nothing is prefixed.
--
-- > pages :: Database
-- > pages = Database {name = "wiki", version = 1, stores = ["pages", "attachments"]}
-- >
-- > save :: (MonadIndexedDB m) => Text -> ByteString -> [(Text, ByteString)] -> m ()
-- > save page markdown attachments =
-- >   writeMany pages $
-- >     ("pages", page, Just markdown) : [("attachments", key, Just bytes) | (key, bytes) <- attachments]
--
-- What fails is thrown as an 'IndexedDBError', an exception rather than an
-- 'Either' in every result, because what can fail (another tab holding the
-- database open at an older version, a full disk, a program older than the
-- database it finds) is rare, and the same for every call: a program that
-- cares catches it around its own unit of work, once.
--
-- The browser instance ('BrowserDOM', both browser backends) keeps one open
-- connection per database name. It is opened by the first call, at the
-- version that call's 'Database' asks for, and reopened when a call asks for
-- another; it closes itself when another tab upgrades the database (so as
-- not to block it), and the next call then opens it again, failing with
-- 'VersionTooLow' if this program is now older than the database. The
-- in-memory instance ('MemDOM') keeps every database in one global
-- 'Data.IORef.IORef', and behaves the same.
--
-- Calls made one after another happen in that order. Calls made at once
-- from different threads happen in the order they reach the browser (and
-- each 'writeMany' wholly before or after another): a transaction that
-- writes to a store waits for every earlier one on the same store.
module Web.Storage.IndexedDB
  ( -- * Databases
    Database (..)
  , StoreName (..)

    -- * The store
  , MonadIndexedDB (..)
  , Write

    -- * Values
  , getItem
  , setItem
  , getAllItems
  , itemWrite
  , itemDeletion

    -- * Failures
  , IndexedDBError (..)
  , IndexedDBFailure (..)
  , module Web.Storage.Serialize
  )
where

import Control.Monad.Trans.Identity (IdentityT (..))
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import HPrelude hiding (get, getAll, put)
import Halogen.JSBits (JSText, JSVal, Safety (..), browserJS, fromJSBytes, fromJSText, inBrowser, isNull, jsValText, toJSBytes, toJSText)
import Halogen.VDom.DOM.Monad.Browser (BrowserDOM)
import Halogen.VDom.DOM.Monad.Mem (MemDOM)
import System.IO.Unsafe (unsafePerformIO)
import Web.Storage.Serialize

-- | What a program expects a database to be.
--
-- The stores are made when the database is created or upgraded, that is
-- when it is opened at a version later than the one it has, and only the
-- ones it does not have yet: a store that is no longer named keeps what it
-- holds, and a store that is named needs a version later than the last.
-- Versions start at 1.
data Database = Database
  { name :: Text
  , version :: Int
  , stores :: [StoreName]
  }
  deriving stock (Eq, Show)

-- | The name of one of a database's stores.
newtype StoreName = StoreName Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

-- | One write of a 'writeMany': into which store, under which key, and the
-- bytes to keep there, or 'Nothing' to delete what is.
type Write = (StoreName, Text, Maybe ByteString)

-- | What went wrong, and in which database.
data IndexedDBError = IndexedDBError
  { database :: Text
  , failure :: IndexedDBFailure
  }
  deriving stock (Eq, Show)

instance Exception IndexedDBError

data IndexedDBFailure
  = -- | There is no IndexedDB here, or this page may not use it: natively
    -- in the browser instance, under Node, in a browser that turns it off
    -- (some do in a private window), or in a sandboxed frame or an opaque
    -- origin, whose opening is refused with a @SecurityError@.
    Unavailable
  | -- | The database had to be upgraded (or deleted), and another tab keeps it
    -- open at its older version. A tab running this library closes its
    -- connection when asked, so this is a tab that does not. The upgrade
    -- happens when that tab closes it; until then the call fails.
    UpgradeBlocked
  | -- | The database is at a later version than the one asked for: a newer
    -- version of the program has upgraded it since.
    VersionTooLow
  | -- | The database has no such store: it was added to the 'Database'
    -- without a version bump.
    StoreNotFound StoreName
  | -- | The browser would not let the origin keep any more.
    QuotaExceeded
  | -- | What is kept under this key is not bytes: something other than this
    -- library wrote it.
    NotBytes Text
  | -- | Anything else, by the name and message of the browser's error.
    OtherFailure Text Text
  deriving stock (Eq, Show)

-- | A key-value store of bytes, in databases of stores.
--
-- The 'ReaderT' and 'IdentityT' instances lift every method, and so would an
-- instance for another transformer, written without a body.
class (Monad m) => MonadIndexedDB m where
  -- | What a store keeps under a key, if anything.
  get :: Database -> StoreName -> Text -> m (Maybe ByteString)
  default get :: (LiftsIndexedDB t n m) => Database -> StoreName -> Text -> m (Maybe ByteString)
  get db store key = lift (get db store key)

  -- | Keep bytes under a key, replacing whatever was there.
  put :: Database -> StoreName -> Text -> ByteString -> m ()
  default put :: (LiftsIndexedDB t n m) => Database -> StoreName -> Text -> ByteString -> m ()
  put db store key bytes = lift (put db store key bytes)

  -- | Forget a key.
  delete :: Database -> StoreName -> Text -> m ()
  default delete :: (LiftsIndexedDB t n m) => Database -> StoreName -> Text -> m ()
  delete db store key = lift (delete db store key)

  -- | Every key a store holds, in IndexedDB's order: by UTF-16 code units,
  -- which differs from 'Text''s order only in putting a character beyond
  -- U+FFFF before one from U+E000 to U+FFFF. A key that is not text
  -- (something other than this library wrote it) is left out.
  keys :: Database -> StoreName -> m [Text]
  default keys :: (LiftsIndexedDB t n m) => Database -> StoreName -> m [Text]
  keys db store = lift (keys db store)

  -- | Everything a store holds, read at one moment. Being a 'Map', it is in
  -- 'Text''s order rather than the store's; 'keys' has the store's.
  getAll :: Database -> StoreName -> m (Map Text ByteString)
  default getAll :: (LiftsIndexedDB t n m) => Database -> StoreName -> m (Map Text ByteString)
  getAll db store = lift (getAll db store)

  -- | Forget everything a store holds.
  clear :: Database -> StoreName -> m ()
  default clear :: (LiftsIndexedDB t n m) => Database -> StoreName -> m ()
  clear db store = lift (clear db store)

  -- | Make these writes, to any of the database's stores, in one
  -- transaction: either all of them are kept or, if one fails, none is. They
  -- are made in order, so of two to one key the later wins. This is how a
  -- record and what belongs to it (a page and its attachments) are saved
  -- together, so that a reload never finds one without the other.
  writeMany :: Database -> [Write] -> m ()
  default writeMany :: (LiftsIndexedDB t n m) => Database -> [Write] -> m ()
  writeMany db writes = lift (writeMany db writes)

  -- | Delete a database, its stores and everything in them; the next call
  -- that names it creates it afresh. Nothing happens if there is none.
  deleteDatabase :: Text -> m ()
  default deleteDatabase :: (LiftsIndexedDB t n m) => Text -> m ()
  deleteDatabase = lift . deleteDatabase

-- | What a transformer over a 'MonadIndexedDB' needs for the lifted defaults.
type LiftsIndexedDB t n m = (MonadTrans t, MonadIndexedDB n, m ~ t n)

instance (MonadIndexedDB m) => MonadIndexedDB (ReaderT r m)

instance (MonadIndexedDB m) => MonadIndexedDB (IdentityT m)

--------------------------------------------------------------------------------
-- Values

-- | What is kept under a key, if anything, read back as the value it was
-- written from, or the reason it could not be.
getItem :: forall a m. (MonadIndexedDB m, StorageSerialize a) => Database -> StoreName -> Text -> m (Maybe (Either Text a))
getItem db store key = fmap fromStorageBytes <$> get db store key

-- | Keep a value under a key, replacing whatever was there.
setItem :: forall a m. (MonadIndexedDB m, StorageSerialize a) => Database -> StoreName -> Text -> a -> m ()
setItem db store key = put db store key . toStorageBytes

-- | Every value a store holds, each read back as the value it was written
-- from, or the reason it could not be.
getAllItems :: forall a m. (MonadIndexedDB m, StorageSerialize a) => Database -> StoreName -> m (Map Text (Either Text a))
getAllItems db store = map fromStorageBytes <$> getAll db store

-- | A 'Write' that keeps a value, for 'writeMany'.
itemWrite :: forall a. (StorageSerialize a) => StoreName -> Text -> a -> Write
itemWrite store key value = (store, key, Just (toStorageBytes value))

-- | A 'Write' that forgets a key, for 'writeMany'.
itemDeletion :: StoreName -> Text -> Write
itemDeletion store key = (store, key, Nothing)

--------------------------------------------------------------------------------
-- The browser

-- Each call is described by one binding, which returns at once, and done by
-- 'js_idb_run', which waits: an awaited binding may only return nothing on
-- the JavaScript backend, so the outcome is left on the operation and read
-- afterwards. See @jsbits/indexeddb.js@.
$( browserJS
     ["jsbits/indexeddb.js"]
     [ ("js_idb_database", "halogen_idb_database", Unsafe, [t|JSText -> Int -> IO JSVal|])
     , ("js_idb_add_store", "halogen_idb_add_store", Unsafe, [t|JSVal -> JSText -> IO ()|])
     , ("js_idb_get", "halogen_idb_get", Unsafe, [t|JSVal -> JSText -> JSText -> IO JSVal|])
     , ("js_idb_put", "halogen_idb_put", Unsafe, [t|JSVal -> JSText -> JSText -> JSVal -> IO JSVal|])
     , ("js_idb_delete", "halogen_idb_delete", Unsafe, [t|JSVal -> JSText -> JSText -> IO JSVal|])
     , ("js_idb_keys", "halogen_idb_keys", Unsafe, [t|JSVal -> JSText -> IO JSVal|])
     , ("js_idb_get_all", "halogen_idb_get_all", Unsafe, [t|JSVal -> JSText -> IO JSVal|])
     , ("js_idb_clear", "halogen_idb_clear", Unsafe, [t|JSVal -> JSText -> IO JSVal|])
     , ("js_idb_batch", "halogen_idb_batch", Unsafe, [t|JSVal -> IO JSVal|])
     , ("js_idb_batch_put", "halogen_idb_batch_put", Unsafe, [t|JSVal -> JSText -> JSText -> JSVal -> IO ()|])
     , ("js_idb_batch_delete", "halogen_idb_batch_delete", Unsafe, [t|JSVal -> JSText -> JSText -> IO ()|])
     , ("js_idb_delete_database", "halogen_idb_delete_database", Unsafe, [t|JSText -> IO JSVal|])
     , ("js_idb_run", "halogen_idb_run", Safe, [t|JSVal -> IO ()|])
     , ("js_idb_failed", "halogen_idb_failed", Unsafe, [t|JSVal -> IO Bool|])
     , ("js_idb_error_name", "halogen_idb_error_name", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_idb_error_message", "halogen_idb_error_message", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_idb_result", "halogen_idb_result", Unsafe, [t|JSVal -> IO JSVal|])
     , ("js_idb_length", "halogen_idb_length", Unsafe, [t|JSVal -> IO Int|])
     , ("js_idb_index", "halogen_idb_index", Unsafe, [t|JSVal -> Int -> IO JSVal|])
     ]
 )

-- | The browser's IndexedDB, on both browser backends. Natively there is
-- none, and every call fails with 'Unavailable'.
instance MonadIndexedDB BrowserDOM where
  get db (StoreName store) key = liftIO $ do
    result <- perform db.name (descriptor db >>= \d -> js_idb_get d (toJSText store) (toJSText key))
    if isNull result then pure Nothing else Just <$> fromJSBytes result

  put db (StoreName store) key bytes = liftIO $ do
    array <- toJSBytes bytes
    void $ perform db.name (descriptor db >>= \d -> js_idb_put d (toJSText store) (toJSText key) array)

  delete db (StoreName store) key =
    liftIO . void $ perform db.name (descriptor db >>= \d -> js_idb_delete d (toJSText store) (toJSText key))

  keys db (StoreName store) = liftIO $ do
    result <- perform db.name (descriptor db >>= \d -> js_idb_keys d (toJSText store))
    map jsValText <$> elements result

  getAll db (StoreName store) = liftIO $ do
    result <- perform db.name (descriptor db >>= \d -> js_idb_get_all d (toJSText store))
    found <- map jsValText <$> (elements =<< js_idb_index result 0)
    bytes <- traverse fromJSBytes =<< elements =<< js_idb_index result 1
    pure $ M.fromList (zip found bytes)

  clear db (StoreName store) =
    liftIO . void $ perform db.name (descriptor db >>= \d -> js_idb_clear d (toJSText store))

  writeMany _ [] = pure ()
  writeMany db writes = liftIO . void . perform db.name $ do
    batch <- js_idb_batch =<< descriptor db
    for_ writes $ \(StoreName store, key, value) -> case value of
      Just bytes -> js_idb_batch_put batch (toJSText store) (toJSText key) =<< toJSBytes bytes
      Nothing -> js_idb_batch_delete batch (toJSText store) (toJSText key)
    pure batch

  deleteDatabase name = liftIO . void $ perform name (js_idb_delete_database (toJSText name))

-- | A 'Database' as the jsbits take it.
descriptor :: Database -> IO JSVal
descriptor db = do
  d <- js_idb_database (toJSText db.name) db.version
  for_ db.stores $ \(StoreName store) -> js_idb_add_store d (toJSText store)
  pure d

-- | Describe an operation, do it, and wait for its result, or throw what it
-- failed with.
--
-- The awaited binding is forced: on WebAssembly its @()@ comes back as a
-- thunk, and the thread waits for the promise only when that is forced.
perform :: Text -> IO JSVal -> IO JSVal
perform database describe = do
  unless inBrowser $ throwIO (IndexedDBError database Unavailable)
  operation <- describe
  evaluate =<< js_idb_run operation
  js_idb_failed operation >>= \case
    False -> js_idb_result operation
    True -> do
      name <- fromJSText <$> js_idb_error_name operation
      message <- fromJSText <$> js_idb_error_message operation
      throwIO . IndexedDBError database $ case name of
        "UnavailableError" -> Unavailable
        "BlockedError" -> UpgradeBlocked
        "VersionError" -> VersionTooLow
        "NotFoundError" -> StoreNotFound (StoreName message)
        "QuotaExceededError" -> QuotaExceeded
        "NotBytesError" -> NotBytes message
        _ -> OtherFailure name message

-- | The elements of a JavaScript array.
elements :: JSVal -> IO [JSVal]
elements array = do
  count <- js_idb_length array
  for [0 .. count - 1] (js_idb_index array)

--------------------------------------------------------------------------------
-- In memory

-- | A database as the in-memory instance keeps it.
data MemDatabase = MemDatabase
  { version :: Int
  , stores :: Map StoreName (Map Text ByteString)
  }

-- | Every database the in-memory instance holds, by name.
--
-- One global, as the browser has one IndexedDB per origin: every 'MemDOM' in
-- a process sees the same databases, which is how a test watches something
-- persist from one program to the next, and why a test that wants to start
-- from nothing deletes its database first (or uses a name of its own).
{-# NOINLINE ambientDatabases #-}
ambientDatabases :: IORef (Map Text MemDatabase)
ambientDatabases = unsafePerformIO (newIORef M.empty)

-- | IndexedDB as the browser has it, in memory: the same upgrades, the same
-- failures for a version that is too low or a store that was never made,
-- and 'writeMany' all or nothing. Nothing is ever blocked (there is no other
-- tab) and nothing is ever too much. On the browser backends it works too,
-- though nothing there uses it.
instance MonadIndexedDB MemDOM where
  get db store key = withStore db store $ \entries -> (entries, M.lookup key entries)
  put db store key bytes = withStore db store $ \entries -> (M.insert key bytes entries, ())
  delete db store key = withStore db store $ \entries -> (M.delete key entries, ())
  keys db store = withStore db store $ \entries -> (entries, sortBy utf16Order (M.keys entries))
  getAll db store = withStore db store $ \entries -> (entries, entries)
  clear db store = withStore db store $ const (M.empty, ())

  writeMany _ [] = pure ()
  writeMany db writes = withDatabase db $ \present ->
    case find (`M.notMember` present) [store | (store, _, _) <- writes] of
      Just missing -> Left (StoreNotFound missing)
      Nothing -> Right (foldl' write present writes, ())
    where
      write present (store, key, value) = M.adjust (M.alter (const value) key) store present

  deleteDatabase name = liftIO $ atomicModifyIORef'_ ambientDatabases (M.delete name)

-- | How IndexedDB orders keys of text: by their UTF-16 code units, as
-- JavaScript compares strings. 'Text' compares code points, which agrees
-- except that a character beyond U+FFFF is two code units from
-- U+D800 to U+DFFF, and so comes before one from U+E000 to U+FFFF.
utf16Order :: Text -> Text -> Ordering
utf16Order = comparing (concatMap units . T.unpack)
  where
    units :: Char -> [Int]
    units c
      | ord c < 0x10000 = [ord c]
      | otherwise = let n = ord c - 0x10000 in [0xD800 + n `div` 0x400, 0xDC00 + n `mod` 0x400]

-- | Do something to one store of a database.
withStore :: Database -> StoreName -> (Map Text ByteString -> (Map Text ByteString, a)) -> MemDOM a
withStore db store f = withDatabase db $ \present -> case M.lookup store present of
  Nothing -> Left (StoreNotFound store)
  Just entries -> Right (first (\changed -> M.insert store changed present) (f entries))

-- | Open a database, as the browser would, and do something to its stores,
-- all in one step. An upgrade that opening it makes is kept even if what is
-- done then fails, as in the browser, where opening is a step of its own.
withDatabase :: Database -> (Map StoreName (Map Text ByteString) -> Either IndexedDBFailure (Map StoreName (Map Text ByteString), a)) -> MemDOM a
withDatabase db f = liftIO $ do
  outcome <- atomicModifyIORef' ambientDatabases $ \databases ->
    case opened (M.lookup db.name databases) of
      Left failure -> (databases, Left failure)
      Right current -> case f current.stores of
        Left failure -> (M.insert db.name current databases, Left failure)
        Right (changed, a) -> (M.insert db.name (MemDatabase current.version changed) databases, Right a)
  either (throwIO . IndexedDBError db.name) pure outcome
  where
    opened :: Maybe MemDatabase -> Either IndexedDBFailure MemDatabase
    opened = \case
      _ | db.version < 1 -> Left (OtherFailure "TypeError" "a version is a positive number")
      Nothing -> Right (upgraded (MemDatabase db.version M.empty))
      Just current
        | db.version < current.version -> Left VersionTooLow
        | db.version > current.version -> Right (upgraded current)
        | otherwise -> Right current

    -- The stores the database does not have yet are made; the rest, named
    -- or not, are kept as they are.
    upgraded :: MemDatabase -> MemDatabase
    upgraded current =
      MemDatabase
        { version = db.version
        , stores = M.union current.stores (M.fromSet (const M.empty) (S.fromList db.stores))
        }
