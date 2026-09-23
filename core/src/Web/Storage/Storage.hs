-- | What a browser keeps for a page between visits.
--
-- A store holds one entry per key: the key, under the prefix that says whose
-- it is, and the base64 of whatever "Web.Storage.Serialize" turned the value
-- into. Base64 because the bytes a serializer produces are bytes — a store
-- holds text, and not every byte is text.
--
-- One entry per key rather than one object holding every key, because a store
-- is shared by every tab open on the page. An object has to be read, changed
-- and written back, and two tabs doing that at once leave only the later one's
-- change: the keys the earlier tab wrote go back to what they were, however
-- little the two had to do with each other. An entry of its own per key means
-- a tab only ever writes over what it meant to write over.
--
-- What a backend has to provide for any of this is one key of text at a time:
-- 'Halogen.VDom.DOM.Monad.Class.MonadBrowserDOM' asks it to read, write,
-- remove and list, so the browser keeps the entries in @window.localStorage@
-- and the in-memory DOM keeps them in an 'Data.IORef.IORef', and neither has
-- to know what is in them.
module Web.Storage.Storage
  ( StorageKind (..)
  , storagePrefix

    -- * One item at a time
  , getItem
  , setItem
  , removeItem
  , member
  , keys
  , clear

    -- * The store as a whole
  , StorageObject
  , readStorageObject
  , writeStorageObject
  , module Web.Storage.Serialize
  )
where

import Data.ByteString.Base64 qualified as Base64
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import HPrelude
import Halogen.VDom.DOM.Monad.Class (MonadBrowserDOM (..), StorageKind (..), storagePrefix)
import Web.Storage.Serialize

-- | A store's contents: the bytes kept under each key, base64 having been
-- undone.
type StorageObject = Map Text ByteString

-- | What is kept under a key, if anything, read back as the value it was
-- written from — or the reason it could not be.
getItem :: forall a m. (MonadBrowserDOM m, StorageSerialize a) => StorageKind -> Text -> m (Maybe (Either Text a))
getItem kind key = fmap (decodeBytes >=> fromStorageBytes) <$> readStorageItem kind (prefixed key)

-- | Keep a value under a key, replacing whatever was there.
setItem :: forall a m. (MonadBrowserDOM m, StorageSerialize a) => StorageKind -> Text -> a -> m ()
setItem kind key value = writeStorageItem kind (prefixed key) (encodeBytes $ toStorageBytes value)

-- | Forget a key.
removeItem :: forall m. (MonadBrowserDOM m) => StorageKind -> Text -> m ()
removeItem kind key = removeStorageItem kind (prefixed key)

-- | Whether anything is kept under a key.
member :: forall m. (MonadBrowserDOM m) => StorageKind -> Text -> m Bool
member kind key = isJust <$> readStorageItem kind (prefixed key)

-- | Every key the store holds for this program.
--
-- What anything else keeps in the same store is not this program's business
-- and is left out, which is what the prefix on every key written here is for.
keys :: forall m. (MonadBrowserDOM m) => StorageKind -> m [Text]
keys kind = mapMaybe (T.stripPrefix storagePrefix) <$> storageItemKeys kind

-- | Forget everything this program keeps in the store, and nothing else.
clear :: forall m. (MonadBrowserDOM m) => StorageKind -> m ()
clear kind = traverse_ (removeItem kind) =<< keys kind

-- | Everything the store holds for this program.
--
-- A store anything can write to can hold anything, so a value that cannot be
-- read back as bytes is left out rather than reported: the alternative is a
-- page that cannot start because something else left a mess behind. What one
-- key holds, and why it could not be read, is 'getItem'.
--
-- The entries are read one at a time and are not a snapshot of the store: a
-- tab writing to it while this runs can be caught halfway.
readStorageObject :: forall m. (MonadBrowserDOM m) => StorageKind -> m StorageObject
readStorageObject kind = do
  present <- keys kind
  M.fromList . catMaybes <$> for present readEntry
  where
    readEntry :: Text -> m (Maybe (Text, ByteString))
    readEntry key = do
      stored <- readStorageItem kind (prefixed key)
      pure $ (key,) <$> (rightToMaybe . decodeBytes =<< stored)

-- | Replace everything the store holds for this program: a key the object does
-- not have is forgotten.
--
-- One entry at a time, like everything else here, so a key written by another
-- tab between the reading and the writing is replaced rather than lost — and a
-- key belonging to something other than this program is not touched at all.
writeStorageObject :: forall m. (MonadBrowserDOM m) => StorageKind -> StorageObject -> m ()
writeStorageObject kind object = do
  present <- keys kind
  traverse_ (removeItem kind) (filter (`M.notMember` object) present)
  traverse_ (\(key, bytes) -> writeStorageItem kind (prefixed key) (encodeBytes bytes)) (M.toList object)

-- | The key a store keeps one of this program's keys under.
prefixed :: Text -> Text
prefixed = (storagePrefix <>)

-- | Undo the base64 an entry is kept as.
decodeBytes :: Text -> Either Text ByteString
decodeBytes = first toS . Base64.decode . TE.encodeUtf8

-- | What an entry is kept as.
encodeBytes :: ByteString -> Text
encodeBytes = TE.decodeUtf8 . Base64.encode
