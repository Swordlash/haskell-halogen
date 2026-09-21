-- | What a browser keeps for a page between visits.
--
-- A store is one JSON object, kept under a single key: its keys are the keys
-- a program stores things under, and its values are the base64 of whatever
-- "Web.Storage.Serialize" turned those things into. Base64 because the bytes a
-- serializer produces are bytes — a store holds text, and not every byte is
-- text.
--
-- Keeping the whole store as one object is what makes it the same object
-- everywhere: 'Halogen.VDom.DOM.Monad.Class.MonadBrowserDOM' asks a backend
-- only to read and write that text, so the browser keeps it in
-- @window.localStorage@ and the in-memory DOM keeps it in an 'IORef', and
-- neither has to know what is in it.
module Web.Storage.Storage
  ( StorageKind (..)
  , storageKey

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

    -- * The object itself
  , parseStorageObject
  , renderStorageObject
  , module Web.Storage.Serialize
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as M
import Data.Text.Encoding qualified as TE
import HPrelude
import Halogen.VDom.DOM.Monad.Class (MonadBrowserDOM (..), StorageKind (..), storageKey)
import Web.Storage.Serialize

-- | A store's contents: the bytes kept under each key, base64 having been
-- undone.
type StorageObject = Map Text ByteString

-- | What is kept under a key, if anything, read back as the value it was
-- written from — or the reason it could not be.
getItem :: forall a m. (MonadBrowserDOM m, StorageSerialize a) => StorageKind -> Text -> m (Maybe (Either Text a))
getItem kind key = fmap fromStorageBytes . M.lookup key <$> readStorageObject kind

-- | Keep a value under a key, replacing whatever was there.
setItem :: forall a m. (MonadBrowserDOM m, StorageSerialize a) => StorageKind -> Text -> a -> m ()
setItem kind key value = do
  object <- readStorageObject kind
  writeStorageObject kind $ M.insert key (toStorageBytes value) object

-- | Forget a key.
removeItem :: forall m. (MonadBrowserDOM m) => StorageKind -> Text -> m ()
removeItem kind key = do
  object <- readStorageObject kind
  writeStorageObject kind $ M.delete key object

-- | Whether anything is kept under a key.
member :: forall m. (MonadBrowserDOM m) => StorageKind -> Text -> m Bool
member kind key = M.member key <$> readStorageObject kind

-- | Every key the store holds.
keys :: forall m. (MonadBrowserDOM m) => StorageKind -> m [Text]
keys kind = M.keys <$> readStorageObject kind

-- | Forget everything in the store.
clear :: forall m. (MonadBrowserDOM m) => StorageKind -> m ()
clear kind = writeStorageObject kind M.empty

-- | The whole store.
--
-- A store anything can write to can hold anything, so what cannot be read as
-- this object is read as an empty one rather than as an error: the alternative
-- is a page that cannot start because something else left a mess behind.
readStorageObject :: forall m. (MonadBrowserDOM m) => StorageKind -> m StorageObject
readStorageObject kind = parseStorageObject <$> readStorage kind

-- | Replace the whole store.
writeStorageObject :: forall m. (MonadBrowserDOM m) => StorageKind -> StorageObject -> m ()
writeStorageObject kind = writeStorage kind . renderStorageObject

-- | Read the object out of the text a store keeps.
parseStorageObject :: Text -> StorageObject
parseStorageObject =
  maybe M.empty (M.mapMaybe decodeValue)
    . Aeson.decodeStrict'
    . TE.encodeUtf8
  where
    decodeValue :: Text -> Maybe ByteString
    decodeValue = rightToMaybe . Base64.decode . TE.encodeUtf8

-- | Render the object a store keeps.
renderStorageObject :: StorageObject -> Text
renderStorageObject =
  TE.decodeUtf8
    . BSL.toStrict
    . Aeson.encode
    . M.map (TE.decodeUtf8 . Base64.encode)
