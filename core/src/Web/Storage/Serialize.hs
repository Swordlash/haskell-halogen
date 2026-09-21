-- | Turning a value into the bytes a store keeps, and back.
--
-- A store holds text, and what a program wants to keep is a value, so
-- something has to say how the two stand for each other. The default is JSON,
-- through @aeson@: a type that has 'ToJSON' and 'FromJSON' needs no more than
--
-- @
-- instance StorageSerialize MyValue
-- @
--
-- and anything else can say how it wants to be written by giving the two
-- methods. Either way the bytes are base64 on their way into the store — see
-- "Web.Storage.Storage" — so a serializer is free to produce whatever bytes
-- suit it.
module Web.Storage.Serialize
  ( StorageSerialize (..)
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Text.Encoding qualified as TE
import HPrelude

class StorageSerialize a where
  -- | The bytes to keep.
  toStorageBytes :: a -> ByteString
  default toStorageBytes :: (ToJSON a) => a -> ByteString
  toStorageBytes = BSL.toStrict . encode

  -- | What those bytes were, or why they could not be read: a store can hold
  -- something an older version of the program wrote, or something else wrote
  -- entirely.
  fromStorageBytes :: ByteString -> Either Text a
  default fromStorageBytes :: (FromJSON a) => ByteString -> Either Text a
  fromStorageBytes = first toS . eitherDecodeStrict'

-- | Text is kept as its UTF-8, not as a JSON string: what comes back out of
-- the store is then the text itself rather than the text in quotes.
instance StorageSerialize Text where
  toStorageBytes = TE.encodeUtf8
  fromStorageBytes = first show . TE.decodeUtf8'

-- | Bytes are kept as themselves.
instance StorageSerialize ByteString where
  toStorageBytes = identity
  fromStorageBytes = Right

instance StorageSerialize BSL.ByteString where
  toStorageBytes = BSL.toStrict
  fromStorageBytes = Right . BSL.fromStrict

-- The rest go through JSON, which is what the defaults do.
instance StorageSerialize Int

instance StorageSerialize Integer

instance StorageSerialize Double

instance StorageSerialize Bool

instance StorageSerialize ()

instance (ToJSON a, FromJSON a) => StorageSerialize (Maybe a)

instance (ToJSON a, FromJSON a) => StorageSerialize [a]
