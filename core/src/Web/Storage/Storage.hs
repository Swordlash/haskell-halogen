-- | The Web Storage API.
--
-- 'Web.HTML.Window.localStorage' and 'Web.HTML.Window.sessionStorage' both
-- hand back one of these; they differ only in how long what is written to them
-- lasts.
--
-- Off the browser backends there is nowhere to put anything, so a read finds
-- nothing and a write goes nowhere. That is a component written for the
-- browser still running against the in-memory DOM, with its persistence
-- quietly absent — not an error, because there is nothing wrong with asking to
-- store something where nothing can be stored.
module Web.Storage.Storage
  ( Storage (..)
  , getItem
  , setItem
  , removeItem
  , clear
  , itemCount
  , keyAt
  )
where

import Data.Foreign
import HPrelude

newtype Storage = Storage (Foreign Storage)

-- | What is stored under this key, if anything.
getItem :: (MonadIO m) => Text -> Storage -> m (Maybe Text)

-- | Store a value under a key, replacing whatever was there.
setItem :: (MonadIO m) => Text -> Text -> Storage -> m ()

-- | Remove whatever is stored under a key.
removeItem :: (MonadIO m) => Text -> Storage -> m ()

-- | Remove everything.
clear :: (MonadIO m) => Storage -> m ()

-- | How many keys there are. The DOM calls this @length@.
itemCount :: (MonadIO m) => Storage -> m Int

-- | The nth key, in whatever order the browser keeps them. The DOM calls this
-- @key@.
keyAt :: (MonadIO m) => Int -> Storage -> m (Maybe Text)

#if defined(javascript_HOST_ARCH)
foreign import javascript unsafe "js_storage_get_item" js_storage_get_item :: Foreign Text -> Storage -> IO (Nullable Text)
foreign import javascript unsafe "js_storage_set_item" js_storage_set_item :: Foreign Text -> Foreign Text -> Storage -> IO ()
foreign import javascript unsafe "js_storage_remove_item" js_storage_remove_item :: Foreign Text -> Storage -> IO ()
foreign import javascript unsafe "js_storage_clear" js_storage_clear :: Storage -> IO ()
foreign import javascript unsafe "js_storage_length" js_storage_length :: Storage -> IO (Foreign Int)
foreign import javascript unsafe "js_storage_key" js_storage_key :: Int -> Storage -> IO (Nullable Text)
#elif defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "$2.getItem($1)" js_storage_get_item :: Foreign Text -> Storage -> IO (Nullable Text)
foreign import javascript unsafe "$3.setItem($1, $2)" js_storage_set_item :: Foreign Text -> Foreign Text -> Storage -> IO ()
foreign import javascript unsafe "$2.removeItem($1)" js_storage_remove_item :: Foreign Text -> Storage -> IO ()
foreign import javascript unsafe "$1.clear()" js_storage_clear :: Storage -> IO ()
foreign import javascript unsafe "$1.length" js_storage_length :: Storage -> IO (Foreign Int)
foreign import javascript unsafe "$2.key($1)" js_storage_key :: Int -> Storage -> IO (Nullable Text)
#endif

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
getItem key storage = liftIO $ fmap foreignToString . nullableToMaybe <$> js_storage_get_item (stringToForeign key) storage
setItem key value storage = liftIO $ js_storage_set_item (stringToForeign key) (stringToForeign value) storage
removeItem key storage = liftIO $ js_storage_remove_item (stringToForeign key) storage
clear = liftIO . js_storage_clear
itemCount storage = liftIO $ foreignToInt <$> js_storage_length storage
keyAt index storage = liftIO $ fmap foreignToString . nullableToMaybe <$> js_storage_key index storage
#else
getItem _ _ = pure Nothing
setItem _ _ _ = pass
removeItem _ _ = pass
clear _ = pass
itemCount _ = pure 0
keyAt _ _ = pure Nothing
#endif
