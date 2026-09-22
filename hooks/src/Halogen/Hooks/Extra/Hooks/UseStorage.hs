{-# LANGUAGE QualifiedDo #-}

-- | State that outlives the page.
--
-- The value is read out of the browser's storage when the component mounts and
-- written back whenever it changes, so a reload finds it where it was left. A
-- component whose key changes while it is mounted reads the new key's value in
-- the same way, rather than carrying the old one over to it.
-- How it is written is 'Storage.StorageSerialize', which for most types means
-- the JSON they already have instances for; where it is written is
-- "Web.Storage.Storage", one object per store.
--
-- The state is an 'Either' because a store can hold something this version of
-- the program cannot read — written by an older one, or by something else
-- entirely — and the alternatives are to lose it or to crash on it.
module Halogen.Hooks.Extra.Hooks.UseStorage
  ( UseStorage
  , useLocalStorage
  , useSessionStorage
  , useStorageWith
  )
where

import Data.IORef (readIORef, writeIORef)
import Halogen.Hooks qualified as Hooks
import Halogen.Hooks.Types (Hook, HookK (..), HookM)
import Halogen.VDom.DOM.Monad (MonadBrowserDOM, StorageKind (..))
import Protolude
import Web.Storage.Storage (StorageSerialize)
import Web.Storage.Storage qualified as Storage

-- | The hooks the storage hooks use: the value, which store and key it was
-- read from, the effect that reads it and the effect that writes it back.
type UseStorage a hooks =
  UseState (Either Text a)
    : UseRef (Maybe (StorageKind, Text))
    : UseEffect (StorageKind, Text)
    : UseEffect (Either Text a)
    : hooks

-- | The value, and how to change it. Changing it writes it back.
type Stored scope slots output m a =
  (Either Text a, (Either Text a -> Either Text a) -> HookM scope slots output m ())

-- | State kept in @window.localStorage@, which outlives the tab.
useLocalStorage
  :: forall a scope q slots output m hooks
   . (Eq a, StorageSerialize a, MonadIO m, MonadBrowserDOM m)
  => Text
  -- ^ the key to keep it under
  -> a
  -- ^ what it is when nothing has been kept yet
  -> Hook scope q slots output m (UseStorage a hooks) hooks (Stored scope slots output m a)
useLocalStorage = useStorageWith LocalStorage

-- | State kept in @window.sessionStorage@, which is emptied when the tab is
-- closed.
useSessionStorage
  :: forall a scope q slots output m hooks
   . (Eq a, StorageSerialize a, MonadIO m, MonadBrowserDOM m)
  => Text
  -> a
  -> Hook scope q slots output m (UseStorage a hooks) hooks (Stored scope slots output m a)
useSessionStorage = useStorageWith SessionStorage

-- | The two above, given the store to keep the value in.
useStorageWith
  :: forall a scope q slots output m hooks
   . (Eq a, StorageSerialize a, MonadIO m, MonadBrowserDOM m)
  => StorageKind
  -> Text
  -> a
  -> Hook scope q slots output m (UseStorage a hooks) hooks (Stored scope slots output m a)
useStorageWith kind key initial = Hooks.do
  (value, valueId) <- Hooks.useState (Right initial)
  (_, loadedFrom) <- Hooks.useRef Nothing

  -- Read when the component mounts, and again whenever the store or the key
  -- changes under it. What is on show belongs to a key: when that is no longer
  -- the key being asked for, neither is the value, and leaving it there would
  -- show one key's state and then save it under another's.
  Hooks.useTickEffect (kind, key) $ do
    stored <- lift $ Storage.getItem kind key
    case stored of
      -- Nothing kept yet: start the store off at the default rather than
      -- leaving it empty until something changes.
      Nothing -> do
        lift $ Storage.setItem kind key initial
        current <- Hooks.get valueId
        -- On a mount that is what the state already holds, and putting it
        -- again would cost a render for nothing. After a change of key it is
        -- not: the state is still showing the key before it.
        when (current /= Right initial) $ Hooks.put valueId (Right initial)
      Just found -> Hooks.put valueId found
    liftIO $ writeIORef loadedFrom (Just (kind, key))
    pure Nothing

  Hooks.useTickEffect value $ do
    -- Only once this key has been read, which the mount and every later change
    -- of key leave undone for as long as it takes the effect above to run. The
    -- store is the authority until then, and writing here would put the
    -- default, or the key before this one's value, over what is kept.
    loaded <- liftIO $ readIORef loadedFrom
    when (loaded == Just (kind, key)) $ do
      -- What the state holds now rather than what this render saw: the effect
      -- above runs first, and what it put there came from the store.
      current <- Hooks.get valueId
      for_ current $ \v -> lift $ Storage.setItem kind key v
    pure Nothing

  Hooks.pure (value, Hooks.modify_ valueId)
