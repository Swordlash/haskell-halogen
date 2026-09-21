{-# LANGUAGE QualifiedDo #-}

-- | State that outlives the page.
--
-- The value is read out of the browser's storage when the component mounts and
-- written back whenever it changes, so a reload finds it where it was left.
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

import Data.IORef (atomicModifyIORef')
import Halogen.Hooks qualified as Hooks
import Halogen.Hooks.Types (Hook, HookK (..), HookM)
import Halogen.VDom.DOM.Monad (MonadBrowserDOM, StorageKind (..))
import Protolude
import Web.Storage.Storage (StorageSerialize)
import Web.Storage.Storage qualified as Storage

-- | The hooks the storage hooks use: the value, whether it has been loaded
-- yet, the effect that loads it and the effect that writes it back.
type UseStorage a hooks =
  UseState (Either Text a)
    : UseRef Bool
    : UseEffect ()
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
  (_, loaded) <- Hooks.useRef False

  Hooks.useLifecycleEffect $ do
    stored <- lift $ Storage.getItem kind key
    case stored of
      -- Nothing kept yet: start the store off at the default rather than
      -- leaving it empty until something changes.
      Nothing -> lift $ Storage.setItem kind key initial
      Just found -> Hooks.put valueId found
    pure Nothing

  Hooks.useTickEffect value $ do
    -- The first run of this effect is the mount, where the value is still the
    -- default and the store is the authority: writing here would overwrite
    -- what the effect above is in the middle of reading.
    mounting <- liftIO $ atomicModifyIORef' loaded (\seen -> (True, not seen))
    unless mounting $ for_ value $ \v -> lift $ Storage.setItem kind key v
    pure Nothing

  Hooks.pure (value, Hooks.modify_ valueId)
