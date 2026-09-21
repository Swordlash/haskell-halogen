{-# LANGUAGE QualifiedDo #-}

-- | State that outlives the page.
--
-- The value is read out of the browser's storage when the component mounts and
-- written back whenever it changes, so a reload finds it where it was left.
--
-- What a store holds is text, and what a component holds is a value, so the
-- hook is given both directions and the state is an 'Either': a store can
-- contain something this version of the program cannot read, and pretending
-- otherwise would mean either losing it or crashing on it.
module Halogen.Hooks.Extra.Hooks.UseStorage
  ( UseStorage
  , StorageInterface (..)
  , useLocalStorage
  , useSessionStorage
  , useStorageWith
  )
where

import Data.IORef (atomicModifyIORef')
import Halogen.Hooks qualified as Hooks
import Halogen.Hooks.Types (Hook, HookK (..), HookM)
import Halogen.VDom.DOM.Monad (MonadBrowserDOM, window)
import Protolude
import Web.DOM.Internal.Types (Window)
import Web.HTML.Window qualified as Window
import Web.Storage.Storage (Storage)
import Web.Storage.Storage qualified as Storage

-- | Which key to keep the value under, what to do when there is nothing there,
-- and how the value and its text stand for each other.
data StorageInterface a = StorageInterface
  { key :: Text
  , defaultValue :: a
  , encode :: a -> Text
  , decode :: Text -> Either Text a
  }

-- | The hooks the storage hooks use: the value, whether it has been loaded
-- yet, the effect that loads it and the effect that writes it back.
type UseStorage a hooks =
  UseState (Either Text a)
    : UseRef Bool
    : UseEffect ()
    : UseEffect (Either Text a)
    : hooks

-- | State kept in @window.localStorage@, which outlives the tab.
useLocalStorage
  :: forall a scope q slots output m hooks
   . (Eq a, MonadIO m, MonadBrowserDOM m)
  => StorageInterface a
  -> Hook scope q slots output m (UseStorage a hooks) hooks (Either Text a, (Either Text a -> Either Text a) -> HookM scope slots output m ())
useLocalStorage = useStorageWith Window.localStorage

-- | State kept in @window.sessionStorage@, which is emptied when the tab is
-- closed.
useSessionStorage
  :: forall a scope q slots output m hooks
   . (Eq a, MonadIO m, MonadBrowserDOM m)
  => StorageInterface a
  -> Hook scope q slots output m (UseStorage a hooks) hooks (Either Text a, (Either Text a -> Either Text a) -> HookM scope slots output m ())
useSessionStorage = useStorageWith Window.sessionStorage

-- | The two above, given the store to keep the value in.
useStorageWith
  :: forall a scope q slots output m hooks
   . (Eq a, MonadIO m, MonadBrowserDOM m)
  => (Window -> m Storage)
  -> StorageInterface a
  -> Hook scope q slots output m (UseStorage a hooks) hooks (Either Text a, (Either Text a -> Either Text a) -> HookM scope slots output m ())
useStorageWith openStore StorageInterface {key, defaultValue, encode, decode} = Hooks.do
  (value, valueId) <- Hooks.useState (Right defaultValue)
  (_, loaded) <- Hooks.useRef False

  Hooks.useLifecycleEffect $ do
    store <- storage
    stored <- lift $ Storage.getItem key store
    case stored of
      -- Nothing kept yet: start the store off at the default rather than
      -- leaving it empty until something changes.
      Nothing -> lift $ Storage.setItem key (encode defaultValue) store
      Just text -> Hooks.put valueId (decode text)
    pure Nothing

  Hooks.useTickEffect value $ do
    -- The first run of this effect is the mount, where the value is still the
    -- default and the store is the authority: writing here would overwrite
    -- what the effect above is in the middle of reading.
    mounting <- liftIO $ atomicModifyIORef' loaded (\seen -> (True, not seen))
    unless mounting $ for_ value $ \v -> do
      store <- storage
      lift $ Storage.setItem key (encode v) store
    pure Nothing

  Hooks.pure (value, Hooks.modify_ valueId)
  where
    storage :: HookM scope slots output m Storage
    storage = lift $ openStore =<< window
