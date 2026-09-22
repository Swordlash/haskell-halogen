{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | What a store keeps, and what shape it keeps it in.
--
-- The in-memory DOM has the same two stores the browser has, so all of this is
-- exercised without one — on native, which is where that backend has its
-- instance. The browser backends have real stores and no browser to run these
-- under, so there they say nothing.
module Test.Storage (spec) where

import Prelude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "storage" $ pure ()

#else


import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)
import Halogen.VDom.DOM.Monad (MemDOM, StorageKind (..), readStorageItem, removeStorageItem, runMemDOM, writeStorageItem)
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual)
import Web.Storage.Storage qualified as Storage

-- | A type that says nothing about how it is stored: the class writes it as
-- the JSON its own instances describe.
data Settings = Settings
  { theme :: Text
  , fontSize :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Storage.StorageSerialize)

-- | Each test starts from an empty store: the in-memory one is global, and
-- deliberately outlives the component that wrote to it.
emptied :: MemDOM a -> IO a
emptied act = runMemDOM $ do
  Storage.clear LocalStorage
  Storage.clear SessionStorage
  act

-- | 'assertEqual', where these tests run.
expect :: (Eq a, Show a) => String -> a -> a -> MemDOM ()
expect message expected actual = liftIO (assertEqual message expected actual)

spec :: Spec
spec = describe "storage" $ do
  it "reads back what it was given" $ emptied $ do
    Storage.setItem LocalStorage "count" (42 :: Int)
    stored <- Storage.getItem LocalStorage "count"
    expect "count" (Just (Right 42 :: Either Text Int)) stored

  it "stores a type that only says it is JSON" $ emptied $ do
    let settings = Settings {theme = "dark", fontSize = 14}
    Storage.setItem LocalStorage "settings" settings
    stored <- Storage.getItem LocalStorage "settings"
    expect "settings" (Just (Right settings)) stored

  it "keeps one entry per key, under a prefix, as base64" $ emptied $ do
    Storage.setItem LocalStorage "greeting" ("hi" :: Text)
    raw <- readStorageItem LocalStorage "haskell-halogen:greeting"
    expect "entry" (Just "aGk=") raw

  it "finds nothing under a key nothing was put under" $ emptied $ do
    stored <- Storage.getItem LocalStorage "absent"
    expect "absent" (Nothing :: Maybe (Either Text Int)) stored

  it "says why a value cannot be read as what was asked for" $ emptied $ do
    Storage.setItem LocalStorage "count" ("not a number" :: Text)
    stored <- Storage.getItem @Int LocalStorage "count"
    expect "unreadable" True $ case stored of
      Just (Left _) -> True
      _ -> False

  it "keeps the two stores apart" $ emptied $ do
    Storage.setItem LocalStorage "where" ("local" :: Text)
    Storage.setItem SessionStorage "where" ("session" :: Text)
    local <- Storage.getItem LocalStorage "where"
    session <- Storage.getItem SessionStorage "where"
    expect "local" (Just (Right ("local" :: Text))) local
    expect "session" (Just (Right ("session" :: Text))) session

  it "forgets a key, and everything" $ emptied $ do
    Storage.setItem LocalStorage "a" ("1" :: Text)
    Storage.setItem LocalStorage "b" ("2" :: Text)
    Storage.removeItem LocalStorage "a"
    expect "after removeItem" ["b"] =<< Storage.keys LocalStorage
    Storage.clear LocalStorage
    expect "after clear" [] =<< Storage.keys LocalStorage

  it "leaves what the page shares the store with alone" $ emptied $ do
    writeStorageItem LocalStorage "someone else's" "not ours"
    Storage.setItem LocalStorage "ours" ("1" :: Text)
    expect "keys" ["ours"] =<< Storage.keys LocalStorage
    Storage.clear LocalStorage
    expect "cleared" [] =<< Storage.keys LocalStorage
    expect "theirs" (Just "not ours") =<< readStorageItem LocalStorage "someone else's"
    -- The store is left as it was found, since nothing else empties it.
    removeStorageItem LocalStorage "someone else's"

  it "says why a value it cannot decode cannot be read" $ emptied $ do
    writeStorageItem LocalStorage "haskell-halogen:bad" "not base64!"
    stored <- Storage.getItem @Text LocalStorage "bad"
    expect "undecodable" True $ case stored of
      Just (Left _) -> True
      _ -> False

  it "leaves a value it cannot decode out of the whole store" $ emptied $ do
    Storage.setItem LocalStorage "good" ("hi" :: Text)
    writeStorageItem LocalStorage "haskell-halogen:bad" "not base64!"
    object <- Storage.readStorageObject LocalStorage
    expect "only the good one" ["good"] (M.keys object)

  it "writes one key without reading the others" $ emptied $ do
    Storage.setItem LocalStorage "a" ("1" :: Text)
    -- What another tab wrote after this one last looked at the store.
    writeStorageItem LocalStorage "haskell-halogen:b" "Mg=="
    Storage.setItem LocalStorage "a" ("3" :: Text)
    expect "both" ["a", "b"] =<< Storage.keys LocalStorage
    expect "theirs" (Just (Right ("2" :: Text))) =<< Storage.getItem LocalStorage "b"

#endif
