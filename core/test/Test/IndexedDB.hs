{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The IndexedDB store's in-memory instance, which is what a native program
-- (a server rendering a page, a test) gets, and which has to behave as the
-- browser's does: the same upgrades, the same failures, the same
-- all-or-nothing batches. The browser's own is tested in Chromium, in
-- hspec-halogen's suite.
--
-- 'MemDOM' runs on every backend, so this runs on all three. The one test
-- of 'BrowserDOM' here, which has no IndexedDB natively, runs natively only:
-- on wasm this suite is a command module under Node, where a call that waits
-- for a promise cannot, and the suite would end there without a word.
module Test.IndexedDB (spec) where

import Control.Exception (try)
import Control.Monad.Reader (runReaderT)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
import Halogen.VDom.DOM.Monad (runBrowserDOM)
#endif
import Prelude
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Web.Storage.IndexedDB (Database (..), IndexedDBError (..), IndexedDBFailure (..), StoreName)
import Web.Storage.IndexedDB qualified as IDB

-- | A database of two stores, under a name of each test's own: every 'MemDOM'
-- in the process shares the databases, as tabs share an origin's.
database :: Text -> Database
database name = Database {name = name, version = 1, stores = ["pages", "attachments"]}

-- | Run a test against a database that starts empty.
fresh :: Database -> MemDOM a -> IO a
fresh db act = runMemDOM (IDB.deleteDatabase db.name >> act)

-- | What a call fails with, if it does.
failureOf :: MemDOM a -> IO (Maybe IndexedDBFailure)
failureOf act = either (\(IndexedDBError _ failure) -> Just failure) (const Nothing) <$> try (runMemDOM act)

pages, attachments :: StoreName
pages = "pages"
attachments = "attachments"

spec :: Spec
spec = describe "IndexedDB in memory" $ do
  it "reads back what it was given, and nothing where nothing was put" $ do
    let db = database "read-back"
    fresh db $ IDB.put db pages "home" "# Home"
    runMemDOM (IDB.get db pages "home") `shouldReturn` Just "# Home"
    runMemDOM (IDB.get db pages "absent") `shouldReturn` Nothing

  it "keeps bytes that are not text as they are" $ do
    let db = database "bytes"
        bytes = BS.pack [0 .. 255] <> BS.pack [0xff, 0xfe, 0x00, 0xc3]
    fresh db $ IDB.put db attachments "logo.png" bytes
    runMemDOM (IDB.get db attachments "logo.png") `shouldReturn` Just bytes

  it "lists keys in order, reads a store whole, and forgets" $ do
    let db = database "keys"
    fresh db $ do
      IDB.put db pages "b" "2"
      IDB.put db pages "a" "1"
      IDB.put db pages "c" "3"
    runMemDOM (IDB.keys db pages) `shouldReturn` ["a", "b", "c"]
    runMemDOM (IDB.getAll db pages) `shouldReturn` M.fromList [("a", "1"), ("b", "2"), ("c", "3")]
    runMemDOM (IDB.delete db pages "b" >> IDB.keys db pages) `shouldReturn` ["a", "c"]
    runMemDOM (IDB.clear db pages >> IDB.keys db pages) `shouldReturn` []

  it "keeps the stores of a database, and databases, apart" $ do
    let one = database "apart-one"
        two = database "apart-two"
    fresh one $ IDB.put one pages "k" "one's page"
    fresh two $ IDB.put two pages "k" "two's page"
    runMemDOM (IDB.put one attachments "k" "one's attachment")
    runMemDOM (IDB.get one pages "k") `shouldReturn` Just "one's page"
    runMemDOM (IDB.get one attachments "k") `shouldReturn` Just "one's attachment"
    runMemDOM (IDB.get two pages "k") `shouldReturn` Just "two's page"

  it "reads and writes values through StorageSerialize" $ do
    let db = database "typed"
    fresh db $ do
      IDB.setItem db pages "count" (42 :: Int)
      IDB.setItem db pages "title" ("Home" :: Text)
    runMemDOM (IDB.getItem db pages "count") `shouldReturn` Just (Right (42 :: Int))
    runMemDOM (IDB.getItem db pages "title") `shouldReturn` Just (Right ("Home" :: Text))
    stored <- runMemDOM (IDB.getItem @Int db pages "title")
    either (const True) (const False) <$> stored `shouldBe` Just True
    everything <- runMemDOM (IDB.getAllItems @Int db pages)
    M.lookup "count" everything `shouldBe` Just (Right 42)

  it "writes many keys to many stores, in order" $ do
    let db = database "many"
    fresh db $ IDB.put db pages "old" "gone soon"
    runMemDOM . IDB.writeMany db $
      [ IDB.itemWrite pages "home" ("first" :: Text)
      , IDB.itemWrite attachments "home/logo.png" (BS.pack [1, 2, 3])
      , IDB.itemDeletion pages "old"
      , IDB.itemWrite pages "home" ("second" :: Text)
      ]
    runMemDOM (IDB.getAll db pages) `shouldReturn` M.fromList [("home", "second")]
    runMemDOM (IDB.getAll db attachments) `shouldReturn` M.fromList [("home/logo.png", BS.pack [1, 2, 3])]

  it "writes all of a batch or none of it" $ do
    let db = database "all-or-nothing"
    fresh db $ IDB.put db pages "home" "before"
    failed <-
      failureOf . IDB.writeMany db $
        [ (pages, "home", Just "after")
        , (attachments, "logo.png", Just "bytes")
        , ("nowhere", "k", Just "v")
        ]
    failed `shouldBe` Just (StoreNotFound "nowhere")
    runMemDOM (IDB.getAll db pages) `shouldReturn` M.fromList [("home", "before")]
    runMemDOM (IDB.keys db attachments) `shouldReturn` []

  it "makes a store only with a version bump, keeping what the others hold" $ do
    let db = database "upgrade"
        sameVersion = db {stores = db.stores <> ["comments"]}
        bumped = sameVersion {version = 2}
    fresh db $ IDB.put db pages "home" "kept"
    failureOf (IDB.put sameVersion "comments" "c1" "first!") `shouldReturn` Just (StoreNotFound "comments")
    runMemDOM (IDB.put bumped "comments" "c1" "first!")
    runMemDOM (IDB.get bumped "comments" "c1") `shouldReturn` Just "first!"
    runMemDOM (IDB.get bumped pages "home") `shouldReturn` Just "kept"

  it "refuses a version lower than the database's" $ do
    let db = database "too-old"
    fresh db $ IDB.put db {version = 3} pages "home" "from version 3"
    failureOf (IDB.get db pages "home") `shouldReturn` Just VersionTooLow
    runMemDOM (IDB.get db {version = 3} pages "home") `shouldReturn` Just "from version 3"

  it "refuses a version that is not positive" $ do
    let db = database "version-zero"
    outcome <- failureOf (IDB.keys db {version = 0} pages)
    outcome `shouldBe` Just (OtherFailure "TypeError" "a version is a positive number")

  it "deletes a database, which is then made afresh" $ do
    let db = database "deleted"
    fresh db $ IDB.put db {version = 2} pages "home" "gone"
    runMemDOM (IDB.deleteDatabase db.name)
    runMemDOM (IDB.get db pages "home") `shouldReturn` Nothing

  it "keeps what one program wrote for the next" $ do
    let db = database "persists"
    fresh db $ IDB.put db pages "home" "written by the first"
    runMemDOM (IDB.get db pages "home") `shouldReturn` Just "written by the first"

  it "is lifted through ReaderT" $ do
    let db = database "reader"
    fresh db $ runReaderT (IDB.put db pages "home" "through ReaderT") ()
    runMemDOM (runReaderT (IDB.get db pages "home") ()) `shouldReturn` Just "through ReaderT"

  noIndexedDB

-- | 'BrowserDOM' natively, where there is no IndexedDB, says so.
noIndexedDB :: Spec
#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
noIndexedDB = it "says so when there is no IndexedDB" $ do
  outcome <- try (runBrowserDOM (IDB.keys (database "nowhere") pages))
  either (\(IndexedDBError _ failure) -> Just failure) (const Nothing) outcome `shouldBe` Just Unavailable
#else
noIndexedDB = pure ()
#endif
