-- | core's IndexedDB store, in the browser it is for: what is written is
-- read back, bytes as bytes, a batch is kept whole or not at all, versions
-- upgrade, and a component finds what an earlier one kept.
module Test.IndexedDB (spec) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Example.Notebook qualified as Notebook
import Protolude hiding (find)
import Test.Hspec.Halogen
import UnliftIO.Async (forConcurrently_)
import Web.Storage.IndexedDB (Database (..), IndexedDBError (..), IndexedDBFailure (..), MonadIndexedDB, StoreName)
import Web.Storage.IndexedDB qualified as IDB

spec :: forall m -> (MonadBrowserTest m, MonadIndexedDB m) => Spec
spec m = do
  it "reads back what it wrote, and nothing where nothing was written" $ runPage $ do
    let db = database "read-back"
    inPage m (fresh db >> IDB.put db pages "home" "# Home")
    inPage m (IDB.get db pages "home") `shouldReturn` Just "# Home"
    inPage m (IDB.get db pages "absent") `shouldReturn` Nothing

  it "keeps bytes that are not text as they are" $ runPage $ do
    let db = database "bytes"
        bytes = BS.pack [0 .. 255] <> BS.pack [0xff, 0xfe, 0x00, 0xc3]
        large = BS.concat (replicate 8192 (BS.pack [0 .. 255]))
    inPage m $ do
      fresh db
      IDB.put db attachments "odd.bin" bytes
      IDB.put db attachments "large.bin" large
    inPage m (IDB.get db attachments "odd.bin") `shouldReturn` Just bytes
    inPage m (IDB.get db attachments "large.bin") `shouldReturn` Just large
    inPage m (IDB.put db attachments "empty.bin" "" >> IDB.get db attachments "empty.bin") `shouldReturn` Just ""

  it "lists keys in order, reads a store whole, and forgets" $ runPage $ do
    let db = database "keys"
    inPage m $ do
      fresh db
      IDB.put db pages "b" "2"
      IDB.put db pages "a" "1"
      IDB.put db pages "c" "3"
    inPage m (IDB.keys db pages) `shouldReturn` ["a", "b", "c"]
    inPage m (IDB.getAll db pages) `shouldReturn` M.fromList [("a", "1"), ("b", "2"), ("c", "3")]
    inPage m (IDB.delete db pages "b" >> IDB.keys db pages) `shouldReturn` ["a", "c"]
    inPage m (IDB.clear db pages >> IDB.keys db pages) `shouldReturn` []

  -- The order 'MemDOM' reproduces: a character beyond U+FFFF is a surrogate
  -- pair, whose code units come before U+E000.
  it "orders keys by UTF-16 code units" $ runPage $ do
    let db = database "utf16"
    inPage m $ do
      fresh db
      IDB.put db pages "\xE000" "private use"
      IDB.put db pages "\x10000" "beyond the BMP"
      IDB.put db pages "a" "ascii"
    inPage m (IDB.keys db pages) `shouldReturn` ["a", "\x10000", "\xE000"]

  it "reads and writes values through StorageSerialize" $ runPage $ do
    let db = database "typed"
    inPage m $ do
      fresh db
      IDB.setItem db pages "count" (42 :: Int)
      IDB.setItem db pages "title" ("Home" :: Text)
    inPage m (IDB.getItem db pages "count") `shouldReturn` Just (Right (42 :: Int))
    inPage m (IDB.getItem db pages "title") `shouldReturn` Just (Right ("Home" :: Text))
    notAnInt <- inPage m (IDB.getItem db pages "title")
    (isLeft <$> (notAnInt :: Maybe (Either Text Int))) `shouldBe` Just True

  it "writes many keys to many stores, in order" $ runPage $ do
    let db = database "many"
    inPage m $ do
      fresh db
      IDB.put db pages "old" "gone soon"
      IDB.writeMany
        db
        [ IDB.itemWrite pages "home" ("first" :: Text)
        , IDB.itemWrite attachments "home/logo.png" (BS.pack [1, 2, 3])
        , IDB.itemDeletion pages "old"
        , IDB.itemWrite pages "home" ("second" :: Text)
        ]
    inPage m (IDB.getAll db pages) `shouldReturn` M.fromList [("home", "second")]
    inPage m (IDB.getAll db attachments) `shouldReturn` M.fromList [("home/logo.png", BS.pack [1, 2, 3])]

  it "writes all of a batch or none of it" $ runPage $ do
    let db = database "all-or-nothing"
    inPage m (fresh db >> IDB.put db pages "home" "before")
    failureOf m (IDB.writeMany db [(pages, "home", Just "after"), (attachments, "logo.png", Just "bytes"), ("nowhere", "k", Just "v")])
      `shouldReturn` Just (StoreNotFound "nowhere")
    inPage m (IDB.getAll db pages) `shouldReturn` M.fromList [("home", "before")]
    inPage m (IDB.keys db attachments) `shouldReturn` []

  it "makes a store only with a version bump, and refuses an older version" $ runPage $ do
    let db = database "upgrade"
        sameVersion = db {stores = db.stores <> ["comments"]}
        bumped = sameVersion {version = 2}
    inPage m (fresh db >> IDB.put db pages "home" "kept")
    failureOf m (IDB.put sameVersion "comments" "c1" "first!") `shouldReturn` Just (StoreNotFound "comments")
    inPage m (IDB.put bumped "comments" "c1" "first!")
    inPage m (IDB.get bumped "comments" "c1") `shouldReturn` Just "first!"
    inPage m (IDB.get bumped pages "home") `shouldReturn` Just "kept"
    failureOf m (IDB.get db pages "home") `shouldReturn` Just VersionTooLow
    inPage m (IDB.get bumped pages "home") `shouldReturn` Just "kept"

  it "serves calls made at once from many threads" $ runPage $ do
    let db = database "concurrent"
        names = map show [1 .. 20 :: Int]
    inPage m (fresh db)
    inPage m (forConcurrently_ names (\key -> IDB.put db pages key (encodeUtf8 key)))
    inPage m (IDB.keys db pages) `shouldReturn` sort names

  it "gives a component what an earlier one kept" $ runPage $ do
    let db = Database {name = "hspec-halogen-notebook", version = 1, stores = [Notebook.notes]}
    inPage m (IDB.deleteDatabase db.name)
    keeper <- mount m Notebook.component (Notebook.Input {database = db, note = "remember this"})
    find keeper ".kept" >>= (`shouldHaveText` "nothing yet")
    find keeper "button.keep" >>= click
    find keeper ".kept" >>= (`shouldHaveText` "remember this")
    unmount keeper
    finder <- mount m Notebook.component (Notebook.Input {database = db, note = "something else"})
    find finder ".kept" >>= (`shouldHaveText` "remember this")

-- | A database of two stores, under a name of each test's own.
database :: Text -> Database
database name = Database {name = "hspec-halogen-" <> name, version = 1, stores = [pages, attachments]}

pages, attachments :: StoreName
pages = "pages"
attachments = "attachments"

-- | Start from an empty database: one a run before left behind is deleted.
fresh :: (MonadIndexedDB n) => Database -> n ()
fresh db = IDB.deleteDatabase db.name

-- | Run the component monad from a test.
inPage :: forall n -> (MonadBrowserTest n) => n a -> PageM s a
inPage _ act = unsafeIOToPageM (runTest act)

-- | What a call fails with, if it does.
failureOf :: forall n -> (MonadBrowserTest n) => n a -> PageM s (Maybe IndexedDBFailure)
failureOf _ act = unsafeIOToPageM $ either (\(IndexedDBError _ failure) -> Just failure) (const Nothing) <$> try (runTest act)
