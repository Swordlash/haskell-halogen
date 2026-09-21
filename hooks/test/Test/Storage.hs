{-# LANGUAGE CPP #-}
{-# LANGUAGE QualifiedDo #-}

-- | The storage hooks, against the in-memory DOM.
--
-- What a store is depends on the backend, and the in-memory one has the two
-- stores a browser has — on native, which is where that backend has its
-- instance. The browser backends have real stores and no browser to run these
-- under, so there this says nothing.
module Test.Storage (spec) where

import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "hooks-extra storage" $ pure ()

#else

import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Hooks qualified as Hooks
import Halogen.Hooks.Extra.Hooks (useLocalStorage)
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (MemDOM, StorageKind (..), runMemDOM)
import Test.Harness (dispose, lastRender, start)
import Test.Hspec (Spec, describe, it, shouldBe)
import Web.Storage.Storage qualified as Storage

-- | Counts, and keeps the count where a reload would find it.
--
-- Its monad is 'MemDOM' rather than 'IO': what a store is depends on the
-- backend, and the in-memory one has the two stores the browser has.
persistentComponent :: HS.Emitter IO () -> H.Component H.VoidF () Void MemDOM
persistentComponent events = Hooks.component @Empty $ \_input -> Hooks.do
  (count, setCount) <- useLocalStorage "test/count" (0 :: Int)

  Hooks.useLifecycleEffect $ do
    void $ Hooks.subscribe $ map (\() -> setCount (map (+ 1))) events
    pure Nothing

  Hooks.pure $ HH.text ("count=" <> either ("unreadable: " <>) show count)

-- | 'shouldBe', where these tests run.
expect :: (MonadIO m, Eq a, Show a) => a -> a -> m ()
expect expected actual = liftIO (actual `shouldBe` expected)

spec :: Spec
spec = describe "hooks-extra storage" $ do
  it "keeps state where a remount finds it" $ runMemDOM $ do
    Storage.clear LocalStorage
    source <- liftIO HS.create

    first <- start (persistentComponent source.emitter) ()
    expect "count=0" =<< lastRender first
    liftIO $ HS.notify source.listener ()
    liftIO $ HS.notify source.listener ()
    expect "count=2" =<< lastRender first

    -- What the component keeps is what the store holds, readable by anything
    -- else that knows the key.
    expect (Just (Right (2 :: Int))) =<< Storage.getItem LocalStorage "test/count"
    dispose first

    -- A second mount starts from the store rather than from the default.
    second <- start (persistentComponent source.emitter) ()
    expect "count=2" =<< lastRender second
    dispose second

  it "starts from the default when the store holds something it cannot read" $ runMemDOM $ do
    Storage.clear LocalStorage
    Storage.setItem LocalStorage "test/count" ("not a number" :: Text)
    source <- liftIO HS.create
    harness <- start (persistentComponent source.emitter) ()
    -- The reason comes from whatever could not read it, so only the shape of
    -- the answer is this test's business.
    rendered <- lastRender harness
    expect True ("count=unreadable: " `T.isPrefixOf` rendered)
    dispose harness

#endif
