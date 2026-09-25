{-# LANGUAGE CPP #-}
{-# LANGUAGE QualifiedDo #-}

-- | The storage hooks, across the DOM backends.
--
-- Runs on the in-memory DOM natively, and through the browser FFI on JS and
-- wasm using the Web Storage globals supplied by the test scripts.
module Test.Storage (spec) where

import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Hooks qualified as Hooks
import Halogen.Hooks.Extra.Hooks (useLocalStorage)
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (StorageKind (..))
import Protolude
#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
#else
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
#endif
import Test.Harness (dispose, eventually, lastRender, start)
import Test.Hspec (Spec, describe, it, shouldBe)
import Web.Storage.Storage qualified as Storage

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
type TestDOM = BrowserDOM
runTestDOM :: TestDOM a -> IO a
runTestDOM = runBrowserDOM
#else
type TestDOM = MemDOM
runTestDOM :: TestDOM a -> IO a
runTestDOM = runMemDOM
#endif

-- | Counts, and keeps the count where a reload would find it.
--
-- Its monad is the tested DOM backend rather than 'IO', so every persistence
-- operation goes through that backend's storage methods.
persistentComponent :: HS.Emitter IO () -> H.Component H.VoidF () Void TestDOM
persistentComponent events = Hooks.component @Empty $ \_input -> Hooks.do
  (count, setCount) <- useLocalStorage "test/count" (0 :: Int)

  Hooks.useLifecycleEffect $ do
    void $ Hooks.subscribe $ map (\() -> setCount (map (+ 1))) events
    pure Nothing

  Hooks.pure $ HH.text ("count=" <> either ("unreadable: " <>) show count)

-- | Counts under a key it can be told to change, which is what a component
-- whose input starts naming somebody else looks like from inside the hook.
switchingComponent :: HS.Emitter IO (Either Text ()) -> H.Component H.VoidF () Void TestDOM
switchingComponent events = Hooks.component @Empty $ \_input -> Hooks.do
  (key, keyId) <- Hooks.useState "test/a"
  (count, setCount) <- useLocalStorage key (0 :: Int)

  Hooks.useLifecycleEffect $ do
    void $ Hooks.subscribe $ map (either (Hooks.put keyId) (const $ setCount (map (+ 1)))) events
    pure Nothing

  Hooks.pure $ HH.text (key <> "=" <> either ("unreadable: " <>) show count)

-- | 'shouldBe', where these tests run.
expect :: (MonadIO m, Eq a, Show a) => a -> a -> m ()
expect expected actual = liftIO (actual `shouldBe` expected)

spec :: Spec
spec = describe "hooks-extra storage" $ do
  it "keeps state where a remount finds it" $ runTestDOM $ do
    Storage.clear LocalStorage
    source <- liftIO HS.create

    firstMount <- start (persistentComponent source.emitter) ()
    expect "count=0" =<< lastRender firstMount
    liftIO $ HS.notify source.listener ()
    liftIO $ HS.notify source.listener ()
    expect "count=2" =<< lastRender firstMount

    -- What the component keeps is what the store holds, readable by anything
    -- else that knows the key.
    expect (Just (Right (2 :: Int))) =<< Storage.getItem LocalStorage "test/count"
    dispose firstMount

    -- A second mount starts from the store rather than from the default.
    secondMount <- start (persistentComponent source.emitter) ()
    expect "count=2" =<< lastRender secondMount
    dispose secondMount

  it "reads the key it is given now, and leaves the one before it alone" $ runTestDOM $ do
    Storage.clear LocalStorage
    Storage.setItem LocalStorage "test/b" (7 :: Int)
    source <- liftIO HS.create

    harness <- start (switchingComponent source.emitter) ()
    expect "test/a=0" =<< lastRender harness
    liftIO $ HS.notify source.listener (Right ())
    expect "test/a=1" =<< lastRender harness

    -- Told to keep a different key, it shows what that key holds rather than
    -- carrying the first key's count over to it.
    liftIO $ HS.notify source.listener (Left "test/b")
    eventually "test/b=7" (lastRender harness)

    -- And what it writes from here goes there, while the first key is left
    -- where it was.
    liftIO $ HS.notify source.listener (Right ())
    eventually "test/b=8" (lastRender harness)
    expect (Just (Right (1 :: Int))) =<< Storage.getItem LocalStorage "test/a"
    dispose harness

  it "starts from the default when the store holds something it cannot read" $ runTestDOM $ do
    Storage.clear LocalStorage
    Storage.setItem LocalStorage "test/count" ("not a number" :: Text)
    source <- liftIO HS.create
    harness <- start (persistentComponent source.emitter) ()
    -- The reason comes from whatever could not read it, so only the shape of
    -- the answer is this test's business.
    rendered <- lastRender harness
    expect True ("count=unreadable: " `T.isPrefixOf` rendered)
    dispose harness
