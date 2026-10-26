-- | The store itself: updates go through the reducer, and every one reaches
-- the store's subscribers, newest last.
module Test.Monad (spec) where

import Data.IORef
import Halogen.Store qualified as Store
import Halogen.Subscription qualified as HS
import Protolude
import Test.Fixture
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Halogen.Store.Monad" $ do
  it "reduces each update into the store, which getStore reads" $ do
    store <- Store.newHalogenStore initialStore reducer
    before <- runIn store Store.getStore
    after <- runIn store $ do
      Store.updateStore Increment
      Store.updateStore (Rename "Ada")
      Store.updateStore Increment
      Store.getStore
    before `shouldBe` initialStore
    after `shouldBe` Store {count = 2, name = "Ada"}

  it "tells the store's emitter of every update, unchanged ones too" $ do
    store <- Store.newHalogenStore initialStore reducer
    seen <- newIORef []
    _ <- HS.subscribe (Store.storeEmitter store) $ \s -> modifyIORef' seen (<> [s])
    runIn store $ traverse_ Store.updateStore [Increment, Touch, Rename "Ada"]
    readIORef seen
      >>= (`shouldBe` [Store {count = 1, name = "nobody"}, Store {count = 1, name = "nobody"}, Store {count = 1, name = "Ada"}])

  it "tells no more after unsubscribing" $ do
    store <- Store.newHalogenStore initialStore reducer
    seen <- newIORef (0 :: Int)
    subscription <- HS.subscribe (Store.storeEmitter store) $ \_ -> modifyIORef' seen (+ 1)
    runIn store (Store.updateStore Increment)
    HS.unsubscribe subscription
    runIn store (Store.updateStore Increment)
    readIORef seen >>= (`shouldBe` 1)

  -- The first subscriber answers the first update with a second one, inside
  -- the first one's notification. The other subscriber hears of the second
  -- update before the first one's notification reaches it, and must not be
  -- left believing the store holds the first update's value.
  it "does not hand a subscriber an update older than one it was given" $ do
    store <- Store.newHalogenStore initialStore reducer
    answering <- newIORef []
    listening <- newIORef []
    _ <- HS.subscribe (Store.storeEmitter store) $ \s -> do
      modifyIORef' answering (<> [s.count])
      when (s.count == 1) $ runIn store (Store.updateStore Increment)
    _ <- HS.subscribe (Store.storeEmitter store) $ \s -> modifyIORef' listening (<> [s.count])
    runIn store (Store.updateStore Increment)
    readIORef answering >>= (`shouldBe` [1, 2])
    readIORef listening >>= (`shouldBe` [2])
    runIn store Store.getStore >>= (`shouldBe` 2) . (.count)

  -- The first update is still being handed to the subscriber on one thread
  -- when another thread makes the second. The second waits for the first
  -- to be handed on, rather than reaching the subscriber at the same time,
  -- or before the first one ends, which would leave it believing the store
  -- holds the first update's value.
  it "hands a subscriber an update made on another thread after the one under way" $ do
    store <- Store.newHalogenStore initialStore reducer
    seen <- newIORef []
    inFirst <- newEmptyMVar
    goOn <- newEmptyMVar
    finished <- newEmptyMVar
    _ <- HS.subscribe (Store.storeEmitter store) $ \s -> do
      when (s.count == 1) $ putMVar inFirst () >> takeMVar goOn
      modifyIORef' seen (<> [s.count])
    _ <- forkIO $ runIn store (Store.updateStore Increment) >> putMVar finished ()
    takeMVar inFirst
    runIn store (Store.updateStore Increment)
    readIORef seen >>= (`shouldBe` [])
    putMVar goOn ()
    takeMVar finished
    readIORef seen >>= (`shouldBe` [1, 2])

  it "hands a subscriber updates made by many threads at once in order, the newest last" $ do
    store <- Store.newHalogenStore initialStore reducer
    seen <- newIORef []
    done <- newEmptyMVar
    _ <- HS.subscribe (Store.storeEmitter store) $ \s -> atomicModifyIORef' seen (\counts -> (s.count : counts, ()))
    let threads = 8 :: Int
        each = 250 :: Int
    replicateM_ threads $ forkIO $ do
      replicateM_ each (runIn store (Store.updateStore Increment))
      putMVar done ()
    replicateM_ threads (takeMVar done)
    counts <- reverse <$> readIORef seen
    and (zipWith (<) counts (drop 1 counts)) `shouldBe` True
    lastMay counts `shouldBe` Just (threads * each)
