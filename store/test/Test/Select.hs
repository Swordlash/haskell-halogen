-- | Selectors: a subscriber hears of the part it selected only when that
-- part changed, and of the first update it is there for whatever it holds.
module Test.Select (spec) where

import Data.IORef
import Halogen.Store qualified as Store
import Halogen.Subscription qualified as HS
import Protolude
import Test.Fixture
import Test.Hspec (Spec, describe, it, shouldBe)

-- | What a subscriber to the selector heard while these updates were made.
heard :: forall a. Store.Selector Store a -> [StoreAction] -> IO [a]
heard selector actions = do
  store <- Store.newHalogenStore initialStore reducer
  seen <- newIORef []
  emitter <- runIn store (Store.emitSelected selector)
  _ <- HS.subscribe emitter $ \a -> modifyIORef' seen (<> [a])
  runIn store (traverse_ Store.updateStore actions)
  readIORef seen

spec :: Spec
spec = describe "Halogen.Store.Select" $ do
  it "selectEq delivers the first update, then only the ones that change the part" $
    heard (Store.selectEq (.count)) [Rename "Ada", Increment, Rename "Grace", Touch, Increment]
      >>= (`shouldBe` [0, 1, 2])

  it "selectEq on another part hears of that part alone" $
    heard (Store.selectEq (.name)) [Increment, Rename "Ada", Increment, Rename "Ada", Rename "Grace"]
      >>= (`shouldBe` ["nobody", "Ada", "Grace"])

  it "select compares with the function it is given" $
    heard (Store.select (\a b -> a `div` 2 == b `div` 2) (.count)) [Increment, Increment, Increment, Increment, Increment]
      >>= (`shouldBe` [1, 2, 4])

  it "selectAll delivers every update, even one that changes nothing" $
    heard Store.selectAll [Touch, Touch, Increment]
      >>= (`shouldBe` [initialStore, initialStore, Store {count = 1, name = "nobody"}])

  it "compares each subscriber with what it was told itself" $ do
    store <- Store.newHalogenStore initialStore reducer
    early <- newIORef []
    late <- newIORef []
    emitter <- runIn store (Store.emitSelected (Store.selectEq (.count)))
    _ <- HS.subscribe emitter $ \n -> modifyIORef' early (<> [n])
    runIn store (Store.updateStore Increment)
    _ <- HS.subscribe emitter $ \n -> modifyIORef' late (<> [n])
    runIn store (traverse_ Store.updateStore [Rename "Ada", Increment])
    readIORef early >>= (`shouldBe` [1, 2])
    readIORef late >>= (`shouldBe` [1, 2])
