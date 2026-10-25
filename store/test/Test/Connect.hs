-- | Connected components and subscriptions, run by the driver with a render
-- spec that records each component's text (see "Test.Harness").
module Test.Connect
  ( spec
  , CounterQuery (..)
  , Reported (..)
  , counter
  , connectedCounter
  )
where

import Data.IORef
import Data.NT (type (~>) (..))
import Data.Row (Empty, type (.+), type (.==))
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Store qualified as Store
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (runMemDOM)
import Protolude
import Test.Fixture
import Test.Harness
import Test.Hspec (Spec, describe, it, shouldBe)

data CounterQuery a
  = GetConnected (Store.Connected Int Text -> a)
  | Report a

newtype Reported = Reported Int
  deriving stock (Eq, Show)

-- | Shows the count its connection hands it, after the label its parent
-- passed: @a: 3@. It keeps what it is given and nothing else, so it renders
-- once at first and once for each new input.
counter :: forall m. (MonadIO m) => H.Component CounterQuery (Store.Connected Int Text) Reported m
counter = H.component pure render [H.onReceive put, H.onQuery handleQuery]
  where
    render :: Store.Connected Int Text -> H.ComponentHTML () Empty m
    render connected = HH.text (connected.input <> ": " <> show connected.context)
    handleQuery :: CounterQuery a -> H.HalogenM (Store.Connected Int Text) () Empty Reported m (Maybe a)
    handleQuery = \case
      GetConnected reply -> Just . reply <$> get
      Report next -> do
        connected <- get
        H.raise (Reported connected.context)
        pure (Just next)

connectedCounter :: forall m. (Store.MonadStore StoreAction Store m) => H.Component CounterQuery Text Reported m
connectedCounter = Store.connect (Store.selectEq (.count)) counter

-- | Shows the name, and takes no input of its own.
namer :: forall q o m. (Store.MonadStore StoreAction Store m) => H.Component q () o m
namer = Store.connect (Store.selectEq (.name)) (H.component pure render [H.onReceive put])
  where
    render :: Store.Connected Text () -> H.ComponentHTML () Empty m
    render connected = HH.text ("name: " <> connected.context)

data ParentQuery a
  = Dispatch StoreAction a
  | Relabel Text a

type ParentSlots =
  ( "count" .== H.Slot CounterQuery Reported Int
      .+ "name" .== H.Slot H.VoidF Void ()
  )

-- | A toolbar's worth of updates, as queries, over two counters connected to
-- the count (the first labelled as the parent says, the second @b@) and a
-- component connected to the name.
parent :: forall o m. (Store.MonadStore StoreAction Store m) => H.Component ParentQuery () o m
parent = H.component (\_ -> pure "a") render [H.onQuery handleQuery]
  where
    render :: Text -> H.ComponentHTML () ParentSlots m
    render label =
      HH.div_
        [ HH.slot_ "count" 1 connectedCounter label
        , HH.slot_ "count" 2 connectedCounter "b"
        , HH.slot_ "name" () namer ()
        ]
    handleQuery :: ParentQuery a -> H.HalogenM Text () ParentSlots o m (Maybe a)
    handleQuery = \case
      Dispatch action next -> Store.updateStore action $> Just next
      Relabel label next -> put label $> Just next

data WatchQuery a = Names ([Text] -> a)

newtype Named = Named Text

-- | Subscribed to the name in its initializer: every name it is told of, in
-- order.
watcher :: forall o m. (Store.MonadStore StoreAction Store m) => H.Component WatchQuery () o m
watcher =
  H.component
    (\_ -> pure [])
    (\names -> HH.text ("names: " <> mconcat (intersperse "," names)) :: H.ComponentHTML Named Empty m)
    [ H.onInitialize (Store.subscribe (Store.selectEq (.name)) Named)
    , H.onAction (\(Named n) -> modify (<> [n]))
    , H.onQuery (\(Names reply) -> Just . reply <$> get)
    ]

spec :: Spec
spec = describe "Halogen.Store.Connect" $ do
  it "renders the selected part and the input, again only when that part changes" $ do
    store <- Store.newHalogenStore initialStore reducer
    h <- runMemDOM $ start (H.hoist (NT (runIn store)) connectedCounter) "count"
    -- The connection reads the store as it starts, so the first render has
    -- the counter in it already; the connection's own render is empty.
    renders h >>= (`shouldBe` ["", "count: 0"])
    runIn store (Store.updateStore (Rename "Ada"))
    runIn store (Store.updateStore Touch)
    runIn store (Store.updateStore Increment)
    eventually ["count: 0", "count: 1"] (rendersOf "count:" h)
    -- The connection rendered again for the count and for nothing else.
    renders h >>= (`shouldBe` ["", "count: 0", "", "count: 1"])
    runMemDOM (query h (H.mkRequest GetConnected)) >>= (`shouldBe` Just (Store.Connected {context = 1, input = "count"}))
    runMemDOM (dispose h)

  it "answers the connected component's queries with it, and raises its output" $ do
    store <- Store.newHalogenStore initialStore reducer
    h <- runMemDOM $ start (H.hoist (NT (runIn store)) connectedCounter) "count"
    runIn store (Store.updateStore Increment)
    eventually ["count: 0", "count: 1"] (rendersOf "count:" h)
    runMemDOM (query h (H.mkTell Report)) >>= (`shouldBe` Just ())
    eventually [Reported 1] (readIORef h.outputs)
    runMemDOM (dispose h)

  it "runStoreT gives every component of the tree one store, which updates from the tree reach" $ do
    running <- Store.runAndEmitStoreT initialStore reducer parent
    stores <- newIORef []
    _ <- HS.subscribe running.emitter $ \s -> modifyIORef' stores (<> [s])
    h <- runMemDOM $ start running.component ()
    rendersOf "a:" h >>= (`shouldBe` ["a: 0"])
    rendersOf "b:" h >>= (`shouldBe` ["b: 0"])
    rendersOf "name:" h >>= (`shouldBe` ["name: nobody"])

    _ <- runMemDOM (query h (H.mkTell (Dispatch Increment)))
    -- Both counters hear of the one update; the name's component does not.
    eventually ["a: 0", "a: 1"] (rendersOf "a:" h)
    eventually ["b: 0", "b: 1"] (rendersOf "b:" h)
    _ <- runMemDOM (query h (H.mkTell (Dispatch (Rename "Ada"))))
    eventually ["name: nobody", "name: Ada"] (rendersOf "name:" h)
    rendersOf "a:" h >>= (`shouldBe` ["a: 0", "a: 1"])
    rendersOf "b:" h >>= (`shouldBe` ["b: 0", "b: 1"])

    -- New input from the parent reaches the counter with its count.
    _ <- runMemDOM (query h (H.mkTell (Relabel "c")))
    eventually ["c: 1"] (rendersOf "c:" h)

    readIORef stores >>= (`shouldBe` [Store {count = 1, name = "nobody"}, Store {count = 1, name = "Ada"}])
    runMemDOM (dispose h)

  it "subscribe hands a component the selected part's changes as actions" $ do
    store <- Store.newHalogenStore initialStore reducer
    h <- runMemDOM $ start (H.hoist (NT (runIn store)) watcher) ()
    runIn store (traverse_ Store.updateStore [Rename "Ada", Increment, Rename "Ada", Rename "Grace"])
    eventually (Just ["Ada", "Grace"]) (runMemDOM (query h (H.mkRequest Names)))
    runMemDOM (dispose h)
