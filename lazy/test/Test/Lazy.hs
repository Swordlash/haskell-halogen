-- | The wrapper and 'Lazy.memo', with loaders the tests hold back, let go
-- and make fail, in a tree rendered by the driver with a render spec that
-- records each component's text (see "Test.Harness").
module Test.Lazy
  ( spec
  , InnerQuery (..)
  , inner
  , loading
  )
where

import Data.IORef
import Data.Row (Empty, type (.==))
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Lazy qualified as Lazy
import Protolude
import Test.Harness
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

data InnerQuery a
  = Bump a
  | GetCount (Int -> a)

-- | Shows the name its input gives it and how many times it was bumped, and
-- raises the new count when it is.
inner :: forall m. (MonadIO m) => H.Component InnerQuery Text Int m
inner = H.component (\name -> pure (name, 0 :: Int)) render [H.onReceive receive, H.onQuery handleQuery]
  where
    render :: (Text, Int) -> H.ComponentHTML () Empty m
    render (name, count) = HH.text ("inner: " <> name <> " " <> show count)
    receive name = modify (\(_, count) -> (name, count))
    handleQuery :: InnerQuery a -> H.HalogenM (Text, Int) () Empty Int m (Maybe a)
    handleQuery = \case
      Bump next -> do
        (name, count) <- get
        put (name, count + 1)
        H.raise (count + 1)
        pure (Just next)
      GetCount reply -> Just . reply . snd <$> get

loading :: Lazy.Loading
loading = Lazy.Loading {placeholder = HH.text "placeholder", failed = \message -> HH.text ("failed: " <> message)}

data ParentQuery a
  = Rename Text a
  | BumpInner a
  | AskInner (Maybe Int -> a)

newtype Heard = Heard Int

type ParentSlots = ("lazy" .== H.Slot InnerQuery Int ())

-- | Renders the wrapper over the loader, with its name as the input, raises
-- what it hears from it, and sends it queries when it is asked to.
parent :: forall m. (MonadIO m) => m (H.Component InnerQuery Text Int m) -> H.Component ParentQuery Text Int m
parent loader = H.component pure render [H.onQuery handleQuery, H.onAction (\(Heard count) -> H.raise count)]
  where
    render :: Text -> H.ComponentHTML Heard ParentSlots m
    render name = HH.slot "lazy" () (Lazy.lazy loading loader) name Heard
    handleQuery :: ParentQuery a -> H.HalogenM Text Heard ParentSlots Int m (Maybe a)
    handleQuery = \case
      Rename name next -> put name $> Just next
      BumpInner next -> H.tell "lazy" () Bump $> Just next
      AskInner reply -> Just . reply <$> H.request "lazy" () GetCount

-- | What the failing loaders throw.
newtype Unreachable = Unreachable Text
  deriving stock (Show)

instance Exception Unreachable where
  displayException (Unreachable why) = toS why

-- | A loader that waits for the test to let it go.
held :: IO (MVar (), IO (H.Component InnerQuery Text Int IO))
held = do
  gate <- newEmptyMVar
  pure (gate, readMVar gate $> inner)

lastInner :: Harness q o m -> IO (Maybe Text)
lastInner h = lastMay <$> rendersOf "inner" h

spec :: Spec
spec = describe "Halogen.Lazy" $ do
  it "shows the placeholder until the loader is done, then the component with the latest input" $ do
    (gate, loader) <- held
    h <- start (parent loader) "a"
    eventually True (not . null <$> rendersOf "placeholder" h)
    -- A new input while it loads is the one the component starts with.
    query h (Rename "b" ()) `shouldReturn` Just ()
    rendersOf "inner" h `shouldReturn` []
    putMVar gate ()
    eventually (Just "inner: b 0") (lastInner h)
    -- And it hears every input after that.
    query h (Rename "c" ()) `shouldReturn` Just ()
    eventually (Just "inner: c 0") (lastInner h)
    dispose h

  it "raises the component's output and sends it the queries, which find nothing before it is there" $ do
    (gate, loader) <- held
    h <- start (parent loader) "a"
    query h (AskInner identity) `shouldReturn` Just Nothing
    query h (BumpInner ()) `shouldReturn` Just ()
    putMVar gate ()
    eventually (Just "inner: a 0") (lastInner h)
    query h (BumpInner ()) `shouldReturn` Just ()
    eventually (Just "inner: a 1") (lastInner h)
    readIORef h.outputs `shouldReturn` [1]
    query h (AskInner identity) `shouldReturn` Just (Just 1)
    dispose h

  it "shows what a failing loader threw, and answers queries Nothing" $ do
    h <- start (parent (throwIO (Unreachable "no such library"))) "a"
    eventually ["failed: no such library"] (rendersOf "failed" h)
    query h (AskInner identity) `shouldReturn` Just Nothing
    rendersOf "inner" h `shouldReturn` []
    dispose h

  it "shows the component at once when the loader has it already" $ do
    h <- start (parent (pure inner)) "a"
    eventually (Just "inner: a 0") (lastInner h)
    dispose h

  describe "memo" $ do
    it "loads once for every mount" $ do
      loads <- newIORef (0 :: Int)
      shared <- Lazy.memo (atomicModifyIORef' loads (\n -> (n + 1, ())) $> inner)
      h1 <- start (parent shared) "a"
      eventually (Just "inner: a 0") (lastInner h1)
      h2 <- start (parent shared) "b"
      eventually (Just "inner: b 0") (lastInner h2)
      readIORef loads `shouldReturn` 1
      dispose h1
      dispose h2

    it "makes a use that comes while it loads wait for that load" $ do
      loads <- newIORef (0 :: Int)
      gate <- newEmptyMVar
      shared <- Lazy.memo (atomicModifyIORef' loads (\n -> (n + 1, n + 1)) <* readMVar gate)
      (a, b) <- concurrently shared (threadDelay 10000 *> putMVar gate () *> shared)
      (a, b) `shouldBe` (1, 1)
      readIORef loads `shouldReturn` 1

    it "forgets a load that failed, for the next mount to load again" $ do
      loads <- newIORef (0 :: Int)
      shared <- Lazy.memo $ do
        n <- atomicModifyIORef' loads (\n -> (n + 1, n + 1))
        when (n == 1) $ throwIO (Unreachable "the network is down")
        pure inner
      h1 <- start (parent shared) "a"
      eventually ["failed: the network is down"] (rendersOf "failed" h1)
      h2 <- start (parent shared) "b"
      eventually (Just "inner: b 0") (lastInner h2)
      h3 <- start (parent shared) "c"
      eventually (Just "inner: c 0") (lastInner h3)
      readIORef loads `shouldReturn` 2
      traverse_ dispose [h1, h2, h3]
