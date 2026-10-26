-- | A place to run a component where there is no browser, the same on every
-- backend.
--
-- The driver is given a 'AD.RenderSpec' that does not touch a DOM at all: it
-- records the text of every render, each component's separately, and walks
-- the slots so that child components still mount. That is enough to watch a
-- tree from the outside: what each component rendered, how often, and what
-- the root raised.
module Test.Harness
  ( Harness (..)
  , start
  , query
  , dispose
  , renders
  , rendersOf
  , eventually
  )
where

import Data.IORef
import Data.Row (Row)
import Data.Text qualified as T
import Halogen (Component, HalogenSocket (..))
import Halogen.Component (ComponentSlot (..), ComponentSlotBox)
import Halogen.HTML.Core qualified as HC
import Halogen.IO.Driver qualified as AD
import Halogen.IO.Driver.State (RenderStateX (..))
import Halogen.Query.Input (Input)
import Halogen.Subscription qualified as HS
import Halogen.VDom.Types (VDom (..), runGraft)
import Protolude
import Test.Hspec (shouldBe)
import UnliftIO (MonadUnliftIO)

-- | Nothing is kept per render: what the tests look at is the log of texts.
data TestRenderState (s :: Type) (act :: Type) (ps :: Row Type) (o :: Type)
  = TestRenderState

data Harness q o m = Harness
  { socket :: HalogenSocket q o m
  , texts :: IORef [Text]
  , outputs :: IORef [o]
  }

-- | Mount a component and start watching it.
start :: forall q i o m. (MonadUnliftIO m) => Component q i o m -> i -> m (Harness q o m)
start c i = do
  texts <- liftIO $ newIORef []
  outputs <- liftIO $ newIORef []
  socket <- AD.runUI (renderSpec texts) c i
  void $ HS.subscribe socket.messages $ \o -> liftIO $ modifyIORef' outputs (<> [o])
  pure Harness {socket, texts, outputs}

-- | Send a query to the root, as a parent would.
query :: forall q o m a. Harness q o m -> q a -> m (Maybe a)
query h q = let HalogenSocket {query = send} = h.socket in send q

dispose :: forall q o m. Harness q o m -> m ()
dispose h = h.socket.dispose

-- | Every render so far, in order: one text per render of a component.
renders :: forall q o m. Harness q o m -> IO [Text]
renders h = readIORef h.texts

-- | The renders whose text starts with this, which is how a test tells its
-- components apart.
rendersOf :: forall q o m. Text -> Harness q o m -> IO [Text]
rendersOf prefix h = filter (T.isPrefixOf prefix) <$> renders h

-- | Wait for something to become true, then assert it. A store updated from
-- the test's own thread is heard by the tree when the tree is free, which
-- need not be before the update returns.
eventually :: forall a. (Eq a, Show a) => a -> IO a -> IO ()
eventually expected act = go (2000 :: Int)
  where
    go n = do
      actual <- act
      if actual == expected || n == 0
        then actual `shouldBe` expected
        else threadDelay 1000 *> go (n - 1)

renderSpec :: forall m. (MonadIO m) => IORef [Text] -> AD.RenderSpec m TestRenderState
renderSpec texts =
  AD.RenderSpec
    { AD.render = renderHtml texts
    , AD.renderChild = identity
    , AD.removeChild = \_ -> pure ()
    , AD.dispose = \_ -> pure ()
    , AD.sameElement = \_ _ -> pure False
    , AD.rootNode = \_ -> pure Nothing
    }

renderHtml
  :: forall s act ps o m
   . (MonadIO m)
  => IORef [Text]
  -> (Input act -> m ())
  -> (ComponentSlotBox ps m act -> m (RenderStateX TestRenderState))
  -> HC.HTML (ComponentSlot ps m act) act
  -> Maybe (TestRenderState s act ps o)
  -> m (TestRenderState s act ps o)
renderHtml texts _handler renderChild html _prev = do
  rendered <- liftIO $ evaluate (textOf (HC.unHTML html))
  liftIO $ modifyIORef' texts (<> [rendered])
  traverse_ renderSlot (slotsOf (HC.unHTML html))
  pure TestRenderState
  where
    renderSlot :: ComponentSlot ps m act -> m ()
    renderSlot = \case
      ComponentSlot box -> void (renderChild box)
      ThunkSlot _ -> panic "Test.Harness: thunk slots are unsupported"

-- | Everything the tree says, in document order. Child components render
-- separately, so their text is not part of their parent's.
textOf :: forall p w. VDom p w -> Text
textOf = \case
  Text t -> t
  Elem _ _ _ cs -> foldMap textOf cs
  Keyed _ _ _ cs -> foldMap (textOf . snd) cs
  Widget _ -> ""
  Grafted g -> textOf (runGraft g)

slotsOf :: forall p w. VDom p w -> [w]
slotsOf = \case
  Text _ -> []
  Elem _ _ _ cs -> concatMap slotsOf cs
  Keyed _ _ _ cs -> concatMap (slotsOf . snd) cs
  Widget w -> [w]
  Grafted g -> slotsOf (runGraft g)
