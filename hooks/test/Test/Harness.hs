-- | A place to run a component where there is no browser.
--
-- The driver is given a 'AD.RenderSpec' that does not touch a DOM at all: it
-- records the text of every render and walks the slots so child components
-- still mount. That is enough to watch a hooks component from the outside —
-- what it rendered, in what order, and what it raised.
module Test.Harness
  ( Harness (..)
  , start
  , query
  , dispose
  , lastRender
  , eventually
  )
where

import Control.Monad.Catch (MonadMask)
import Control.Monad.Fork (MonadFork, MonadKill)
import Control.Monad.Parallel (MonadParallel)
import Control.Monad.UUID (MonadUUID)
import Data.IORef
import Data.Row (Row)
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

-- | Nothing is kept per render: what the tests look at is the log.
data TestRenderState (s :: Type) (act :: Type) (ps :: Row Type) (o :: Type)
  = TestRenderState

data Harness q o m = Harness
  { socket :: HalogenSocket q o m
  , renders :: IORef [Text]
  , outputs :: IORef [o]
  }

-- | What the driver asks of a component monad. 'IO' has all of it, and so does
-- 'Halogen.VDom.DOM.Monad.MemDOM', which is the one to run in when a test
-- needs a component monad that is a browser.
type TestMonad m = (MonadIO m, MonadUnliftIO m, MonadFork m, MonadKill m, MonadParallel m, MonadMask m, MonadUUID m)

-- | Mount a component and start watching it.
start :: forall q i o m. (TestMonad m) => Component q i o m -> i -> m (Harness q o m)
start c i = do
  renders <- liftIO $ newIORef []
  outputs <- liftIO $ newIORef []
  socket <- AD.runUI (renderSpec renders) c i
  void $ HS.subscribe socket.messages $ \o -> liftIO $ modifyIORef' outputs (<> [o])
  pure Harness {socket, renders, outputs}

-- | Send a query to the component, as a parent would.
--
-- Not a field of 'Harness': 'HalogenSocket'\'s @query@ is rank-2, which a
-- record selector cannot be.
query :: forall q o m a. Harness q o m -> q a -> m (Maybe a)
query h q = let HalogenSocket {query = send} = h.socket in send q

-- | Finalize the component.
dispose :: forall q o m. Harness q o m -> m ()
dispose h = h.socket.dispose

-- | Wait for something to become true, then assert it.
--
-- The driver forks a component's finalizers, so @dispose@ returns before they
-- have run; anything an effect's cleanup does has to be waited for rather than
-- assumed to have happened.
eventually :: forall a m. (MonadIO m) => (Eq a, Show a) => a -> m a -> m ()
eventually expected act = go (1000 :: Int)
  where
    go n = do
      actual <- act
      if actual == expected || n == 0
        then liftIO (actual `shouldBe` expected)
        else liftIO (threadDelay 1000) *> go (n - 1)

-- | The text of the most recent render.
lastRender :: forall q o m. (MonadIO m) => Harness q o m -> m Text
lastRender h = liftIO $ fromMaybe "" . lastMay <$> readIORef h.renders

renderSpec :: forall m. (MonadIO m) => IORef [Text] -> AD.RenderSpec m TestRenderState
renderSpec renders =
  AD.RenderSpec
    { AD.render = renderHtml renders
    , AD.renderChild = identity
    , AD.removeChild = \_ -> pure ()
    , AD.dispose = \_ -> pure ()
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
renderHtml renders _handler renderChild html _prev = do
  -- Forced here, not when a test looks at it: what a render says is a fact
  -- about the moment it happened, and a hook program may have counted
  -- something into it.
  rendered <- liftIO $ evaluate (textOf (HC.unHTML html))
  liftIO $ modifyIORef' renders (<> [rendered])
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
