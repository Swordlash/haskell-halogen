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

-- | Nothing is kept per render: what the tests look at is the log.
data TestRenderState (s :: Type) (act :: Type) (ps :: Row Type) (o :: Type)
  = TestRenderState

data Harness q o = Harness
  { socket :: HalogenSocket q o IO
  , renders :: IORef [Text]
  , outputs :: IORef [o]
  }

-- | Mount a component and start watching it.
start :: forall q i o. Component q i o IO -> i -> IO (Harness q o)
start c i = do
  renders <- newIORef []
  outputs <- newIORef []
  socket <- AD.runUI (renderSpec renders) c i
  void $ HS.subscribe socket.messages $ \o -> modifyIORef' outputs (<> [o])
  pure Harness {socket, renders, outputs}

-- | Send a query to the component, as a parent would.
--
-- Not a field of 'Harness': 'HalogenSocket'\'s @query@ is rank-2, which a
-- record selector cannot be.
query :: forall q o a. Harness q o -> q a -> IO (Maybe a)
query h q = let HalogenSocket {query = send} = h.socket in send q

-- | Finalize the component.
dispose :: forall q o. Harness q o -> IO ()
dispose h = h.socket.dispose

-- | Wait for something to become true, then assert it.
--
-- The driver forks a component's finalizers, so @dispose@ returns before they
-- have run; anything an effect's cleanup does has to be waited for rather than
-- assumed to have happened.
eventually :: forall a. (Eq a, Show a) => a -> IO a -> IO ()
eventually expected act = go (1000 :: Int)
  where
    go n = do
      actual <- act
      if actual == expected || n == 0
        then actual `shouldBe` expected
        else threadDelay 1000 *> go (n - 1)

-- | The text of the most recent render.
lastRender :: forall q o. Harness q o -> IO Text
lastRender h = fromMaybe "" . lastMay <$> readIORef h.renders

renderSpec :: IORef [Text] -> AD.RenderSpec IO TestRenderState
renderSpec renders =
  AD.RenderSpec
    { AD.render = renderHtml renders
    , AD.renderChild = identity
    , AD.removeChild = \_ -> pure ()
    , AD.dispose = \_ -> pure ()
    }

renderHtml
  :: forall s act ps o
   . IORef [Text]
  -> (Input act -> IO ())
  -> (ComponentSlotBox ps IO act -> IO (RenderStateX TestRenderState))
  -> HC.HTML (ComponentSlot ps IO act) act
  -> Maybe (TestRenderState s act ps o)
  -> IO (TestRenderState s act ps o)
renderHtml renders _handler renderChild html _prev = do
  -- Forced here, not when a test looks at it: what a render says is a fact
  -- about the moment it happened, and a hook program may have counted
  -- something into it.
  rendered <- evaluate (textOf (HC.unHTML html))
  modifyIORef' renders (<> [rendered])
  traverse_ renderSlot (slotsOf (HC.unHTML html))
  pure TestRenderState
  where
    renderSlot :: ComponentSlot ps IO act -> IO ()
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
