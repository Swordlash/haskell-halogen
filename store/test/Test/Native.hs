{-# LANGUAGE CPP #-}

-- | A tree in 'Store.StoreT' rendered into the in-memory DOM by the real
-- VDom driver, natively: the DOM classes 'Store.StoreT' takes from the
-- monad underneath, an event's action updating the store, and a page
-- rendered ahead of the program adopted by a connected component.
module Test.Native (spec) where

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

import Protolude
import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "a store tree in the in-memory DOM" $ pure ()

#else

import Data.Foreign (toForeign)
import Data.IORef
import Data.NT (type (~>) (..))
import Data.Row (type (.==))
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.Store qualified as Store
import Halogen.VDom.DOM.Monad (MemDOM, appendChild, runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.Driver (hydrateUI, runUI)
import Halogen.VDom.Types (ElemName (..))
import Protolude
import Test.Connect (CounterQuery, Reported, connectedCounter)
import Test.Fixture
import Test.Harness (eventually)
import Test.Hspec (Spec, describe, it, shouldBe)
import Web.Event.Event (Event (..))

data Clicked = Clicked

type Slots = ("count" .== H.Slot CounterQuery Reported ())

-- | A button that increments the store, and a counter connected to it.
app :: forall q o m. (Store.MonadStore StoreAction Store m) => H.Component q () o m
app =
  H.component
    (\_ -> pure ())
    ( \_ ->
        HH.div_
          [ HH.button [HE.onClick (const Clicked)] [HH.text "+"]
          , HH.slot_ "count" () connectedCounter "count"
          ]
          :: H.ComponentHTML Clicked Slots m
    )
    [H.onAction (\Clicked -> Store.updateStore Increment)]

spec :: Spec
spec = describe "a store tree in the in-memory DOM" $ do
  it "runs in StoreT over the DOM, and a click's update shows in the connected component" $ do
    store <- Store.newHalogenStore initialStore reducer
    container <- N.newElement Nothing (ElemName "div")
    H.HalogenSocket {H.dispose = dispose} <- runMemDOM (runIn store (runUI app () (N.fromNative container)))
    markup container >>= (`shouldBe` "<div><button>+</button>count: 0</div>")
    traverse_ click =<< elementsNamed "button" container
    eventually "<div><button>+</button>count: 1</div>" (markup container)
    runIn store Store.getStore >>= (`shouldBe` Store {count = 1, name = "nobody"})
    runMemDOM (runIn store dispose)

  it "a connected component adopts the page rendered for it" $ do
    store <- Store.newHalogenStore initialStore reducer
    runIn store (Store.updateStore Increment)
    let component = H.hoist (NT (runIn store)) app
    container <- prerendered component
    before <- idents container
    markup container >>= (`shouldBe` "<div><button>+</button>count: 1</div>")
    H.HalogenSocket {H.dispose = dispose} <- runMemDOM (hydrateUI component () (N.fromNative container))
    -- Every node the page had is the one the tree now holds.
    idents container >>= (`shouldBe` before)
    traverse_ click =<< elementsNamed "button" container
    eventually "<div><button>+</button>count: 2</div>" (markup container)
    runMemDOM dispose

markup :: N.NativeNode -> IO Text
markup container = mconcat <$> (traverse N.renderToText =<< N.childNodes container)

-- | A container holding what the component rendered there, as a page that
-- arrived with it would: the component is disposed of and its tree put back
-- without the listeners it had.
prerendered :: H.Component H.VoidF () Void MemDOM -> IO N.NativeNode
prerendered component = do
  container <- N.newElement Nothing (ElemName "div")
  H.HalogenSocket {H.dispose = dispose} <- runMemDOM (runUI component () (N.fromNative container))
  rendered <- N.childNodes container
  runMemDOM dispose
  for_ rendered $ \node -> do
    stripListeners node
    runMemDOM (appendChild (N.fromNative node) (N.fromNative container))
  pure container
  where
    stripListeners node = do
      writeIORef node.listeners []
      mapM_ stripListeners =<< N.childNodes node

-- | Every node under this one, depth first.
descendants :: N.NativeNode -> IO [N.NativeNode]
descendants node = do
  children <- N.childNodes node
  concat <$> traverse (\c -> (c :) <$> descendants c) children

idents :: N.NativeNode -> IO [Int]
idents node = map (.ident) <$> descendants node

elementsNamed :: Text -> N.NativeNode -> IO [N.NativeNode]
elementsNamed name node = filter named <$> descendants node
  where
    named n = n.kind == N.ElementNode Nothing (ElemName name)

-- | Run the click listeners an element has, as a click would.
click :: N.NativeNode -> IO ()
click node = do
  listeners <- readIORef node.listeners
  for_ [l | l <- listeners, l.eventType == "click"] $ \l -> runMemDOM (l.fire (Event (toForeign ())))

#endif
