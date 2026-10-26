{-# LANGUAGE CPP #-}

-- | What "Halogen.Trace" reports of a tree: the events of a component's
-- life in order, who a child's parent is, nothing at all while nobody
-- listens, and a state put back with 'Trace.restoreState'. Run with a
-- render spec that touches no DOM, so that it runs on every backend; where
-- a component stands on the page is checked against the in-memory DOM,
-- natively.
module Test.Trace (spec) where

import Control.Exception (ErrorCall (..), SomeException, throwIO, try)
import Control.Monad (void, when)
import Control.Monad.State.Class (gets, modify)
import Data.Dynamic (fromDynamic, toDyn)
import Data.Foldable (traverse_)
import Data.IORef
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Row (Row, type (.==))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Halogen qualified as H
import Halogen.Component (ComponentSlot (..))
import Halogen.HTML qualified as HH
import Halogen.HTML.Core qualified as HC
import Halogen.IO.Driver qualified as AD
import Halogen.IO.Driver.State (RenderStateX (..))
import Halogen.Query.Input (Input)
import Halogen.Subscription qualified as HS
import Halogen.Trace qualified as Trace
import Halogen.VDom.Types (VDom (..), runGraft)
import Prelude
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)
#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
import Data.Row (Empty)
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.Driver qualified as VD
import Halogen.VDom.Types (ElemName (..))
#endif

----------------------------------------------------------------------
-- A render spec with no DOM, which records the text of every render.

data Rendered (s :: Type) (act :: Type) (ps :: Row Type) (o :: Type) = Rendered

walking :: IORef [Text] -> AD.RenderSpec IO Rendered
walking = walkingWith (const (pure ()))

-- | 'walking', whose render also runs the check on each render's text: a
-- render fails when the check throws.
walkingWith :: (Text -> IO ()) -> IORef [Text] -> AD.RenderSpec IO Rendered
walkingWith check texts =
  AD.RenderSpec
    { AD.render = render
    , AD.renderChild = id
    , AD.removeChild = \_ -> pure ()
    , AD.dispose = \_ -> pure ()
    , AD.sameElement = \_ _ -> pure False
    , AD.rootNode = \_ -> pure Nothing
    }
  where
    render
      :: (Input act -> IO ())
      -> (H.ComponentSlotBox ps IO act -> IO (RenderStateX Rendered))
      -> HC.HTML (ComponentSlot ps IO act) act
      -> Maybe (Rendered s act ps o)
      -> IO (Rendered s act ps o)
    render _ renderChild html _ = do
      let (slots, text) = walk (HC.unHTML html)
      check (mconcat text)
      traverse_ (\case ComponentSlot box -> void (renderChild box); ThunkSlot _ -> pure ()) slots
      modifyIORef' texts (<> [mconcat text])
      pure Rendered

walk :: VDom p w -> ([w], [Text])
walk = \case
  Text t -> ([], [t])
  Elem _ _ _ cs -> foldMap walk cs
  Keyed _ _ _ cs -> foldMap (walk . snd) cs
  Widget w -> ([w], [])
  Grafted g -> walk (runGraft g)

----------------------------------------------------------------------
-- The components.

-- | A counter's state, a type of this module's, so that the events here
-- are told apart from any other tree's by their label. The emitter is
-- where its clicks come from: with no DOM, a component of this module
-- hears its actions through a subscription.
data Counter = Counter {count :: Int, clicks :: HS.Emitter IO ()}

data CounterQuery a = GetCount (Int -> a)

counter :: H.Component CounterQuery (HS.Emitter IO ()) Void IO
counter =
  H.component
    (\clicks -> pure Counter {count = 0, clicks})
    (\c -> HH.text (T.pack (show c.count)))
    [ H.onInitialize (gets (.clicks) >>= void . H.subscribe)
    , H.onAction (\() -> modify (\c -> c {count = c.count + 1}))
    , H.onQuery (\(GetCount reply) -> gets (\c -> Just (reply c.count)))
    ]

-- | A component whose render the render spec refuses (its text is
-- "broken"), and a parent that renders it: the child's first render fails,
-- so the parent's does, and the tree is never made.
data Broken = Broken

broken :: H.Component H.VoidF () Void IO
broken = H.component (\() -> pure Broken) (\Broken -> HH.text "broken") []

refusingBroken :: Text -> IO ()
refusingBroken text = when (text == "broken") (throwIO (ErrorCall "a render that fails"))

data Breaking = Breaking

breaking :: H.Component H.VoidF () Void IO
breaking =
  H.component
    (\() -> pure Breaking)
    (\Breaking -> HH.div_ [HH.slot_ "child" () broken ()] :: H.ComponentHTML () ("child" .== H.Slot H.VoidF Void ()) IO)
    []

-- | A parent rendering one counter, named.
data Parent = Parent (HS.Emitter IO ())

parent :: H.Component H.VoidF (HS.Emitter IO ()) Void IO
parent =
  H.component
    (pure . Parent)
    (\(Parent clicks) -> HH.div_ [HH.slot_ "child" () (H.named "child counter" counter) clicks] :: H.ComponentHTML () ("child" .== H.Slot CounterQuery Void ()) IO)
    []

----------------------------------------------------------------------
-- Listening.

-- | The events of this module's components, oldest first, while the body
-- runs with a listener.
listened :: (IORef [Trace.Event] -> IO a) -> IO a
listened body = do
  events <- newIORef []
  stop <- Trace.subscribe $ \event ->
    if "Test.Trace." `T.isPrefixOf` event.tag.label || event.tag.label == "child counter"
      then modifyIORef' events (<> [event])
      else pure ()
  result <- body events
  stop
  pure result

-- | What happened, by constructor.
kind :: Trace.What -> String
kind = takeWhile (/= ' ') . show

ask :: AD.HalogenSocket q o IO -> q a -> IO (Maybe a)
ask AD.HalogenSocket {AD.query = q} = q

spec :: Spec
spec = describe "Halogen.Trace" $ do
  it "reports a component's life in order" $ do
    texts <- newIORef []
    clicks <- HS.create
    kinds <- listened $ \events -> do
      socket <- AD.runUI (walking texts) counter clicks.emitter
      HS.notify clicks.listener ()
      socket.dispose
      map (kind . (.what)) <$> readIORef events
    assertEqual
      "the events"
      ["Mounted", "Rendered", "Subscribed", "Initialized", "Action", "StateChanged", "Rendered", "Finalized"]
      kinds

  it "labels a component with its state's type, or its name, and gives a child its parent" $ do
    texts <- newIORef []
    clicks <- HS.create
    tags <- listened $ \events -> do
      socket <- AD.runUI (walking texts) parent clicks.emitter
      socket.dispose
      mapMaybe (\e -> case e.what of Trace.Mounted -> Just e.tag; _ -> Nothing) <$> readIORef events
    case tags of
      [root, child] -> do
        assertEqual "the root's label" "Test.Trace.Parent" root.label
        assertEqual "the root's parent" Nothing root.parent
        assertEqual "the child's label" "child counter" child.label
        assertEqual "the child's parent" (Just root.id) child.parent
        assertWith "the child's state type is known" (fmap show child.stateType == Just "Counter")
      _ -> assertWith ("two components mounted, found " <> show tags) False

  it "forgets a component whose first render throws" $ do
    texts <- newIORef []
    (kinds, keys) <- listened $ \events -> do
      failed <- try (AD.runUI (walkingWith refusingBroken texts) breaking ()) :: IO (Either SomeException (AD.HalogenSocket H.VoidF Void IO))
      assertWith "the mount failed" (either (const True) (const False) failed)
      evs <- readIORef events
      pure (map (kind . (.what)) evs, [e.tag.id | e <- evs, Trace.Mounted <- [e.what]])
    -- The parent mounts, then the child; the child's render fails, which
    -- ends the child, and the parent's render with it.
    assertEqual "the events" ["Mounted", "Mounted", "Finalized", "Finalized"] kinds
    case keys of
      [parentKey, childKey] -> do
        Trace.restoreState childKey (toDyn Broken) >>= assertEqual "the child is not kept" False
        Trace.restoreState parentKey (toDyn Breaking) >>= assertEqual "the parent is not kept" False
      _ -> assertWith ("two components mounted, found " <> show keys) False

  it "reports nothing, and keeps nothing, while nobody listens" $ do
    texts <- newIORef []
    clicks <- HS.create
    heard <- newIORef (0 :: Int)
    stop <- Trace.subscribe (\_ -> modifyIORef' heard (+ 1))
    stop
    Trace.listening >>= assertEqual "nobody listens" False
    socket <- AD.runUI (walking texts) counter clicks.emitter
    HS.notify clicks.listener ()
    readIORef heard >>= assertEqual "a listener gone hears nothing" 0
    -- Made while nobody listened, the counter is not kept: its state cannot
    -- be put back, though its events are heard once someone listens.
    restored <- listened $ \events -> do
      HS.notify clicks.listener ()
      evs <- readIORef events
      case [(e.tag.id, d) | e <- evs, Trace.StateChanged (Just d) <- [e.what]] of
        (key, d) : _ -> Trace.restoreState key d
        [] -> pure True
    assertEqual "a component made while nobody listened is not kept" False restored
    socket.dispose

  it "puts an earlier state back, and renders it" $ do
    texts <- newIORef []
    clicks <- HS.create
    listened $ \events -> do
      socket <- AD.runUI (walking texts) counter clicks.emitter
      HS.notify clicks.listener ()
      HS.notify clicks.listener ()
      evs <- readIORef events
      case [(e.tag.id, d) | e <- evs, Trace.StateChanged (Just d) <- [e.what]] of
        [(key, first), _] -> do
          assertEqual "the first state carried" (Just 1) ((.count) <$> fromDynamic @Counter first)
          Trace.restoreState key first >>= assertEqual "restored" True
          ask socket (H.mkRequest GetCount) >>= assertEqual "the count is back" (Just 1)
          readIORef texts >>= assertEqual "rendered again" ["0", "1", "2", "1"]
          after <- drop (length evs) <$> readIORef events
          assertEqual "the change reported, and the render" ["StateChanged", "Rendered", "Queried"] (map (kind . (.what)) after)
          Trace.restoreState key (toDyn (5 :: Int)) >>= assertEqual "refused another type" False
          socket.dispose
          Trace.restoreState key first >>= assertEqual "refused once the component is gone" False
          Trace.restoreState (-1) first >>= assertEqual "refused for no component" False
        found -> assertWith ("two state changes, found " <> show (length found)) False

#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
  it "finds the node a component stands for on the page" $
    listened $ \events -> do
      container <- N.newElement Nothing (ElemName "div")
      socket <- runMemDOM (VD.runUI shown () (N.fromNative container))
      evs <- readIORef events
      case [e.tag.id | e <- evs, Trace.Mounted <- [e.what]] of
        [key] -> do
          Just node <- Trace.rootNode key
          N.renderToText (N.toNative node) >>= assertEqual "the root's node" "<p>shown</p>"
          runMemDOM socket.dispose
          Trace.rootNode key >>= assertWith "nothing once the component is gone" . null
        found -> assertWith ("one component mounted, found " <> show found) False
#endif

#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
-- | A paragraph, rendered into the in-memory DOM.
data Shown = Shown

shown :: H.Component H.VoidF () Void MemDOM
shown = H.component (\_ -> pure Shown) (\_ -> HH.p_ [HH.text "shown"] :: H.ComponentHTML () Empty MemDOM) []
#endif
