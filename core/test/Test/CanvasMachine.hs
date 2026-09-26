{-# LANGUAGE CPP #-}

-- | Memoized parts of a canvas scene, through the machine that reconciles
-- them: what a render skips, what it still updates, and what it removes.
-- The scene is built on the in-memory DOM, with an attribute machine that
-- keeps an element's tap handler in a cell the way the Pixi backend does.
module Test.CanvasMachine (spec) where

import Prelude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "canvas machine" $ pure ()

#else

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Halogen.Canvas.Core
import Halogen.Canvas.Elements qualified as CE
import Halogen.Canvas.Types
import Halogen.VDom qualified as V
import Halogen.VDom.DOM.Monad (DomElement, DomNode, MemDOM, runMemDOM, setAttribute)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.Thunk (Thunk)
import Halogen.VDom.Thunk qualified as Thunk
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual)
import Web.HTML.Common (AttrName (..))

data Tap = Tap

data Action = Clicked | Hovered
  deriving stock (Eq, Show)

-- Top-level, so that each is the same function in every render.
asClicked, asHovered :: Tap -> Action
asClicked Tap = Clicked
asHovered Tap = Hovered

style :: TextStyle
style = TextStyle {font = SystemFont "serif", fontSize = 12, textColor = 0, textAlign = AlignLeft}

-- | A label drawn from a number, which sends 'Tap' when tapped.
part :: Int -> CanvasNode () Tap
part = CE.memoized (==) (\n -> CE.text (Point 0 0) (T.pack (show n)) style [Handler PointerTap (const (Just Tap))])

-- | What the attribute machines saw: how many times a label's props were
-- patched, and the tap handler of each element that has one, by node.
data Rig = Rig
  { patches :: IORef Int
  , taps :: IORef (Map Int (IORef (() -> Maybe Action)))
  }

attributes :: Rig -> DomElement MemDOM -> V.Machine MemDOM [CanvasProp () Action] ()
attributes rig el = build
  where
    node = N.toNative el
    build props = do
      cell <- liftIO (newIORef (const Nothing))
      apply cell props
      pure (V.Step () cell patch halt)
    patch cell props = do
      -- The containers around the parts are patched on every render.
      when (any isLabel props) $ liftIO (modifyIORef' rig.patches (+ 1))
      apply cell props
      pure (V.Step () cell patch halt)
    halt _ = liftIO (modifyIORef' rig.taps (Map.delete node.ident))
    isLabel = \case
      Label _ _ -> True
      _ -> False
    apply cell props = for_ props $ \case
      Label txt _ -> setAttribute Nothing (AttrName "label") txt el
      Handler PointerTap f -> liftIO (writeIORef cell f >> modifyIORef' rig.taps (Map.insert node.ident cell))
      _ -> pure ()

type Scene = V.Step MemDOM (V.VDom [CanvasProp () Action] (Thunk (CanvasNode ()) Action)) (DomNode MemDOM)

-- | A scene on the in-memory DOM, and the steps to render it again, tap
-- everything, and read what is on it.
data Canvas = Canvas
  { render :: CanvasNode () Action -> IO ()
  , tapAll :: IO [Maybe Action]
  , patched :: IO Int
  , labels :: IO [(Int, Text)]
  -- ^ Each label on the scene, in order, and the node that shows it.
  , listening :: IO [Int]
  , remove :: IO ()
  }

mount :: CanvasNode () Action -> IO Canvas
mount first = do
  rig <- Rig <$> newIORef 0 <*> newIORef Map.empty
  doc <- N.newDocument
  let vdomSpec = V.VDomSpec {V.buildWidget = Thunk.buildThunk unCanvasNode, V.buildAttributes = attributes rig, V.document = N.fromNative doc}
  scene <- newIORef =<< runMemDOM (V.buildVDom vdomSpec (unCanvasNode first))
  let current :: IO Scene
      current = readIORef scene
  pure
    Canvas
      { render = \next -> current >>= \s -> runMemDOM (V.step s (unCanvasNode next)) >>= writeIORef scene
      , tapAll = readIORef rig.taps >>= traverse (fmap ($ ()) . readIORef) . Map.elems
      , patched = readIORef rig.patches
      , labels = do
          root <- N.toNative . V.extract <$> current
          kids <- N.childNodes root
          concat <$> traverse (\k -> map ((k.ident,) . snd) . filter ((== (Nothing, "label")) . fst) <$> N.attributeList k) kids
      , listening = Map.keys <$> readIORef rig.taps
      , remove = current >>= runMemDOM . V.halt
      }

mapping :: IO ()
mapping = do
  canvas <- mount (CE.group_ [fmap asClicked (part 3)])
  canvas.tapAll >>= assertEqual "mounted" [Just Clicked]
  [(node, _)] <- canvas.labels
  canvas.render (CE.group_ [fmap asClicked (part 3)])
  canvas.patched >>= assertEqual "equal input, same mapping: the part is skipped" 0
  canvas.render (CE.group_ [fmap asClicked (part 4)])
  canvas.patched >>= assertEqual "another input: its props are updated" 1
  canvas.labels >>= assertEqual "in place" [(node, "4")]
  canvas.render (CE.group_ [fmap asHovered (part 4)])
  canvas.tapAll >>= assertEqual "equal input, another mapping: the new action" [Just Hovered]
  canvas.labels >>= assertEqual "still in place" [(node, "4")]
  canvas.render (CE.group_ [fmap asHovered (part 4)])
  canvas.patched >>= assertEqual "and skipped again once the mapping holds" 2
  canvas.remove
  canvas.listening >>= assertEqual "no listener after the scene is gone" []

keyedParts :: IO ()
keyedParts = do
  let row keys = CE.keyedGroup [] [(T.pack (show k), fmap asClicked (part k)) | k <- keys]
  canvas <- mount (row [1, 2])
  before <- canvas.labels
  canvas.render (row [2, 1])
  canvas.labels >>= assertEqual "moved, the same nodes" (reverse before)
  canvas.patched >>= assertEqual "and not patched" 0
  canvas.tapAll >>= assertEqual "both still listen" [Just Clicked, Just Clicked]
  canvas.render (row [2])
  let kept = mapMaybe (\(n, l) -> if l == "2" then Just n else Nothing) before
  canvas.listening >>= assertEqual "the removed part's listener is gone" kept
  canvas.remove
  canvas.listening >>= assertEqual "and the rest with the scene" []

spec :: Spec
spec =
  describe "canvas machine" $ do
    it "skips a memoized part while its input and mapping hold, and updates it when either changes" mapping
    it "keeps a keyed memoized part's nodes when it moves, and its listeners go with it" keyedParts

#endif
