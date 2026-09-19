{-# LANGUAGE CPP #-}

module Main where

import Data.Row (type (.==))
import Halogen qualified as H
import Halogen.Canvas.Elements qualified as CE
import Halogen.Canvas.Pixi qualified as Pixi
import Halogen.Canvas.Properties qualified as CP
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Protolude hiding (State, state)

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (runBrowserDOM)
import Halogen.VDom.Driver (runUI)
#endif

-- | What the scene can point at. Carried by both hover events so that a
-- @pointerout@ arriving after the pointer has already entered the next
-- object does not clear the new hover.
data Target = Title | Tile Int
  deriving stock (Eq, Show)

data GameEvent
  = TileClicked Int
  | Entered Target
  | Exited Target
  deriving stock (Eq, Show)

newtype Action = CanvasOutput (Pixi.CanvasEvent GameEvent)

type Slots = "canvas" .== H.Slot H.VoidF (Pixi.CanvasEvent GameEvent) ()

-- | The camera lives here because the scene is rebuilt on every hover; were
-- it a constant in 'scene', each hover would snap the view back to the
-- origin.
data State = State
  { camera :: Pixi.Camera
  , hovered :: Maybe Target
  }

----------------------------------------------------------------------
-- The scene

titleStyle :: Pixi.TextStyle
titleStyle =
  Pixi.TextStyle
    { font =
        Pixi.AssetFont
          { family = "Press Start 2P"
          , source = "https://cdn.jsdelivr.net/npm/@fontsource/press-start-2p@5.2.6/files/press-start-2p-latin-400-normal.woff2"
          }
    , fontSize = 28
    , textColor = 0xe0f2fe
    , textAlign = Pixi.AlignCenter
    }

titleText :: Text
titleText = "PIXEL FIELD"

titleAt :: Pixi.Point
titleAt = Pixi.Point (-315) (-300)

tileSize :: Pixi.Point
tileSize = Pixi.Point 88 88

tileAt :: Int -> Pixi.Point
tileAt index =
  Pixi.Point
    (fromIntegral (index `mod` 6) * 170 - 425)
    (fromIntegral (index `div` 6) * 170 - 170)

scene :: State -> Pixi.View GameEvent
scene state =
  Pixi.view
    state.camera
    Pixi.defaultInteraction
    [ grid
    , title state.hovered
    , CE.keyedGroup [] [(show index, tile state.hovered index) | index <- [0 .. 17]]
    ]

grid :: Pixi.PixiNode GameEvent
grid = CE.group_ $ foldMap line [-1200, -1100 .. 1200]
  where
    stroke = Pixi.StrokeStyle {strokeColor = 0x334155, strokeWidth = 2, strokeAlpha = 1}
    line coordinate =
      [ CE.line_ (Pixi.Point coordinate (-1200)) (Pixi.Point coordinate 1200) stroke
      , CE.line_ (Pixi.Point (-1200) coordinate) (Pixi.Point 1200 coordinate) stroke
      ]

title :: Maybe Target -> Pixi.PixiNode GameEvent
title hovered = CE.text titleAt titleText titleStyle (hoverProps Title hovered)

tile :: Maybe Target -> Int -> Pixi.PixiNode GameEvent
tile hovered index =
  CE.sprite (tileAt index) tileSize (Pixi.Texture "./pixi-tile.svg") $
    CP.onClick (const $ Just $ TileClicked index)
      : hoverProps (Tile index) hovered

-- | Report the pointer coming and going, and while it is here, frame the
-- object with a one pixel white border.
--
-- The border is a prop rather than a rectangle in the scene because only the
-- backend can say how big the object is: a label's extent is whatever the
-- font laid out, and a sprite's is whatever its texture turned out to be. It
-- is measured against the bounds Pixi hit-tests, so it frames exactly what
-- is clickable.
hoverProps :: Target -> Maybe Target -> [Pixi.PixiProp GameEvent]
hoverProps target hovered =
  [ CP.onPointerOver (const $ Just $ Entered target)
  , CP.onPointerOut (const $ Just $ Exited target)
  , CP.cursor "pointer"
  ]
    <> [ CP.outline Pixi.StrokeStyle {strokeColor = 0xffffff, strokeWidth = 1, strokeAlpha = 1} 1
       | hovered == Just target
       ]

----------------------------------------------------------------------

parent :: H.Component H.VoidF () Void IO
parent =
  H.mkComponent $
    H.ComponentSpec
      { initialState = const $ pure State {camera = Pixi.defaultCamera, hovered = Nothing}
      , render
      , eval = H.mkEval $ H.defaultEval {H.handleAction = handleAction}
      }
  where
    render :: State -> H.ComponentHTML Action Slots IO
    render state =
      HH.div
        [HP.styleText "position:fixed;inset:0;overflow:hidden;background:#111827"]
        [ HH.slot "canvas" () Pixi.component (scene state) CanvasOutput
        , HH.div
            [HP.styleText "position:absolute;left:20px;top:16px;color:#f8fafc;pointer-events:none;font:16px system-ui"]
            [HH.text "Drag to pan · wheel to zoom · click a tile"]
        ]

    handleAction (CanvasOutput output) = case output of
      Pixi.CameraChanged camera -> modify $ \state -> state {camera}
      Pixi.Fired (Entered target) -> modify $ \state -> state {hovered = Just target}
      Pixi.Fired (Exited target) ->
        modify $ \state -> if state.hovered == Just target then state {hovered = Nothing} else state
      Pixi.Fired (TileClicked index) -> liftIO $ putStrLn ("tile clicked: " <> show index :: Text)

main :: IO ()

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
main = void $ runBrowserDOM HA.awaitBody >>= runUI parent ()
#else
main = putStrLn ("The Pixi example can only run in a JavaScript or wasm browser target." :: Text)
#endif

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = main
#endif
