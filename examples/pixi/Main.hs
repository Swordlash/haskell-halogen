{-# LANGUAGE CPP #-}

module Main where

import Data.Row (type (.==))
import Halogen qualified as H
import Halogen.Canvas.Elements qualified as CE
import Halogen.Canvas.Pixi qualified as Pixi
import Halogen.Canvas.Properties qualified as CP
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.Svg.Attributes qualified as SA
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

-- | One tile, drawn rather than loaded.
--
-- The artwork was an SVG asset until the canvas language grew a path
-- primitive; it is now the same drawing expressed in the path commands from
-- "Halogen.Svg.Attributes", which is one fewer file to ship and to keep in
-- step with the code. The two gradients the asset had are flat fills here -
-- 'Pixi.FillStyle' is a single colour.
--
-- The group carries the position and the interaction, so the parts below it
-- are drawn around an origin of their own and none of them has to know where
-- the tile ended up.
tile :: Maybe Target -> Int -> Pixi.PixiNode GameEvent
tile hovered index =
  CE.group
    ( CP.at (tileAt index)
        : CP.onClick (const $ Just $ TileClicked index)
        : CP.hitArea (Pixi.RectHit (Pixi.Point 0 0) (Pixi.Point 88 88))
        : hoverProps (Tile index) hovered
    )
    [ CE.rectangle_ (Pixi.Point 0 0) (Pixi.Point 82.5 82.5) (Just plate) (Just plateEdge)
    , CE.path_ gem (Just gemFill) (Just gemEdge)
    , CE.path_ facets Nothing (Just facetEdge)
    , CE.circle_ (art 50 43) (scaled 7) (Just highlight) Nothing
    ]
  where
    plate = Pixi.FillStyle {fillColor = 0x0ea5e9, fillAlpha = 1}
    plateEdge = Pixi.StrokeStyle {strokeColor = 0x082f49, strokeWidth = scaled 8, strokeAlpha = 1}
    gemFill = Pixi.FillStyle {fillColor = 0x7dd3fc, fillAlpha = 1}
    gemEdge = Pixi.StrokeStyle {strokeColor = 0xe0f2fe, strokeWidth = scaled 5, strokeAlpha = 1}
    facetEdge = Pixi.StrokeStyle {strokeColor = 0x075985, strokeWidth = scaled 4, strokeAlpha = 0.8}
    highlight = Pixi.FillStyle {fillColor = 0xffffff, fillAlpha = 0.8}

    gem = polyline [(64, 22), (101, 51), (87, 99), (41, 99), (27, 51)] <> [SA.z]
    facets =
      polyline [(27, 51), (64, 68), (101, 51)]
        <> polyline [(64, 22), (64, 68), (41, 99)]
        <> polyline [(64, 68), (87, 99)]

    polyline = \case
      [] -> []
      (startX, startY) : rest ->
        uncurry (SA.m SA.Abs) (place startX startY)
          : map (uncurry (SA.l SA.Abs) . uncurry place) rest
    place x y = let Pixi.Point x' y' = art x y in (x', y')

-- | Artwork coordinates were drawn on the asset's 128 unit square, with the
-- origin at its top left. The tile is 88 units across and drawn around its
-- centre, so both axes shift and shrink.
art :: Double -> Double -> Pixi.Point
art x y = Pixi.Point (scaled (x - 64)) (scaled (y - 64))

scaled :: Double -> Double
scaled value = value * 88 / 128

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
