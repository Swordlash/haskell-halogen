{-# LANGUAGE CPP #-}

module Main where

import Data.Row (type (.==))
import Halogen qualified as H
import Halogen.Canvas.Pixi qualified as Pixi
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (runBrowserDOM)
import Halogen.VDom.Driver (runUI)
#endif

data GameEvent = TileClicked Int
  deriving stock (Eq, Show)

newtype Action = CanvasOutput (Pixi.CanvasEvent GameEvent)

type Slots = "canvas" .== H.Slot H.VoidF (Pixi.CanvasEvent GameEvent) ()

scene :: Pixi.View GameEvent
scene = Pixi.view Pixi.defaultCamera Pixi.defaultInteraction $ do
  let titleStyle =
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
  Pixi.keyed "title" $ Pixi.text (Pixi.Point (-315) (-300)) "PIXEL FIELD" titleStyle
  let gridStroke = Pixi.StrokeStyle {strokeColor = 0x334155, strokeWidth = 2, strokeAlpha = 1}
  Pixi.keyed "grid" $ for_ [-1200, -1100 .. 1200] $ \coordinate -> do
    Pixi.line (Pixi.Point coordinate (-1200)) (Pixi.Point coordinate 1200) gridStroke
    Pixi.line (Pixi.Point (-1200) coordinate) (Pixi.Point 1200 coordinate) gridStroke
  let tileTexture = Pixi.Texture "./pixi-tile.svg"
  for_ [0 .. 17] $ \index -> do
    let column = index `mod` 6
        row = index `div` 6
        position = Pixi.Point (fromIntegral column * 170 - 425) (fromIntegral row * 170 - 170)
    Pixi.keyed (show index)
      $ Pixi.clickable (TileClicked index)
      $ Pixi.sprite position (Pixi.Point 88 88) tileTexture

parent :: H.Component H.VoidF () Void IO
parent =
  H.mkComponent $
    H.ComponentSpec
      { initialState = const $ pure ()
      , render = const render
      , eval = H.mkEval $ H.defaultEval {H.handleAction = handleAction}
      }
  where
    render :: H.ComponentHTML Action Slots IO
    render =
      HH.div
        [HP.styleText "position:fixed;inset:0;overflow:hidden;background:#111827"]
        [ HH.slot "canvas" () Pixi.component scene CanvasOutput
        , HH.div
            [HP.styleText "position:absolute;left:20px;top:16px;color:#f8fafc;pointer-events:none;font:16px system-ui"]
            [HH.text "Drag to pan · wheel to zoom · click a tile"]
        ]

    handleAction (CanvasOutput event) = liftIO $ print event

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
