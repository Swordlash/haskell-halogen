{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Halogen.Canvas.Pixi.FFI
  ( Application
  , Object
  , Event
  , Callback
  , Canvas
  , Timer
  , canvas
  , newApplication
  , initializeApplication
  , applicationCreated
  , applicationReady
  , destroyApplication
  , newContainer
  , newGraphics
  , addToStage
  , addChild
  , removeChild
  , parentOf
  , setChildIndex
  , destroyObject
  , clearGraphics
  , moveTo
  , lineTo
  , rect
  , circle
  , ellipse
  , quadraticCurveTo
  , bezierCurveTo
  , arc
  , svgPath
  , fill
  , stroke
  , newText
  , setSystemText
  , setAssetText
  , clearText
  , newSprite
  , setTexture
  , clearTexture
  , centerAnchor
  , setPosition
  , setScale
  , setRotation
  , setSize
  , setOutline
  , clearOutline
  , refreshOutline
  , setCacheAsTexture
  , clearCacheAsTexture
  , addListener
  , removeListener
  , setEventMode
  , setCursor
  , setRectHitArea
  , setCircleHitArea
  , setPolygonHitArea
  , clearHitArea
  , enableStageEvents
  , onPointerDown
  , onPointerMove
  , onPointerEnd
  , onWheel
  , removeWheel
  , pointerId
  , globalX
  , globalY
  , localX
  , localY
  , eventButton
  , currentTarget
  , preventDefault
  , clientX
  , clientY
  , deltaY
  , canvasLeft
  , canvasTop
  , canvasWidth
  , canvasHeight
  , screenWidth
  , screenHeight
  , mkCallback
  , freeCallback
  , scheduleTimeout
  , cancelTimeout
  )
where

import Data.Foreign
import Halogen.JSBits hiding (mkCallback)
import Protolude
import Web.DOM.Internal.Types (HTMLElement (..))
import Web.Event.Internal.Types (Event (..))

newtype Application = Application (Foreign Application)
  deriving newtype (Inert)

newtype Object = Object (Foreign Object)
  deriving newtype (Inert)

newtype Timer = Timer (Foreign Timer)
  deriving newtype (Inert)

newtype Canvas = Canvas (Foreign HTMLElement)

canvas :: HTMLElement -> Canvas
canvas (HTMLElement value) = Canvas value

-- Natively there is no engine to talk to, so every binding ignores its
-- arguments and the handles are inert placeholders, only ever passed around.
$(browserJS ["jsbits/pixi.js"]
  [ ("newApplication", "halogen_pixi_new_application", Unsafe, [t| IO Application |])
  , ("initializeApplicationRaw", "halogen_pixi_initialize_application", Unsafe, [t| Application -> JSText -> Canvas -> Callback -> IO () |])
  , ("applicationCreated", "halogen_pixi_application_created", Unsafe, [t| Application -> IO Bool |])
  , ("applicationReady", "halogen_pixi_application_ready", Unsafe, [t| Application -> IO Bool |])
  , ("destroyApplication", "halogen_pixi_destroy_application", Unsafe, [t| Application -> IO () |])
  , ("newContainer", "halogen_pixi_new_container", Unsafe, [t| Application -> IO Object |])
  , ("newGraphics", "halogen_pixi_new_graphics", Unsafe, [t| Application -> IO Object |])
  , ("addToStage", "halogen_pixi_add_to_stage", Unsafe, [t| Application -> Object -> IO () |])
  , ("addChild", "halogen_pixi_add_child", Unsafe, [t| Object -> Object -> IO () |])
  , ("removeChild", "halogen_pixi_remove_child", Unsafe, [t| Object -> Object -> IO () |])
  , ("parentOfRaw", "halogen_pixi_parent_of", Unsafe, [t| Object -> IO (Nullable Object) |])
  , ("setChildIndex", "halogen_pixi_set_child_index", Unsafe, [t| Object -> Object -> Int -> IO () |])
  , ("destroyObject", "halogen_pixi_destroy_object", Unsafe, [t| Object -> IO () |])
  , ("clearGraphics", "halogen_pixi_clear_graphics", Unsafe, [t| Object -> IO () |])
  , ("moveTo", "halogen_pixi_move_to", Unsafe, [t| Object -> Double -> Double -> IO () |])
  , ("lineTo", "halogen_pixi_line_to", Unsafe, [t| Object -> Double -> Double -> IO () |])
  , ("rect", "halogen_pixi_rect", Unsafe, [t| Object -> Double -> Double -> Double -> Double -> IO () |])
  , ("circle", "halogen_pixi_circle", Unsafe, [t| Object -> Double -> Double -> Double -> IO () |])
  , ("ellipse", "halogen_pixi_ellipse", Unsafe, [t| Object -> Double -> Double -> Double -> Double -> IO () |])
  , ("quadraticCurveTo", "halogen_pixi_quadratic_curve_to", Unsafe, [t| Object -> Double -> Double -> Double -> Double -> IO () |])
  , ("bezierCurveTo", "halogen_pixi_bezier_curve_to", Unsafe, [t| Object -> Double -> Double -> Double -> Double -> Double -> Double -> IO () |])
  , ("arc", "halogen_pixi_arc", Unsafe, [t| Object -> Double -> Double -> Double -> Double -> Double -> Bool -> IO () |])
  , ("svgPathRaw", "halogen_pixi_svg_path", Unsafe, [t| Application -> Object -> JSText -> IO () |])
  , ("fill", "halogen_pixi_fill", Unsafe, [t| Object -> Int -> Double -> IO () |])
  , ("stroke", "halogen_pixi_stroke", Unsafe, [t| Object -> Int -> Double -> Double -> IO () |])
  , ("newText", "halogen_pixi_new_text", Unsafe, [t| Application -> IO Object |])
  , ("setSystemTextRaw", "halogen_pixi_set_system_text", Unsafe, [t| Object -> JSText -> JSText -> Double -> Int -> JSText -> IO () |])
  , ("setAssetTextRaw", "halogen_pixi_set_asset_text", Unsafe, [t| Application -> Object -> JSText -> JSText -> JSText -> Double -> Int -> JSText -> IO () |])
  , ("newSprite", "halogen_pixi_new_sprite", Unsafe, [t| Application -> IO Object |])
  , ("setTextureRaw", "halogen_pixi_set_texture", Unsafe, [t| Application -> Object -> JSText -> IO () |])
  , ("clearText", "halogen_pixi_clear_text", Unsafe, [t| Object -> IO () |])
  , ("clearTexture", "halogen_pixi_clear_texture", Unsafe, [t| Application -> Object -> IO () |])
  , ("centerAnchor", "halogen_pixi_center_anchor", Unsafe, [t| Object -> IO () |])
  , ("setPosition", "halogen_pixi_set_position", Unsafe, [t| Object -> Double -> Double -> IO () |])
  , ("setScale", "halogen_pixi_set_scale", Unsafe, [t| Object -> Double -> Double -> IO () |])
  , ("setRotation", "halogen_pixi_set_rotation", Unsafe, [t| Object -> Double -> IO () |])
  , ("setSize", "halogen_pixi_set_size", Unsafe, [t| Object -> Double -> Double -> IO () |])
  , ("setOutline", "halogen_pixi_set_outline", Unsafe, [t| Application -> Object -> Int -> Double -> Double -> Double -> IO () |])
  , ("clearOutline", "halogen_pixi_clear_outline", Unsafe, [t| Object -> IO () |])
  , ("refreshOutline", "halogen_pixi_refresh_outline", Unsafe, [t| Object -> IO () |])
  , ("setCacheAsTexture", "halogen_pixi_set_cache_as_texture", Unsafe, [t| Application -> Object -> Double -> IO () |])
  , ("clearCacheAsTexture", "halogen_pixi_clear_cache_as_texture", Unsafe, [t| Object -> IO () |])
  , ("onRaw", "halogen_pixi_on", Unsafe, [t| Object -> JSText -> Callback -> IO () |])
  , ("offRaw", "halogen_pixi_off", Unsafe, [t| Object -> JSText -> Callback -> IO () |])
  , ("setEventModeRaw", "halogen_pixi_set_event_mode", Unsafe, [t| Object -> JSText -> IO () |])
  , ("setCursorRaw", "halogen_pixi_set_cursor", Unsafe, [t| Object -> JSText -> IO () |])
  , ("setRectHitArea", "halogen_pixi_set_rect_hit_area", Unsafe, [t| Application -> Object -> Double -> Double -> Double -> Double -> IO () |])
  , ("setCircleHitArea", "halogen_pixi_set_circle_hit_area", Unsafe, [t| Application -> Object -> Double -> Double -> Double -> IO () |])
  , ("setPolygonHitAreaRaw", "halogen_pixi_set_polygon_hit_area", Unsafe, [t| Application -> Object -> JSText -> IO () |])
  , ("clearHitArea", "halogen_pixi_clear_hit_area", Unsafe, [t| Object -> IO () |])
  , ("enableStageEvents", "halogen_pixi_enable_stage_events", Unsafe, [t| Application -> IO () |])
  , ("onPointerDown", "halogen_pixi_on_pointer_down", Unsafe, [t| Application -> Callback -> IO () |])
  , ("onPointerMove", "halogen_pixi_on_pointer_move", Unsafe, [t| Application -> Callback -> IO () |])
  , ("onPointerEnd", "halogen_pixi_on_pointer_end", Unsafe, [t| Application -> Callback -> IO () |])
  , ("onWheel", "halogen_pixi_on_wheel", Unsafe, [t| Canvas -> Callback -> IO () |])
  , ("removeWheel", "halogen_pixi_remove_wheel", Unsafe, [t| Canvas -> Callback -> IO () |])
  , ("pointerId", "halogen_pixi_pointer_id", Unsafe, [t| Event -> Int |])
  , ("globalX", "halogen_pixi_global_x", Unsafe, [t| Event -> Double |])
  , ("globalY", "halogen_pixi_global_y", Unsafe, [t| Event -> Double |])
  , ("localX", "halogen_pixi_local_x", Unsafe, [t| Object -> Event -> Double |])
  , ("localY", "halogen_pixi_local_y", Unsafe, [t| Object -> Event -> Double |])
  , ("eventButton", "halogen_pixi_event_button", Unsafe, [t| Event -> Int |])
  , ("currentTarget", "halogen_pixi_current_target", Unsafe, [t| Event -> Object |])
  , ("preventDefault", "halogen_pixi_prevent_default", Unsafe, [t| Event -> IO () |])
  , ("clientX", "halogen_pixi_client_x", Unsafe, [t| Event -> Double |])
  , ("clientY", "halogen_pixi_client_y", Unsafe, [t| Event -> Double |])
  , ("deltaY", "halogen_pixi_delta_y", Unsafe, [t| Event -> Double |])
  , ("snapshot", "halogen_pixi_snapshot", Unsafe, [t| JSVal -> IO JSVal |])
  , ("canvasLeft", "halogen_pixi_canvas_left", Unsafe, [t| Canvas -> IO Double |])
  , ("canvasTop", "halogen_pixi_canvas_top", Unsafe, [t| Canvas -> IO Double |])
  , ("canvasWidth", "halogen_pixi_canvas_width", Unsafe, [t| Canvas -> IO Double |])
  , ("canvasHeight", "halogen_pixi_canvas_height", Unsafe, [t| Canvas -> IO Double |])
  , ("screenWidth", "halogen_pixi_screen_width", Unsafe, [t| Application -> IO Double |])
  , ("screenHeight", "halogen_pixi_screen_height", Unsafe, [t| Application -> IO Double |])
  , ("scheduleTimeout", "halogen_pixi_schedule_timeout", Unsafe, [t| Callback -> Int -> IO Timer |])
  , ("cancelTimeout", "halogen_pixi_cancel_timeout", Unsafe, [t| Timer -> IO () |])
  ])

-- | Start the application on the canvas, loading Pixi from the URL; the
-- callback runs once it is up, or has failed to come up. Natively at once.
initializeApplication :: Application -> Text -> Canvas -> Callback -> IO ()
initializeApplication application url canvas' done
  | inBrowser = initializeApplicationRaw application (toJSText url) canvas' done
  | otherwise = invokeCallback done inert
setSystemText :: Object -> Text -> Text -> Double -> Int -> Text -> IO ()
setSystemText object value family size color align = setSystemTextRaw object (toJSText value) (toJSText family) size color (toJSText align)
setAssetText :: Application -> Object -> Text -> Text -> Text -> Double -> Int -> Text -> IO ()
setAssetText application object value family source size color align = setAssetTextRaw application object (toJSText value) (toJSText family) (toJSText source) size color (toJSText align)
setTexture :: Application -> Object -> Text -> IO ()
setTexture application object asset = setTextureRaw application object (toJSText asset)
svgPath :: Application -> Object -> Text -> IO ()
svgPath application object commands = svgPathRaw application object (toJSText commands)
parentOf :: Object -> IO (Maybe Object)
parentOf object = fmap Object . nullableToMaybe <$> parentOfRaw object
addListener :: Object -> Text -> Callback -> IO ()
addListener object eventType = onRaw object (toJSText eventType)
removeListener :: Object -> Text -> Callback -> IO ()
removeListener object eventType = offRaw object (toJSText eventType)
setEventMode :: Object -> Text -> IO ()
setEventMode object mode = setEventModeRaw object (toJSText mode)
setCursor :: Object -> Text -> IO ()
setCursor object cursor = setCursorRaw object (toJSText cursor)
setPolygonHitArea :: Application -> Object -> [Double] -> IO ()
setPolygonHitArea application object coordinates = setPolygonHitAreaRaw application object (toJSText (polygonText coordinates))
-- | A polygon's coordinates as Pixi's Polygon takes them, flat: "x,y,x,y,…".
polygonText :: [Double] -> Text
polygonText = mconcat . intersperse "," . map show
-- | A handler Pixi calls with an event. It runs at once, so it may prevent
-- the event's default; it sees a snapshot of the event, which Pixi reuses.
mkCallback :: (Event -> IO ()) -> IO Callback
mkCallback handler = mkSyncCallback (\event -> snapshot event >>= handler . Event)
