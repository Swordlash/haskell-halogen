{-# LANGUAGE CPP #-}
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

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Halogen.JSBits (browserJS, Safety (..))
#endif

import Data.Foreign
import Protolude
import Web.DOM.Internal.Types (HTMLElement (..))
import Web.Event.Internal.Types (Event (..))

#if defined(javascript_HOST_ARCH)
import GHC.JS.Foreign.Callback qualified as JS
import GHC.JS.Prim (JSVal, toJSString)
#elif defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim (JSString (..), JSVal, freeJSVal, toJSString)
#endif

newtype Application = Application (Foreign Application)

newtype Object = Object (Foreign Object)

newtype Timer = Timer (Foreign Timer)

newtype Canvas = Canvas (Foreign HTMLElement)

#if defined(javascript_HOST_ARCH)
type Callback = JS.Callback (JSVal -> IO ())
#elif defined(wasm32_HOST_ARCH)
type Callback = JSVal
foreign import javascript "wrapper" wasmMkCallback :: (JSVal -> IO ()) -> IO JSVal
#else
newtype Callback = Callback (Event -> IO ())
#endif

canvas :: HTMLElement -> Canvas
canvas (HTMLElement value) = Canvas value

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
$(browserJS ["jsbits/pixi.js"]
  [ ("newApplication", "halogen_pixi_new_application", Unsafe, [t| IO Application |])
  , ("initializeApplicationRaw", "halogen_pixi_initialize_application", Unsafe, [t| Application -> JSVal -> Canvas -> Callback -> IO () |])
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
  , ("svgPathRaw", "halogen_pixi_svg_path", Unsafe, [t| Application -> Object -> JSVal -> IO () |])
  , ("fill", "halogen_pixi_fill", Unsafe, [t| Object -> Int -> Double -> IO () |])
  , ("stroke", "halogen_pixi_stroke", Unsafe, [t| Object -> Int -> Double -> Double -> IO () |])
  , ("newText", "halogen_pixi_new_text", Unsafe, [t| Application -> IO Object |])
  , ("setSystemTextRaw", "halogen_pixi_set_system_text", Unsafe, [t| Object -> JSVal -> JSVal -> Double -> Int -> JSVal -> IO () |])
  , ("setAssetTextRaw", "halogen_pixi_set_asset_text", Unsafe, [t| Application -> Object -> JSVal -> JSVal -> JSVal -> Double -> Int -> JSVal -> IO () |])
  , ("newSprite", "halogen_pixi_new_sprite", Unsafe, [t| Application -> IO Object |])
  , ("setTextureRaw", "halogen_pixi_set_texture", Unsafe, [t| Application -> Object -> JSVal -> IO () |])
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
  , ("onRaw", "halogen_pixi_on", Unsafe, [t| Object -> JSVal -> Callback -> IO () |])
  , ("offRaw", "halogen_pixi_off", Unsafe, [t| Object -> JSVal -> Callback -> IO () |])
  , ("setEventModeRaw", "halogen_pixi_set_event_mode", Unsafe, [t| Object -> JSVal -> IO () |])
  , ("setCursorRaw", "halogen_pixi_set_cursor", Unsafe, [t| Object -> JSVal -> IO () |])
  , ("setRectHitArea", "halogen_pixi_set_rect_hit_area", Unsafe, [t| Application -> Object -> Double -> Double -> Double -> Double -> IO () |])
  , ("setCircleHitArea", "halogen_pixi_set_circle_hit_area", Unsafe, [t| Application -> Object -> Double -> Double -> Double -> IO () |])
  , ("setPolygonHitAreaRaw", "halogen_pixi_set_polygon_hit_area", Unsafe, [t| Application -> Object -> JSVal -> IO () |])
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
#endif

#if defined(javascript_HOST_ARCH)
initializeApplication :: Application -> Text -> Canvas -> Callback -> IO ()
initializeApplication application url = initializeApplicationRaw application (toJSString $ toS url)
setSystemText :: Object -> Text -> Text -> Double -> Int -> Text -> IO ()
setSystemText object value family size color align = setSystemTextRaw object (toJSString $ toS value) (toJSString $ toS family) size color (toJSString $ toS align)
setAssetText :: Application -> Object -> Text -> Text -> Text -> Double -> Int -> Text -> IO ()
setAssetText application object value family source size color align = setAssetTextRaw application object (toJSString $ toS value) (toJSString $ toS family) (toJSString $ toS source) size color (toJSString $ toS align)
setTexture :: Application -> Object -> Text -> IO ()
setTexture application object asset = setTextureRaw application object (toJSString $ toS asset)
svgPath :: Application -> Object -> Text -> IO ()
svgPath application object commands = svgPathRaw application object (toJSString $ toS commands)
parentOf :: Object -> IO (Maybe Object)
parentOf object = fmap Object . nullableToMaybe <$> parentOfRaw object
addListener :: Object -> Text -> Callback -> IO ()
addListener object eventType = onRaw object (toJSString $ toS eventType)
removeListener :: Object -> Text -> Callback -> IO ()
removeListener object eventType = offRaw object (toJSString $ toS eventType)
setEventMode :: Object -> Text -> IO ()
setEventMode object mode = setEventModeRaw object (toJSString $ toS mode)
setCursor :: Object -> Text -> IO ()
setCursor object cursor = setCursorRaw object (toJSString $ toS cursor)
setPolygonHitArea :: Application -> Object -> [Double] -> IO ()
setPolygonHitArea application object coordinates = setPolygonHitAreaRaw application object (toJSString (toS (polygonText coordinates)))
-- | A polygon's coordinates as Pixi's Polygon takes them, flat: "x,y,x,y,…".
polygonText :: [Double] -> Text
polygonText = mconcat . intersperse "," . map show
mkCallback :: (Event -> IO ()) -> IO Callback
mkCallback handler = JS.syncCallback1 JS.ContinueAsync (\event -> snapshot event >>= handler . Event)
freeCallback :: Callback -> IO ()
freeCallback = JS.releaseCallback

#elif defined(wasm32_HOST_ARCH)

initializeApplication :: Application -> Text -> Canvas -> Callback -> IO ()
initializeApplication application url = initializeApplicationRaw application (case toJSString (toS url) of JSString value -> value)
textValue :: Text -> JSVal
textValue value = case toJSString (toS value) of JSString result -> result
setSystemText :: Object -> Text -> Text -> Double -> Int -> Text -> IO ()
setSystemText object value family size color align = setSystemTextRaw object (textValue value) (textValue family) size color (textValue align)
setAssetText :: Application -> Object -> Text -> Text -> Text -> Double -> Int -> Text -> IO ()
setAssetText application object value family source size color align = setAssetTextRaw application object (textValue value) (textValue family) (textValue source) size color (textValue align)
setTexture :: Application -> Object -> Text -> IO ()
setTexture application object asset = setTextureRaw application object (textValue asset)
svgPath :: Application -> Object -> Text -> IO ()
svgPath application object commands = svgPathRaw application object (textValue commands)
parentOf :: Object -> IO (Maybe Object)
parentOf object = fmap Object . nullableToMaybe <$> parentOfRaw object
addListener :: Object -> Text -> Callback -> IO ()
addListener object eventType = onRaw object (textValue eventType)
removeListener :: Object -> Text -> Callback -> IO ()
removeListener object eventType = offRaw object (textValue eventType)
setEventMode :: Object -> Text -> IO ()
setEventMode object mode = setEventModeRaw object (textValue mode)
setCursor :: Object -> Text -> IO ()
setCursor object cursor = setCursorRaw object (textValue cursor)
setPolygonHitArea :: Application -> Object -> [Double] -> IO ()
setPolygonHitArea application object coordinates = setPolygonHitAreaRaw application object (textValue (polygonText coordinates))
-- | A polygon's coordinates as Pixi's Polygon takes them, flat: "x,y,x,y,…".
polygonText :: [Double] -> Text
polygonText = mconcat . intersperse "," . map show
mkCallback :: (Event -> IO ()) -> IO Callback
mkCallback handler = wasmMkCallback (\event -> snapshot event >>= handler . Event)
freeCallback :: Callback -> IO ()
freeCallback = freeJSVal

#else
-- The native backend is an inert stub: there is no JS engine to talk to, so
-- every operation ignores its arguments. The handle types are therefore filled
-- with opaque 'toForeign ()' placeholders that are only ever passed around,
-- never read back -- do not 'unsafeFromForeign' them.
initializeApplication :: Application -> Text -> Canvas -> Callback -> IO ()
initializeApplication _ _ _ (Callback done) = done (Event (toForeign ()))
newApplication :: IO Application
newApplication = pure (Application (toForeign ()))
applicationCreated, applicationReady :: Application -> IO Bool
applicationCreated _ = pure False
applicationReady _ = pure False
destroyApplication :: Application -> IO ()
destroyApplication _ = pure ()
newContainer, newGraphics :: Application -> IO Object
newContainer _ = pure (Object (toForeign ()))
newGraphics _ = pure (Object (toForeign ()))
newText :: Application -> IO Object
newText _ = pure (Object (toForeign ()))
addToStage :: Application -> Object -> IO ()
addToStage _ _ = pure ()
addChild, removeChild :: Object -> Object -> IO ()
addChild _ _ = pure ()
removeChild _ _ = pure ()
parentOf :: Object -> IO (Maybe Object)
parentOf _ = pure Nothing
setChildIndex :: Object -> Object -> Int -> IO ()
setChildIndex _ _ _ = pure ()
destroyObject, clearGraphics :: Object -> IO ()
destroyObject _ = pure ()
clearGraphics _ = pure ()
moveTo, lineTo :: Object -> Double -> Double -> IO ()
moveTo _ _ _ = pure ()
lineTo _ _ _ = pure ()
rect :: Object -> Double -> Double -> Double -> Double -> IO ()
rect _ _ _ _ _ = pure ()
circle :: Object -> Double -> Double -> Double -> IO ()
circle _ _ _ _ = pure ()
ellipse, quadraticCurveTo :: Object -> Double -> Double -> Double -> Double -> IO ()
ellipse _ _ _ _ _ = pure ()
quadraticCurveTo _ _ _ _ _ = pure ()
bezierCurveTo :: Object -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
bezierCurveTo _ _ _ _ _ _ _ = pure ()
arc :: Object -> Double -> Double -> Double -> Double -> Double -> Bool -> IO ()
arc _ _ _ _ _ _ _ = pure ()
fill :: Object -> Int -> Double -> IO ()
fill _ _ _ = pure ()
stroke :: Object -> Int -> Double -> Double -> IO ()
stroke _ _ _ _ = pure ()
setSystemText :: Object -> Text -> Text -> Double -> Int -> Text -> IO ()
setSystemText _ _ _ _ _ _ = pure ()
setAssetText :: Application -> Object -> Text -> Text -> Text -> Double -> Int -> Text -> IO ()
setAssetText _ _ _ _ _ _ _ _ = pure ()
newSprite :: Application -> IO Object
newSprite _ = pure (Object (toForeign ()))
setTexture :: Application -> Object -> Text -> IO ()
setTexture _ _ _ = pure ()
svgPath :: Application -> Object -> Text -> IO ()
svgPath _ _ _ = pure ()
clearText :: Object -> IO ()
clearText _ = pure ()
clearTexture :: Application -> Object -> IO ()
clearTexture _ _ = pure ()
centerAnchor :: Object -> IO ()
centerAnchor _ = pure ()
setPosition, setScale :: Object -> Double -> Double -> IO ()
setPosition _ _ _ = pure ()
setScale _ _ _ = pure ()
setRotation :: Object -> Double -> IO ()
setRotation _ _ = pure ()
setSize :: Object -> Double -> Double -> IO ()
setSize _ _ _ = pure ()
setOutline :: Application -> Object -> Int -> Double -> Double -> Double -> IO ()
setOutline _ _ _ _ _ _ = pure ()
clearOutline, refreshOutline :: Object -> IO ()
clearOutline _ = pure ()
refreshOutline _ = pure ()
setCacheAsTexture :: Application -> Object -> Double -> IO ()
setCacheAsTexture _ _ _ = pure ()
clearCacheAsTexture :: Object -> IO ()
clearCacheAsTexture _ = pure ()
addListener, removeListener :: Object -> Text -> Callback -> IO ()
addListener _ _ _ = pure ()
removeListener _ _ _ = pure ()
setEventMode, setCursor :: Object -> Text -> IO ()
setEventMode _ _ = pure ()
setCursor _ _ = pure ()
setRectHitArea :: Application -> Object -> Double -> Double -> Double -> Double -> IO ()
setRectHitArea _ _ _ _ _ _ = pure ()
setCircleHitArea :: Application -> Object -> Double -> Double -> Double -> IO ()
setCircleHitArea _ _ _ _ _ = pure ()
setPolygonHitArea :: Application -> Object -> [Double] -> IO ()
setPolygonHitArea _ _ _ = pure ()
clearHitArea :: Object -> IO ()
clearHitArea _ = pure ()
enableStageEvents :: Application -> IO ()
enableStageEvents _ = pure ()
onPointerDown, onPointerMove, onPointerEnd :: Application -> Callback -> IO ()
onPointerDown _ _ = pure ()
onPointerMove _ _ = pure ()
onPointerEnd _ _ = pure ()
onWheel, removeWheel :: Canvas -> Callback -> IO ()
onWheel _ _ = pure ()
removeWheel _ _ = pure ()
pointerId :: Event -> Int
pointerId _ = 0
globalX, globalY, clientX, clientY, deltaY :: Event -> Double
globalX _ = 0
globalY _ = 0
clientX _ = 0
clientY _ = 0
deltaY _ = 0
localX, localY :: Object -> Event -> Double
localX _ _ = 0
localY _ _ = 0
eventButton :: Event -> Int
eventButton _ = 0
currentTarget :: Event -> Object
currentTarget _ = Object (toForeign ())
preventDefault :: Event -> IO ()
preventDefault _ = pure ()
canvasLeft, canvasTop, canvasWidth, canvasHeight :: Canvas -> IO Double
canvasLeft _ = pure 0
canvasTop _ = pure 0
canvasWidth _ = pure 0
canvasHeight _ = pure 0
screenWidth, screenHeight :: Application -> IO Double
screenWidth _ = pure 0
screenHeight _ = pure 0
mkCallback :: (Event -> IO ()) -> IO Callback
mkCallback = pure . Callback
freeCallback :: Callback -> IO ()
freeCallback _ = pure ()
scheduleTimeout :: Callback -> Int -> IO Timer
scheduleTimeout _ _ = pure (Timer (toForeign ()))
cancelTimeout :: Timer -> IO ()
cancelTimeout _ = pure ()
#endif
