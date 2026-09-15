{-# LANGUAGE CPP #-}

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
  , clearContainer
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
  , fill
  , stroke
  , newText
  , setSystemText
  , setAssetText
  , newSprite
  , setTexture
  , centerAnchor
  , setPosition
  , setScale
  , setRotation
  , setSize
  , onTap
  , enableStageEvents
  , onPointerDown
  , onPointerMove
  , onPointerEnd
  , onWheel
  , removeWheel
  , pointerId
  , globalX
  , globalY
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

import Data.Foreign (Foreign)
import Protolude
import Web.DOM.Internal.Types (HTMLElement (..))

#if defined(javascript_HOST_ARCH)
import GHC.JS.Foreign.Callback qualified as JS
import GHC.JS.Prim (JSVal, toJSString)
#elif defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim (JSString (..), JSVal, freeJSVal, toJSString)
#endif

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
data ApplicationTag
data ObjectTag

type Application = Foreign ApplicationTag
type Object = Foreign ObjectTag
type Event = JSVal
type Canvas = Foreign HTMLElement
type Timer = Foreign TimerTag
#else
data Application = Application
data Object = Object
data Event = Event
data Timer = Timer
type Canvas = Foreign HTMLElement
#endif

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
data TimerTag
#endif

#if defined(javascript_HOST_ARCH)
type Callback = JS.Callback (JSVal -> IO ())
#elif defined(wasm32_HOST_ARCH)
type Callback = JSVal
foreign import javascript "wrapper" wasmMkCallback :: (JSVal -> IO ()) -> IO JSVal
#else
newtype Callback = Callback (Event -> IO ())
#endif

canvas :: HTMLElement -> Canvas
canvas (HTMLElement value) = value

#if defined(javascript_HOST_ARCH)
foreign import javascript unsafe "halogen_pixi_new_application" newApplication :: IO Application
foreign import javascript unsafe "halogen_pixi_initialize_application" initializeApplicationRaw :: Application -> JSVal -> Canvas -> Callback -> IO ()
foreign import javascript unsafe "halogen_pixi_application_created" applicationCreated :: Application -> IO Bool
foreign import javascript unsafe "halogen_pixi_application_ready" applicationReady :: Application -> IO Bool
foreign import javascript unsafe "halogen_pixi_destroy_application" destroyApplication :: Application -> IO ()
foreign import javascript unsafe "halogen_pixi_new_container" newContainer :: Application -> IO Object
foreign import javascript unsafe "halogen_pixi_new_graphics" newGraphics :: Application -> IO Object
foreign import javascript unsafe "halogen_pixi_add_to_stage" addToStage :: Application -> Object -> IO ()
foreign import javascript unsafe "halogen_pixi_add_child" addChild :: Object -> Object -> IO ()
foreign import javascript unsafe "halogen_pixi_clear_container" clearContainer :: Object -> IO ()
foreign import javascript unsafe "halogen_pixi_set_child_index" setChildIndex :: Object -> Object -> Int -> IO ()
foreign import javascript unsafe "halogen_pixi_destroy_object" destroyObject :: Object -> IO ()
foreign import javascript unsafe "halogen_pixi_clear_graphics" clearGraphics :: Object -> IO ()
foreign import javascript unsafe "halogen_pixi_move_to" moveTo :: Object -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_line_to" lineTo :: Object -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_rect" rect :: Object -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_circle" circle :: Object -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_ellipse" ellipse :: Object -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_quadratic_curve_to" quadraticCurveTo :: Object -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_bezier_curve_to" bezierCurveTo :: Object -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_arc" arc :: Object -> Double -> Double -> Double -> Double -> Double -> Bool -> IO ()
foreign import javascript unsafe "halogen_pixi_fill" fill :: Object -> Int -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_stroke" stroke :: Object -> Int -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_new_text" newText :: Application -> IO Object
foreign import javascript unsafe "halogen_pixi_set_system_text" setSystemTextRaw :: Object -> JSVal -> Double -> Double -> JSVal -> Double -> Int -> JSVal -> IO ()
foreign import javascript unsafe "halogen_pixi_set_asset_text" setAssetTextRaw :: Application -> Object -> JSVal -> Double -> Double -> JSVal -> JSVal -> Double -> Int -> JSVal -> IO ()
foreign import javascript unsafe "halogen_pixi_new_sprite" newSprite :: Application -> IO Object
foreign import javascript unsafe "halogen_pixi_set_texture" setTextureRaw :: Application -> Object -> JSVal -> IO ()
foreign import javascript unsafe "halogen_pixi_center_anchor" centerAnchor :: Object -> IO ()
foreign import javascript unsafe "halogen_pixi_set_position" setPosition :: Object -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_set_scale" setScale :: Object -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_set_rotation" setRotation :: Object -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_set_size" setSize :: Object -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_on_tap" onTap :: Object -> Callback -> IO ()
foreign import javascript unsafe "halogen_pixi_enable_stage_events" enableStageEvents :: Application -> IO ()
foreign import javascript unsafe "halogen_pixi_on_pointer_down" onPointerDown :: Application -> Callback -> IO ()
foreign import javascript unsafe "halogen_pixi_on_pointer_move" onPointerMove :: Application -> Callback -> IO ()
foreign import javascript unsafe "halogen_pixi_on_pointer_end" onPointerEnd :: Application -> Callback -> IO ()
foreign import javascript unsafe "halogen_pixi_on_wheel" onWheel :: Canvas -> Callback -> IO ()
foreign import javascript unsafe "halogen_pixi_remove_wheel" removeWheel :: Canvas -> Callback -> IO ()
foreign import javascript unsafe "halogen_pixi_pointer_id" pointerId :: Event -> IO Int
foreign import javascript unsafe "halogen_pixi_global_x" globalX :: Event -> IO Double
foreign import javascript unsafe "halogen_pixi_global_y" globalY :: Event -> IO Double
foreign import javascript unsafe "halogen_pixi_prevent_default" preventDefault :: Event -> IO ()
foreign import javascript unsafe "halogen_pixi_client_x" clientX :: Event -> IO Double
foreign import javascript unsafe "halogen_pixi_client_y" clientY :: Event -> IO Double
foreign import javascript unsafe "halogen_pixi_delta_y" deltaY :: Event -> IO Double
foreign import javascript unsafe "halogen_pixi_canvas_left" canvasLeft :: Canvas -> IO Double
foreign import javascript unsafe "halogen_pixi_canvas_top" canvasTop :: Canvas -> IO Double
foreign import javascript unsafe "halogen_pixi_canvas_width" canvasWidth :: Canvas -> IO Double
foreign import javascript unsafe "halogen_pixi_canvas_height" canvasHeight :: Canvas -> IO Double
foreign import javascript unsafe "halogen_pixi_screen_width" screenWidth :: Application -> IO Double
foreign import javascript unsafe "halogen_pixi_screen_height" screenHeight :: Application -> IO Double
foreign import javascript unsafe "halogen_pixi_schedule_timeout" scheduleTimeout :: Callback -> Int -> IO Timer
foreign import javascript unsafe "halogen_pixi_cancel_timeout" cancelTimeout :: Timer -> IO ()

initializeApplication :: Application -> Text -> Canvas -> Callback -> IO ()
initializeApplication application url = initializeApplicationRaw application (toJSString $ toS url)
setSystemText :: Object -> Text -> Double -> Double -> Text -> Double -> Int -> Text -> IO ()
setSystemText object value x y family size color align = setSystemTextRaw object (toJSString $ toS value) x y (toJSString $ toS family) size color (toJSString $ toS align)
setAssetText :: Application -> Object -> Text -> Double -> Double -> Text -> Text -> Double -> Int -> Text -> IO ()
setAssetText application object value x y family source size color align = setAssetTextRaw application object (toJSString $ toS value) x y (toJSString $ toS family) (toJSString $ toS source) size color (toJSString $ toS align)
setTexture :: Application -> Object -> Text -> IO ()
setTexture application object asset = setTextureRaw application object (toJSString $ toS asset)
mkCallback :: (Event -> IO ()) -> IO Callback
mkCallback = JS.asyncCallback1
freeCallback :: Callback -> IO ()
freeCallback = JS.releaseCallback

#elif defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "({pixi:null,app:null,ready:false})" newApplication :: IO Application
foreign import javascript unsafe "import($2).then(pixi=>{$1.pixi=pixi;$1.app=new pixi.Application();return $1.app.init({canvas:$3,resizeTo:$3.parentElement,preference:'webgl',antialias:true,autoDensity:true,resolution:Math.min(globalThis.devicePixelRatio || 1,2),backgroundColor:0x111827})}).then(()=>{$1.ready=true;$4(null)}).catch(error=>{console.error('Could not load or initialize PixiJS',error);$4(null)})" initializeApplicationRaw :: Application -> JSVal -> Canvas -> Callback -> IO ()
foreign import javascript unsafe "$1.app!==null" applicationCreated :: Application -> IO Bool
foreign import javascript unsafe "$1.ready" applicationReady :: Application -> IO Bool
foreign import javascript unsafe "$1.app.destroy(false,{children:true,texture:false,textureSource:false})" destroyApplication :: Application -> IO ()
foreign import javascript unsafe "new $1.pixi.Container()" newContainer :: Application -> IO Object
foreign import javascript unsafe "new $1.pixi.Graphics()" newGraphics :: Application -> IO Object
foreign import javascript unsafe "$1.app.stage.addChild($2)" addToStage :: Application -> Object -> IO ()
foreign import javascript unsafe "$1.addChild($2)" addChild :: Object -> Object -> IO ()
foreign import javascript unsafe "$1.removeChildren().forEach(child=>child.destroy({children:true,texture:false,textureSource:false}))" clearContainer :: Object -> IO ()
foreign import javascript unsafe "$1.setChildIndex($2,$3)" setChildIndex :: Object -> Object -> Int -> IO ()
foreign import javascript unsafe "$1.destroy({children:true,texture:false,textureSource:false})" destroyObject :: Object -> IO ()
foreign import javascript unsafe "$1.clear()" clearGraphics :: Object -> IO ()
foreign import javascript unsafe "$1.moveTo($2,$3)" moveTo :: Object -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.lineTo($2,$3)" lineTo :: Object -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.rect($2,$3,$4,$5)" rect :: Object -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.circle($2,$3,$4)" circle :: Object -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.ellipse($2,$3,$4,$5)" ellipse :: Object -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.quadraticCurveTo($2,$3,$4,$5)" quadraticCurveTo :: Object -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.bezierCurveTo($2,$3,$4,$5,$6,$7)" bezierCurveTo :: Object -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.arc($2,$3,$4,$5,$6,$7)" arc :: Object -> Double -> Double -> Double -> Double -> Double -> Bool -> IO ()
foreign import javascript unsafe "$1.fill({color:$2,alpha:$3})" fill :: Object -> Int -> Double -> IO ()
foreign import javascript unsafe "$1.stroke({color:$2,width:$3,alpha:$4})" stroke :: Object -> Int -> Double -> Double -> IO ()
foreign import javascript unsafe "new $1.pixi.Text({text:'',style:{}})" newText :: Application -> IO Object
foreign import javascript unsafe "$1.__halogenFontRequest=null;$1.text=$2;$1.position.set($3,$4);$1.style={fontFamily:$5,fontSize:$6,fill:$7,align:$8}" setSystemTextRaw :: Object -> JSVal -> Double -> Double -> JSVal -> Double -> Int -> JSVal -> IO ()
foreign import javascript unsafe "const request={};$2.__halogenFontRequest=request;$2.text=$3;$2.position.set($4,$5);$2.style={fontFamily:$6,fontSize:$8,fill:$9,align:$10};$1.pixi.Assets.load({src:$7,data:{family:$6}}).then(()=>{if(!$2.destroyed&&$2.__halogenFontRequest===request)$2.style={fontFamily:$6,fontSize:$8,fill:$9,align:$10}}).catch(error=>console.error('Could not load PixiJS font',$7,error))" setAssetTextRaw :: Application -> Object -> JSVal -> Double -> Double -> JSVal -> JSVal -> Double -> Int -> JSVal -> IO ()
foreign import javascript unsafe "new $1.pixi.Sprite($1.pixi.Texture.EMPTY)" newSprite :: Application -> IO Object
foreign import javascript unsafe "$2.__halogenAsset=$3;$1.pixi.Assets.load($3).then(texture=>{if(!$2.destroyed&&$2.__halogenAsset===$3)$2.texture=texture}).catch(error=>console.error('Could not load PixiJS texture',$3,error))" setTextureRaw :: Application -> Object -> JSVal -> IO ()
foreign import javascript unsafe "$1.anchor.set(0.5)" centerAnchor :: Object -> IO ()
foreign import javascript unsafe "$1.position.set($2,$3)" setPosition :: Object -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.scale.set($2,$3)" setScale :: Object -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.rotation=$2" setRotation :: Object -> Double -> IO ()
foreign import javascript unsafe "$1.width=$2;$1.height=$3" setSize :: Object -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.eventMode='static';$1.cursor='pointer';$1.on('pointertap',$2)" onTap :: Object -> Callback -> IO ()
foreign import javascript unsafe "$1.app.stage.eventMode='static';$1.app.stage.hitArea=$1.app.screen" enableStageEvents :: Application -> IO ()
foreign import javascript unsafe "$1.app.stage.on('pointerdown',$2)" onPointerDown :: Application -> Callback -> IO ()
foreign import javascript unsafe "$1.app.stage.on('globalpointermove',$2)" onPointerMove :: Application -> Callback -> IO ()
foreign import javascript unsafe "$1.app.stage.on('pointerup',$2);$1.app.stage.on('pointerupoutside',$2);$1.app.stage.on('pointercancel',$2)" onPointerEnd :: Application -> Callback -> IO ()
foreign import javascript unsafe "$1.addEventListener('wheel',$2,{passive:false})" onWheel :: Canvas -> Callback -> IO ()
foreign import javascript unsafe "$1.removeEventListener('wheel',$2)" removeWheel :: Canvas -> Callback -> IO ()
foreign import javascript unsafe "$1.pointerId" pointerId :: Event -> IO Int
foreign import javascript unsafe "$1.global.x" globalX :: Event -> IO Double
foreign import javascript unsafe "$1.global.y" globalY :: Event -> IO Double
foreign import javascript unsafe "$1.preventDefault()" preventDefault :: Event -> IO ()
foreign import javascript unsafe "$1.clientX" clientX :: Event -> IO Double
foreign import javascript unsafe "$1.clientY" clientY :: Event -> IO Double
foreign import javascript unsafe "$1.deltaY" deltaY :: Event -> IO Double
foreign import javascript unsafe "$1.getBoundingClientRect().left" canvasLeft :: Canvas -> IO Double
foreign import javascript unsafe "$1.getBoundingClientRect().top" canvasTop :: Canvas -> IO Double
foreign import javascript unsafe "$1.getBoundingClientRect().width" canvasWidth :: Canvas -> IO Double
foreign import javascript unsafe "$1.getBoundingClientRect().height" canvasHeight :: Canvas -> IO Double
foreign import javascript unsafe "$1.app.screen.width" screenWidth :: Application -> IO Double
foreign import javascript unsafe "$1.app.screen.height" screenHeight :: Application -> IO Double
foreign import javascript unsafe "setTimeout($1,$2)" scheduleTimeout :: Callback -> Int -> IO Timer
foreign import javascript unsafe "clearTimeout($1)" cancelTimeout :: Timer -> IO ()

initializeApplication :: Application -> Text -> Canvas -> Callback -> IO ()
initializeApplication application url = initializeApplicationRaw application (case toJSString (toS url) of JSString value -> value)
textValue :: Text -> JSVal
textValue value = case toJSString (toS value) of JSString result -> result
setSystemText :: Object -> Text -> Double -> Double -> Text -> Double -> Int -> Text -> IO ()
setSystemText object value x y family size color align = setSystemTextRaw object (textValue value) x y (textValue family) size color (textValue align)
setAssetText :: Application -> Object -> Text -> Double -> Double -> Text -> Text -> Double -> Int -> Text -> IO ()
setAssetText application object value x y family source size color align = setAssetTextRaw application object (textValue value) x y (textValue family) (textValue source) size color (textValue align)
setTexture :: Application -> Object -> Text -> IO ()
setTexture application object asset = setTextureRaw application object (textValue asset)
mkCallback :: (Event -> IO ()) -> IO Callback
mkCallback = wasmMkCallback
freeCallback :: Callback -> IO ()
freeCallback = freeJSVal

#else
initializeApplication :: Application -> Text -> Canvas -> Callback -> IO ()
initializeApplication _ _ _ (Callback done) = done Event
newApplication :: IO Application
newApplication = pure Application
applicationCreated, applicationReady :: Application -> IO Bool
applicationCreated _ = pure False
applicationReady _ = pure False
destroyApplication :: Application -> IO ()
destroyApplication _ = pure ()
newContainer, newGraphics :: Application -> IO Object
newContainer _ = pure Object
newGraphics _ = pure Object
newText :: Application -> IO Object
newText _ = pure Object
addToStage :: Application -> Object -> IO ()
addToStage _ _ = pure ()
addChild :: Object -> Object -> IO ()
addChild _ _ = pure ()
clearContainer :: Object -> IO ()
clearContainer _ = pure ()
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
setSystemText :: Object -> Text -> Double -> Double -> Text -> Double -> Int -> Text -> IO ()
setSystemText _ _ _ _ _ _ _ _ = pure ()
setAssetText :: Application -> Object -> Text -> Double -> Double -> Text -> Text -> Double -> Int -> Text -> IO ()
setAssetText _ _ _ _ _ _ _ _ _ _ = pure ()
newSprite :: Application -> IO Object
newSprite _ = pure Object
setTexture :: Application -> Object -> Text -> IO ()
setTexture _ _ _ = pure ()
centerAnchor :: Object -> IO ()
centerAnchor _ = pure ()
setPosition, setScale :: Object -> Double -> Double -> IO ()
setPosition _ _ _ = pure ()
setScale _ _ _ = pure ()
setRotation :: Object -> Double -> IO ()
setRotation _ _ = pure ()
setSize :: Object -> Double -> Double -> IO ()
setSize _ _ _ = pure ()
onTap :: Object -> Callback -> IO ()
onTap _ _ = pure ()
enableStageEvents :: Application -> IO ()
enableStageEvents _ = pure ()
onPointerDown, onPointerMove, onPointerEnd :: Application -> Callback -> IO ()
onPointerDown _ _ = pure ()
onPointerMove _ _ = pure ()
onPointerEnd _ _ = pure ()
onWheel, removeWheel :: Canvas -> Callback -> IO ()
onWheel _ _ = pure ()
removeWheel _ _ = pure ()
pointerId :: Event -> IO Int
pointerId _ = pure 0
globalX, globalY, clientX, clientY, deltaY :: Event -> IO Double
globalX _ = pure 0
globalY _ = pure 0
preventDefault :: Event -> IO ()
preventDefault _ = pure ()
clientX _ = pure 0
clientY _ = pure 0
deltaY _ = pure 0
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
scheduleTimeout _ _ = pure Timer
cancelTimeout :: Timer -> IO ()
cancelTimeout _ = pure ()
#endif
