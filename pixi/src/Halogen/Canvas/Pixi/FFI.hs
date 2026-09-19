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
  , addListener
  , removeListener
  , setEventMode
  , setCursor
  , setRectHitArea
  , setCircleHitArea
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
foreign import javascript unsafe "halogen_pixi_remove_child" removeChild :: Object -> Object -> IO ()
foreign import javascript unsafe "halogen_pixi_parent_of" parentOfRaw :: Object -> IO (Nullable Object)
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
foreign import javascript unsafe "halogen_pixi_svg_path" svgPathRaw :: Application -> Object -> JSVal -> IO ()
foreign import javascript unsafe "halogen_pixi_fill" fill :: Object -> Int -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_stroke" stroke :: Object -> Int -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_new_text" newText :: Application -> IO Object
foreign import javascript unsafe "halogen_pixi_set_system_text" setSystemTextRaw :: Object -> JSVal -> JSVal -> Double -> Int -> JSVal -> IO ()
foreign import javascript unsafe "halogen_pixi_set_asset_text" setAssetTextRaw :: Application -> Object -> JSVal -> JSVal -> JSVal -> Double -> Int -> JSVal -> IO ()
foreign import javascript unsafe "halogen_pixi_new_sprite" newSprite :: Application -> IO Object
foreign import javascript unsafe "halogen_pixi_set_texture" setTextureRaw :: Application -> Object -> JSVal -> IO ()
foreign import javascript unsafe "halogen_pixi_clear_text" clearText :: Object -> IO ()
foreign import javascript unsafe "halogen_pixi_clear_texture" clearTexture :: Application -> Object -> IO ()
foreign import javascript unsafe "halogen_pixi_center_anchor" centerAnchor :: Object -> IO ()
foreign import javascript unsafe "halogen_pixi_set_position" setPosition :: Object -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_set_scale" setScale :: Object -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_set_rotation" setRotation :: Object -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_set_size" setSize :: Object -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_set_outline" setOutline :: Application -> Object -> Int -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_clear_outline" clearOutline :: Object -> IO ()
foreign import javascript unsafe "halogen_pixi_refresh_outline" refreshOutline :: Object -> IO ()
foreign import javascript unsafe "halogen_pixi_on" onRaw :: Object -> JSVal -> Callback -> IO ()
foreign import javascript unsafe "halogen_pixi_off" offRaw :: Object -> JSVal -> Callback -> IO ()
foreign import javascript unsafe "halogen_pixi_set_event_mode" setEventModeRaw :: Object -> JSVal -> IO ()
foreign import javascript unsafe "halogen_pixi_set_cursor" setCursorRaw :: Object -> JSVal -> IO ()
foreign import javascript unsafe "halogen_pixi_set_rect_hit_area" setRectHitArea :: Application -> Object -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_set_circle_hit_area" setCircleHitArea :: Application -> Object -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "halogen_pixi_clear_hit_area" clearHitArea :: Object -> IO ()
foreign import javascript unsafe "halogen_pixi_enable_stage_events" enableStageEvents :: Application -> IO ()
foreign import javascript unsafe "halogen_pixi_on_pointer_down" onPointerDown :: Application -> Callback -> IO ()
foreign import javascript unsafe "halogen_pixi_on_pointer_move" onPointerMove :: Application -> Callback -> IO ()
foreign import javascript unsafe "halogen_pixi_on_pointer_end" onPointerEnd :: Application -> Callback -> IO ()
foreign import javascript unsafe "halogen_pixi_on_wheel" onWheel :: Canvas -> Callback -> IO ()
foreign import javascript unsafe "halogen_pixi_remove_wheel" removeWheel :: Canvas -> Callback -> IO ()
foreign import javascript unsafe "halogen_pixi_pointer_id" pointerId :: Event -> Int
foreign import javascript unsafe "halogen_pixi_global_x" globalX :: Event -> Double
foreign import javascript unsafe "halogen_pixi_global_y" globalY :: Event -> Double
foreign import javascript unsafe "halogen_pixi_local_x" localX :: Object -> Event -> Double
foreign import javascript unsafe "halogen_pixi_local_y" localY :: Object -> Event -> Double
foreign import javascript unsafe "halogen_pixi_event_button" eventButton :: Event -> Int
foreign import javascript unsafe "halogen_pixi_current_target" currentTarget :: Event -> Object
foreign import javascript unsafe "halogen_pixi_prevent_default" preventDefault :: Event -> IO ()
foreign import javascript unsafe "halogen_pixi_client_x" clientX :: Event -> Double
foreign import javascript unsafe "halogen_pixi_client_y" clientY :: Event -> Double
foreign import javascript unsafe "halogen_pixi_delta_y" deltaY :: Event -> Double
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
mkCallback :: (Event -> IO ()) -> IO Callback
mkCallback handler = JS.asyncCallback1 (handler . Event)
freeCallback :: Callback -> IO ()
freeCallback = JS.releaseCallback

#elif defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "({pixi:null,app:null,ready:false})" newApplicationRaw :: IO Application

-- | Install the two helpers the inline snippets in this branch call.
--
-- The javascript backend has jsbits for these; wasm has no such file, and
-- neither of them fits in an expression — so they are installed once, on the
-- first thing any Pixi work has to do.
--
-- @||=@ rather than @??=@: the snippet is emitted through a C string, where
-- @??=@ is a trigraph for @#@.
foreign import javascript unsafe "globalThis.__halogenPixi ||= {resize:object=>{const size=object.__halogenSize;const scale=object.__halogenScale||{x:1,y:1};const texture=size?object.texture:null;const naturalWidth=texture?texture.orig.width:0;const naturalHeight=texture?texture.orig.height:0;object.scale.set(naturalWidth?(size.width/naturalWidth)*scale.x:scale.x,naturalHeight?(size.height/naturalHeight)*scale.y:scale.y)},outline:object=>{const spec=object.__halogenOutline;if(!spec)return;let graphics=object.__halogenOutlineGraphics;if(!graphics){graphics=new spec.holder.pixi.Graphics();graphics.eventMode='none';object.__halogenOutlineGraphics=graphics}if(graphics.parent)graphics.parent.removeChild(graphics);const bounds=object.getLocalBounds();const scaleX=object.scale.x||1;const scaleY=object.scale.y||1;graphics.clear();graphics.scale.set(1/scaleX,1/scaleY);graphics.rect(bounds.x*scaleX-spec.padding,bounds.y*scaleY-spec.padding,bounds.width*scaleX+spec.padding*2,bounds.height*scaleY+spec.padding*2);graphics.stroke({color:spec.color,width:spec.width,alpha:spec.alpha});object.addChild(graphics)}}" installHelpers :: IO ()
foreign import javascript unsafe "import($2).then(pixi=>{$1.pixi=pixi;$1.app=new pixi.Application();return $1.app.init({canvas:$3,resizeTo:$3.parentElement,preference:'webgl',antialias:true,autoDensity:true,resolution:Math.min(globalThis.devicePixelRatio || 1,2),backgroundColor:0x111827})}).then(()=>{$1.ready=true;$4(null)}).catch(error=>{console.error('Could not load or initialize PixiJS',error);$4(null)})" initializeApplicationRaw :: Application -> JSVal -> Canvas -> Callback -> IO ()
foreign import javascript unsafe "$1.app!==null" applicationCreated :: Application -> IO Bool
foreign import javascript unsafe "$1.ready" applicationReady :: Application -> IO Bool
foreign import javascript unsafe "$1.app.destroy(false,{children:true,texture:false,textureSource:false})" destroyApplication :: Application -> IO ()
foreign import javascript unsafe "new $1.pixi.Container()" newContainer :: Application -> IO Object
foreign import javascript unsafe "new $1.pixi.Graphics()" newGraphics :: Application -> IO Object
foreign import javascript unsafe "$1.app.stage.addChild($2)" addToStage :: Application -> Object -> IO ()
foreign import javascript unsafe "$1.addChild($2)" addChild :: Object -> Object -> IO ()
foreign import javascript unsafe "$1.removeChild($2)" removeChild :: Object -> Object -> IO ()
foreign import javascript unsafe "$1.parent ?? null" parentOfRaw :: Object -> IO (Nullable Object)
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
foreign import javascript unsafe "$2.path(new $1.pixi.GraphicsPath($3))" svgPathRaw :: Application -> Object -> JSVal -> IO ()
foreign import javascript unsafe "$1.fill({color:$2,alpha:$3})" fill :: Object -> Int -> Double -> IO ()
foreign import javascript unsafe "$1.stroke({color:$2,width:$3,alpha:$4})" stroke :: Object -> Int -> Double -> Double -> IO ()
foreign import javascript unsafe "new $1.pixi.Text({text:'',style:{}})" newText :: Application -> IO Object
foreign import javascript unsafe "$1.__halogenFontRequest=null;$1.text=$2;$1.style={fontFamily:$3,fontSize:$4,fill:$5,align:$6}" setSystemTextRaw :: Object -> JSVal -> JSVal -> Double -> Int -> JSVal -> IO ()
foreign import javascript unsafe "const request={};$2.__halogenFontRequest=request;$2.text=$3;$2.style={fontFamily:$4,fontSize:$6,fill:$7,align:$8};$1.pixi.Assets.load({src:$5,data:{family:$4}}).then(()=>{if($2.destroyed||$2.__halogenFontRequest!==request)return;$2.style={fontFamily:$4,fontSize:$6,fill:$7,align:$8};globalThis.__halogenPixi.outline($2)}).catch(error=>console.error('Could not load PixiJS font',$5,error))" setAssetTextRaw :: Application -> Object -> JSVal -> JSVal -> JSVal -> Double -> Int -> JSVal -> IO ()
foreign import javascript unsafe "new $1.pixi.Sprite($1.pixi.Texture.EMPTY)" newSprite :: Application -> IO Object
foreign import javascript unsafe "$1.__halogenFontRequest=null;$1.text=''" clearText :: Object -> IO ()
foreign import javascript unsafe "$2.__halogenAsset=null;$2.__halogenSize=null;$2.texture=$1.pixi.Texture.EMPTY;globalThis.__halogenPixi.resize($2)" clearTexture :: Application -> Object -> IO ()
foreign import javascript unsafe "$2.__halogenAsset=$3;$1.pixi.Assets.load($3).then(texture=>{if($2.destroyed||$2.__halogenAsset!==$3)return;$2.texture=texture;globalThis.__halogenPixi.resize($2);globalThis.__halogenPixi.outline($2)}).catch(error=>console.error('Could not load PixiJS texture',$3,error))" setTextureRaw :: Application -> Object -> JSVal -> IO ()
foreign import javascript unsafe "$1.anchor.set(0.5)" centerAnchor :: Object -> IO ()
foreign import javascript unsafe "$1.position.set($2,$3)" setPosition :: Object -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.__halogenScale={x:$2,y:$3};globalThis.__halogenPixi.resize($1)" setScale :: Object -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.rotation=$2" setRotation :: Object -> Double -> IO ()
foreign import javascript unsafe "$1.__halogenSize={width:$2,height:$3};globalThis.__halogenPixi.resize($1)" setSize :: Object -> Double -> Double -> IO ()
foreign import javascript unsafe "$2.__halogenOutline={holder:$1,color:$3,width:$4,alpha:$5,padding:$6};globalThis.__halogenPixi.outline($2)" setOutline :: Application -> Object -> Int -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.__halogenOutline=null;const graphics=$1.__halogenOutlineGraphics;$1.__halogenOutlineGraphics=null;if(graphics)graphics.destroy()" clearOutline :: Object -> IO ()
foreign import javascript unsafe "globalThis.__halogenPixi.outline($1)" refreshOutline :: Object -> IO ()
foreign import javascript unsafe "$1.on($2,$3)" onRaw :: Object -> JSVal -> Callback -> IO ()
foreign import javascript unsafe "$1.off($2,$3)" offRaw :: Object -> JSVal -> Callback -> IO ()
foreign import javascript unsafe "$1.eventMode=$2" setEventModeRaw :: Object -> JSVal -> IO ()
foreign import javascript unsafe "$1.cursor=$2" setCursorRaw :: Object -> JSVal -> IO ()
foreign import javascript unsafe "$2.hitArea=new $1.pixi.Rectangle($3,$4,$5,$6)" setRectHitArea :: Application -> Object -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "$2.hitArea=new $1.pixi.Circle($3,$4,$5)" setCircleHitArea :: Application -> Object -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.hitArea=null" clearHitArea :: Object -> IO ()
foreign import javascript unsafe "$1.app.stage.eventMode='static';$1.app.stage.hitArea=$1.app.screen" enableStageEvents :: Application -> IO ()
foreign import javascript unsafe "$1.app.stage.on('pointerdown',$2)" onPointerDown :: Application -> Callback -> IO ()
foreign import javascript unsafe "$1.app.stage.on('globalpointermove',$2)" onPointerMove :: Application -> Callback -> IO ()
foreign import javascript unsafe "$1.app.stage.on('pointerup',$2);$1.app.stage.on('pointerupoutside',$2);$1.app.stage.on('pointercancel',$2)" onPointerEnd :: Application -> Callback -> IO ()
foreign import javascript unsafe "$1.addEventListener('wheel',$2,{passive:false})" onWheel :: Canvas -> Callback -> IO ()
foreign import javascript unsafe "$1.removeEventListener('wheel',$2)" removeWheel :: Canvas -> Callback -> IO ()
foreign import javascript unsafe "$1.pointerId" pointerId :: Event -> Int
foreign import javascript unsafe "$1.global.x" globalX :: Event -> Double
foreign import javascript unsafe "$1.global.y" globalY :: Event -> Double
foreign import javascript unsafe "$2.getLocalPosition($1).x" localX :: Object -> Event -> Double
foreign import javascript unsafe "$2.getLocalPosition($1).y" localY :: Object -> Event -> Double
foreign import javascript unsafe "$1.button" eventButton :: Event -> Int
foreign import javascript unsafe "$1.currentTarget" currentTarget :: Event -> Object
foreign import javascript unsafe "$1.preventDefault()" preventDefault :: Event -> IO ()
foreign import javascript unsafe "$1.clientX" clientX :: Event -> Double
foreign import javascript unsafe "$1.clientY" clientY :: Event -> Double
foreign import javascript unsafe "$1.deltaY" deltaY :: Event -> Double
foreign import javascript unsafe "$1.getBoundingClientRect().left" canvasLeft :: Canvas -> IO Double
foreign import javascript unsafe "$1.getBoundingClientRect().top" canvasTop :: Canvas -> IO Double
foreign import javascript unsafe "$1.getBoundingClientRect().width" canvasWidth :: Canvas -> IO Double
foreign import javascript unsafe "$1.getBoundingClientRect().height" canvasHeight :: Canvas -> IO Double
foreign import javascript unsafe "$1.app.screen.width" screenWidth :: Application -> IO Double
foreign import javascript unsafe "$1.app.screen.height" screenHeight :: Application -> IO Double
foreign import javascript unsafe "setTimeout($1,$2)" scheduleTimeout :: Callback -> Int -> IO Timer
foreign import javascript unsafe "clearTimeout($1)" cancelTimeout :: Timer -> IO ()

newApplication :: IO Application
newApplication = installHelpers >> newApplicationRaw
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
mkCallback :: (Event -> IO ()) -> IO Callback
mkCallback handler = wasmMkCallback (handler . Event)
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
