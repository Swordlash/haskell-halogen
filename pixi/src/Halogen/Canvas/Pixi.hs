module Halogen.Canvas.Pixi
  ( Point (..)
  , Camera (..)
  , Interaction (..)
  , Transform (..)
  , Texture (..)
  , Font (..)
  , TextAlign (..)
  , TextStyle (..)
  , FillStyle (..)
  , StrokeStyle (..)
  , Config (..)
  , CanvasEvent (..)
  , Drawing
  , View
  , defaultCamera
  , defaultInteraction
  , defaultTransform
  , view
  , mapEvents
  , keyed
  , clickable
  , group
  , line
  , rectangle
  , circle
  , ellipse
  , quadraticBezier
  , bezier
  , arc
  , text
  , sprite
  , defaultConfig
  , rendererWith
  , componentWith
  , renderer
  , component
  )
where

import Halogen qualified as H
import Halogen.Canvas qualified as Canvas
import Halogen.Canvas.Pixi.FFI qualified as FFI
import Protolude hiding (group)
import Data.IORef

data Point = Point Double Double
  deriving stock (Eq, Show)

data Camera = Camera
  { focus :: Point
  , zoom :: Double
  }
  deriving stock (Eq, Show)

data Interaction = Interaction
  { pan :: Bool
  , zoomRange :: Maybe (Double, Double)
  }
  deriving stock (Eq, Show)

data Transform = Transform
  { position :: Point
  , scale :: Point
  , rotation :: Double
  }
  deriving stock (Eq, Show)

newtype Texture = Texture
  { asset :: Text
  }
  deriving stock (Eq, Ord, Show)

data FillStyle = FillStyle
  { fillColor :: Int
  , fillAlpha :: Double
  }
  deriving stock (Eq, Show)

data StrokeStyle = StrokeStyle
  { strokeColor :: Int
  , strokeWidth :: Double
  , strokeAlpha :: Double
  }
  deriving stock (Eq, Show)

data Font
  = SystemFont Text
  | AssetFont
      { family :: Text
      , source :: Text
      }
  deriving stock (Eq, Show)

data TextAlign = AlignLeft | AlignCenter | AlignRight
  deriving stock (Eq, Show)

data TextStyle = TextStyle
  { font :: Font
  , fontSize :: Double
  , textColor :: Int
  , textAlign :: TextAlign
  }
  deriving stock (Eq, Show)

-- | Where the renderer should load PixiJS from. The module must export the
-- PixiJS v8 API. Keeping this in the renderer configuration lets applications
-- use the zero-setup CDN default or point at a self-hosted/bundled module.
newtype Config = Config
  { moduleUrl :: Text
  }
  deriving stock (Eq, Show)

data CanvasEvent event
  = Fired event
  | CameraChanged Camera
  deriving stock (Eq, Show, Functor)

data Node event = Node
  { nodeKey :: Maybe Text
  , nodeEvent :: Maybe event
  , content :: NodeContent event
  }

data NodeContent event
  = Group Transform [Node event]
  | Line Point Point StrokeStyle
  | Rectangle Point Point (Maybe FillStyle) (Maybe StrokeStyle)
  | Circle Point Double (Maybe FillStyle) (Maybe StrokeStyle)
  | Ellipse Point Point (Maybe FillStyle) (Maybe StrokeStyle)
  | QuadraticBezier Point Point Point StrokeStyle
  | Bezier Point Point Point Point StrokeStyle
  | Arc Point Double Double Double Bool StrokeStyle
  | Label Point Text TextStyle
  | Sprite Point Point Texture

newtype Drawing event a = Drawing (a, [Node event])
  deriving stock (Functor)

instance Applicative (Drawing event) where
  pure a = Drawing (a, [])
  Drawing (f, left) <*> Drawing (a, right) = Drawing (f a, left <> right)

instance Monad (Drawing event) where
  Drawing (a, left) >>= next =
    let Drawing (b, right) = next a
     in Drawing (b, left <> right)

data View event = View
  { camera :: Camera
  , interaction :: Interaction
  , nodes :: [Node event]
  }

defaultCamera :: Camera
defaultCamera = Camera {focus = Point 0 0, zoom = 1}

defaultInteraction :: Interaction
defaultInteraction = Interaction {pan = True, zoomRange = Just (0.25, 3)}

defaultTransform :: Transform
defaultTransform = Transform {position = Point 0 0, scale = Point 1 1, rotation = 0}

view :: Camera -> Interaction -> Drawing event () -> View event
view camera interaction (Drawing (_, nodes)) = View {camera, interaction, nodes}

mapEvents :: (event -> event') -> View event -> View event'
mapEvents transform View {camera, interaction, nodes} =
  View {camera, interaction, nodes = map (mapNode transform) nodes}

mapNode :: (event -> event') -> Node event -> Node event'
mapNode transform Node {nodeKey, nodeEvent, content} = Node {nodeKey, nodeEvent = map transform nodeEvent, content = case content of
  Group groupTransform children -> Group groupTransform $ map (mapNode transform) children
  Line start end strokeStyle -> Line start end strokeStyle
  Rectangle position size fillStyle strokeStyle -> Rectangle position size fillStyle strokeStyle
  Circle position radius fillStyle strokeStyle -> Circle position radius fillStyle strokeStyle
  Ellipse position radius fillStyle strokeStyle -> Ellipse position radius fillStyle strokeStyle
  QuadraticBezier start control end strokeStyle -> QuadraticBezier start control end strokeStyle
  Bezier start control1 control2 end strokeStyle -> Bezier start control1 control2 end strokeStyle
  Arc position radius startAngle endAngle anticlockwise strokeStyle -> Arc position radius startAngle endAngle anticlockwise strokeStyle
  Label position value textStyle -> Label position value textStyle
  Sprite position size texture -> Sprite position size texture}

emitNode :: Node event -> Drawing event ()
emitNode node = Drawing ((), [node])

-- | Give a drawing a stable identity for reconciliation. A single emitted
-- node receives the key directly; a multi-node drawing is retained as a keyed
-- group.
keyed :: Text -> Drawing event a -> Drawing event a
keyed key (Drawing (result, nodes)) = Drawing (result, case nodes of
  [node] -> [node {nodeKey = Just key}]
  _ -> [Node {nodeKey = Just key, nodeEvent = Nothing, content = Group defaultTransform nodes}])

-- | Raise an event when the drawing is clicked. Like 'keyed', this applies
-- directly to one node and groups a multi-node drawing when necessary.
clickable :: event -> Drawing event a -> Drawing event a
clickable event (Drawing (result, nodes)) = Drawing (result, case nodes of
  [node] -> [node {nodeEvent = Just event}]
  _ -> [Node {nodeKey = Nothing, nodeEvent = Just event, content = Group defaultTransform nodes}])

group :: Transform -> Drawing event a -> Drawing event a
group transform (Drawing (result, nodes)) = Drawing (result, [Node {nodeKey = Nothing, nodeEvent = Nothing, content = Group transform nodes}])

line :: Point -> Point -> StrokeStyle -> Drawing event ()
line start end strokeStyle = emitNode $ Node Nothing Nothing $ Line start end strokeStyle

rectangle :: Point -> Point -> Maybe FillStyle -> Maybe StrokeStyle -> Drawing event ()
rectangle position size fillStyle strokeStyle = emitNode $ Node Nothing Nothing $ Rectangle position size fillStyle strokeStyle

circle :: Point -> Double -> Maybe FillStyle -> Maybe StrokeStyle -> Drawing event ()
circle position radius fillStyle strokeStyle = emitNode $ Node Nothing Nothing $ Circle position radius fillStyle strokeStyle

ellipse :: Point -> Point -> Maybe FillStyle -> Maybe StrokeStyle -> Drawing event ()
ellipse position radius fillStyle strokeStyle = emitNode $ Node Nothing Nothing $ Ellipse position radius fillStyle strokeStyle

quadraticBezier :: Point -> Point -> Point -> StrokeStyle -> Drawing event ()
quadraticBezier start control end strokeStyle = emitNode $ Node Nothing Nothing $ QuadraticBezier start control end strokeStyle

bezier :: Point -> Point -> Point -> Point -> StrokeStyle -> Drawing event ()
bezier start control1 control2 end strokeStyle = emitNode $ Node Nothing Nothing $ Bezier start control1 control2 end strokeStyle

arc :: Point -> Double -> Double -> Double -> Bool -> StrokeStyle -> Drawing event ()
arc position radius startAngle endAngle anticlockwise strokeStyle = emitNode $ Node Nothing Nothing $ Arc position radius startAngle endAngle anticlockwise strokeStyle

text :: Point -> Text -> TextStyle -> Drawing event ()
text position value textStyle = emitNode $ Node Nothing Nothing $ Label position value textStyle

sprite :: Point -> Point -> Texture -> Drawing event ()
sprite position size texture = emitNode $ Node Nothing Nothing $ Sprite position size texture

defaultConfig :: Config
defaultConfig = Config
  { moduleUrl = "https://cdn.jsdelivr.net/npm/pixi.js@8.20.1/dist/pixi.min.mjs"
  }

componentWith :: Config -> H.Component H.VoidF (View event) (CanvasEvent event) IO
componentWith = Canvas.component . rendererWith

component :: H.Component H.VoidF (View event) (CanvasEvent event) IO
component = componentWith defaultConfig

rendererWith :: Config -> Canvas.Renderer (View event) (CanvasEvent event)

renderer :: Canvas.Renderer (View event) (CanvasEvent event)
renderer = rendererWith defaultConfig

data Runtime event = Runtime
  { app :: FFI.Application
  , canvas :: FFI.Canvas
  , emit :: CanvasEvent event -> IO ()
  , pending :: IORef (Maybe (View event))
  , world :: IORef (Maybe FFI.Object)
  , mountedNodes :: IORef [MountedNode event]
  , camera :: IORef Camera
  , drag :: IORef (Maybe (Int, Double, Double, Bool))
  , callbacks :: IORef [FFI.Callback]
  , wheelCallback :: IORef (Maybe FFI.Callback)
  , cameraTimer :: IORef (Maybe FFI.Timer)
  , disposed :: IORef Bool
  , ready :: IORef Bool
  , cleaned :: IORef Bool
  }

data NodeKind = GroupKind | GraphicsKind | TextKind | SpriteKind
  deriving stock (Eq)

data VisualState
  = GroupVisual Transform
  | LineVisual Point Point StrokeStyle
  | RectangleVisual Point Point (Maybe FillStyle) (Maybe StrokeStyle)
  | CircleVisual Point Double (Maybe FillStyle) (Maybe StrokeStyle)
  | EllipseVisual Point Point (Maybe FillStyle) (Maybe StrokeStyle)
  | QuadraticBezierVisual Point Point Point StrokeStyle
  | BezierVisual Point Point Point Point StrokeStyle
  | ArcVisual Point Double Double Double Bool StrokeStyle
  | TextVisual Point Text TextStyle
  | SpriteVisual Point Point Texture
  deriving stock (Eq)

data MountedNode event = MountedNode
  { mountedIdentity :: NodeIdentity
  , kind :: NodeKind
  , visual :: Maybe VisualState
  , object :: FFI.Object
  , children :: [MountedNode event]
  , click :: Maybe (IORef (Maybe event), FFI.Callback)
  }

data NodeIdentity
  = ExplicitKey Text
  | PositionKey Int
  deriving stock (Eq)

rendererWith Config {moduleUrl} = Canvas.Renderer {mount}
  where
    mount element emit = do
      let canvas = FFI.canvas element
      app <- FFI.newApplication
      pending <- newIORef Nothing
      world <- newIORef Nothing
      mountedNodes <- newIORef []
      camera <- newIORef defaultCamera
      drag <- newIORef Nothing
      callbacks <- newIORef []
      wheelCallback <- newIORef Nothing
      cameraTimer <- newIORef Nothing
      disposed <- newIORef False
      ready <- newIORef False
      cleaned <- newIORef False
      let runtime = Runtime {app, canvas, emit, pending, world, mountedNodes, camera, drag, callbacks, wheelCallback, cameraTimer, disposed, ready, cleaned}
      onReady <- registerPermanent runtime $ \_ -> rendererReady runtime
      FFI.initializeApplication app moduleUrl canvas onReady
      pure
        Canvas.MountedRenderer
          { update = updateRuntime runtime
          , destroy = destroyRuntime runtime
          }

registerPermanent :: Runtime event -> (FFI.Event -> IO ()) -> IO FFI.Callback
registerPermanent Runtime {callbacks} handler = do
  callback <- FFI.mkCallback handler
  modifyIORef' callbacks (callback :)
  pure callback

rendererReady :: Runtime event -> IO ()
rendererReady runtime@Runtime {app, disposed, ready, world} = do
  writeIORef ready True
  isReady <- FFI.applicationReady app
  if isReady
    then do
      isDisposed <- readIORef disposed
      if isDisposed
        then cleanupRuntime runtime
        else do
          root <- FFI.newContainer app
          FFI.addToStage app root
          writeIORef world $ Just root
          installInteraction runtime root
          readIORef runtime.pending >>= traverse_ (renderView runtime root)
    else cleanupRuntime runtime

updateRuntime :: Runtime event -> View event -> IO ()
updateRuntime runtime@Runtime {pending, world} scene = do
  writeIORef pending $ Just scene
  readIORef world >>= traverse_ (renderView runtime `flip` scene)

renderView :: Runtime event -> FFI.Object -> View event -> IO ()
renderView runtime@Runtime {app, camera, mountedNodes} root View {camera = nextCamera, nodes} = do
  writeIORef camera nextCamera
  applyCamera app root nextCamera
  previous <- readIORef mountedNodes
  next <- reconcileNodes runtime root previous nodes
  writeIORef mountedNodes next

nodeKind :: Node event -> NodeKind
nodeKind Node {content} = case content of
  Group {} -> GroupKind
  Line {} -> GraphicsKind
  Rectangle {} -> GraphicsKind
  Circle {} -> GraphicsKind
  Ellipse {} -> GraphicsKind
  QuadraticBezier {} -> GraphicsKind
  Bezier {} -> GraphicsKind
  Arc {} -> GraphicsKind
  Label {} -> TextKind
  Sprite {} -> SpriteKind

visualState :: Node event -> VisualState
visualState Node {content} = case content of
  Group transform _ -> GroupVisual transform
  Line start end strokeStyle -> LineVisual start end strokeStyle
  Rectangle position size fillStyle strokeStyle -> RectangleVisual position size fillStyle strokeStyle
  Circle position radius fillStyle strokeStyle -> CircleVisual position radius fillStyle strokeStyle
  Ellipse position radius fillStyle strokeStyle -> EllipseVisual position radius fillStyle strokeStyle
  QuadraticBezier start control end strokeStyle -> QuadraticBezierVisual start control end strokeStyle
  Bezier start control1 control2 end strokeStyle -> BezierVisual start control1 control2 end strokeStyle
  Arc position radius startAngle endAngle anticlockwise strokeStyle -> ArcVisual position radius startAngle endAngle anticlockwise strokeStyle
  Label position value textStyle -> TextVisual position value textStyle
  Sprite position size texture -> SpriteVisual position size texture

reconcileNodes :: Runtime event -> FFI.Object -> [MountedNode event] -> [Node event] -> IO [MountedNode event]
reconcileNodes runtime parent previous nodes = go 0 previous nodes
  where
    go _ stale [] = traverse_ disposeMounted stale $> []
    go index available (node : rest) = do
      let nodeIdentity = maybe (PositionKey index) ExplicitKey node.nodeKey
          (match, remaining) = takeMatch nodeIdentity available
      mounted <- maybe (mountNode runtime parent nodeIdentity node) (\old -> reconcileNode runtime parent nodeIdentity old node) match
      FFI.setChildIndex parent mounted.object index
      (mounted :) <$> go (index + 1) remaining rest

    takeMatch wanted = search
      where
        search [] = (Nothing, [])
        search (candidate : candidates)
          | candidate.mountedIdentity == wanted = (Just candidate, candidates)
          | otherwise = let (found, remaining) = search candidates in (found, candidate : remaining)

mountNode :: Runtime event -> FFI.Object -> NodeIdentity -> Node event -> IO (MountedNode event)
mountNode runtime parent nodeIdentity node@Node {content} = do
  object <- case content of
    Group {} -> FFI.newContainer runtime.app
    Line {} -> FFI.newGraphics runtime.app
    Rectangle {} -> FFI.newGraphics runtime.app
    Circle {} -> FFI.newGraphics runtime.app
    Ellipse {} -> FFI.newGraphics runtime.app
    QuadraticBezier {} -> FFI.newGraphics runtime.app
    Bezier {} -> FFI.newGraphics runtime.app
    Arc {} -> FFI.newGraphics runtime.app
    Label {} -> FFI.newText runtime.app
    Sprite {} -> FFI.newSprite runtime.app
  FFI.addChild parent object
  let mounted = MountedNode {mountedIdentity = nodeIdentity, kind = nodeKind node, visual = Nothing, object, children = [], click = Nothing}
  updateMounted runtime nodeIdentity mounted node

reconcileNode :: Runtime event -> FFI.Object -> NodeIdentity -> MountedNode event -> Node event -> IO (MountedNode event)
reconcileNode runtime parent nodeIdentity mounted node
  | mounted.kind == nodeKind node = updateMounted runtime nodeIdentity mounted node
  | otherwise = disposeMounted mounted *> mountNode runtime parent nodeIdentity node

updateMounted :: Runtime event -> NodeIdentity -> MountedNode event -> Node event -> IO (MountedNode event)
updateMounted runtime nodeIdentity mounted node@Node {content} = case content of
  Group transform nodes -> do
    when changed $ applyTransform mounted.object transform
    children <- reconcileNodes runtime mounted.object mounted.children nodes
    finish mounted {mountedIdentity = nodeIdentity, visual = Just nextVisual, children}
  Line (Point startX startY) (Point endX endY) strokeStyle -> do
    when changed $ do
      FFI.clearGraphics mounted.object
      FFI.moveTo mounted.object startX startY
      FFI.lineTo mounted.object endX endY
      applyStroke mounted.object strokeStyle
    finish mounted {mountedIdentity = nodeIdentity, visual = Just nextVisual}
  Rectangle (Point x y) (Point width height) fillStyle strokeStyle -> do
    when changed $ do
      FFI.clearGraphics mounted.object
      FFI.rect mounted.object (-width / 2) (-height / 2) width height
      applyPaint mounted.object fillStyle strokeStyle
      FFI.setPosition mounted.object x y
    finish mounted {mountedIdentity = nodeIdentity, visual = Just nextVisual}
  Circle (Point x y) radius fillStyle strokeStyle -> do
    when changed $ do
      FFI.clearGraphics mounted.object
      FFI.circle mounted.object 0 0 radius
      applyPaint mounted.object fillStyle strokeStyle
      FFI.setPosition mounted.object x y
    finish mounted {mountedIdentity = nodeIdentity, visual = Just nextVisual}
  Ellipse (Point x y) (Point radiusX radiusY) fillStyle strokeStyle -> do
    when changed $ do
      FFI.clearGraphics mounted.object
      FFI.ellipse mounted.object 0 0 radiusX radiusY
      applyPaint mounted.object fillStyle strokeStyle
      FFI.setPosition mounted.object x y
    finish mounted {mountedIdentity = nodeIdentity, visual = Just nextVisual}
  QuadraticBezier (Point startX startY) (Point controlX controlY) (Point endX endY) strokeStyle -> do
    when changed $ do
      FFI.clearGraphics mounted.object
      FFI.moveTo mounted.object startX startY
      FFI.quadraticCurveTo mounted.object controlX controlY endX endY
      applyStroke mounted.object strokeStyle
    finish mounted {mountedIdentity = nodeIdentity, visual = Just nextVisual}
  Bezier (Point startX startY) (Point control1X control1Y) (Point control2X control2Y) (Point endX endY) strokeStyle -> do
    when changed $ do
      FFI.clearGraphics mounted.object
      FFI.moveTo mounted.object startX startY
      FFI.bezierCurveTo mounted.object control1X control1Y control2X control2Y endX endY
      applyStroke mounted.object strokeStyle
    finish mounted {mountedIdentity = nodeIdentity, visual = Just nextVisual}
  Arc (Point x y) radius startAngle endAngle anticlockwise strokeStyle -> do
    when changed $ do
      FFI.clearGraphics mounted.object
      FFI.arc mounted.object x y radius startAngle endAngle anticlockwise
      applyStroke mounted.object strokeStyle
    finish mounted {mountedIdentity = nodeIdentity, visual = Just nextVisual}
  Label (Point x y) value TextStyle {font, fontSize, textColor, textAlign} -> do
    when changed $ case font of
      SystemFont family -> FFI.setSystemText mounted.object value x y family fontSize textColor (textAlignName textAlign)
      AssetFont {family, source} -> FFI.setAssetText runtime.app mounted.object value x y family source fontSize textColor (textAlignName textAlign)
    finish mounted {mountedIdentity = nodeIdentity, visual = Just nextVisual}
  Sprite (Point x y) (Point width height) texture -> do
    when changed $ do
      FFI.setTexture runtime.app mounted.object texture.asset
      FFI.centerAnchor mounted.object
      FFI.setPosition mounted.object x y
      FFI.setSize mounted.object width height
    finish mounted {mountedIdentity = nodeIdentity, visual = Just nextVisual}
  where
    nextVisual = visualState node
    changed = mounted.visual /= Just nextVisual
    finish = updateClick runtime node.nodeEvent

textAlignName :: TextAlign -> Text
textAlignName = \case
  AlignLeft -> "left"
  AlignCenter -> "center"
  AlignRight -> "right"

updateClick :: Runtime event -> Maybe event -> MountedNode event -> IO (MountedNode event)
updateClick runtime onClick mounted = case (mounted.click, onClick) of
  (Nothing, Nothing) -> pure mounted
  (Just (eventRef, _), Nothing) -> writeIORef eventRef Nothing $> mounted
  (Just (eventRef, _), Just event) -> writeIORef eventRef (Just event) $> mounted
  (Nothing, Just event) -> do
    eventRef <- newIORef $ Just event
    callback <- FFI.mkCallback $ const $ readIORef eventRef >>= traverse_ (runtime.emit . Fired)
    FFI.onTap mounted.object callback
    pure mounted {click = Just (eventRef, callback)}

applyPaint :: FFI.Object -> Maybe FillStyle -> Maybe StrokeStyle -> IO ()
applyPaint object fillStyle strokeStyle = do
  traverse_ (\FillStyle {fillColor, fillAlpha} -> FFI.fill object fillColor fillAlpha) fillStyle
  traverse_ (applyStroke object) strokeStyle

applyStroke :: FFI.Object -> StrokeStyle -> IO ()
applyStroke object StrokeStyle {strokeColor, strokeWidth, strokeAlpha} =
  FFI.stroke object strokeColor strokeWidth strokeAlpha

disposeMounted :: MountedNode event -> IO ()
disposeMounted mounted = do
  traverse_ disposeCallbacks mounted.children
  traverse_ (FFI.freeCallback . snd) mounted.click
  FFI.destroyObject mounted.object
  where
    disposeCallbacks child = do
      traverse_ disposeCallbacks child.children
      traverse_ (FFI.freeCallback . snd) child.click

applyTransform :: FFI.Object -> Transform -> IO ()
applyTransform object Transform {position = Point x y, scale = Point scaleX scaleY, rotation} = do
  FFI.setPosition object x y
  FFI.setScale object scaleX scaleY
  FFI.setRotation object rotation

applyCamera :: FFI.Application -> FFI.Object -> Camera -> IO ()
applyCamera app root Camera {focus = Point focusX focusY, zoom} = do
  width <- FFI.screenWidth app
  height <- FFI.screenHeight app
  FFI.setPosition root (width / 2 - focusX * zoom) (height / 2 - focusY * zoom)
  FFI.setScale root zoom zoom

installInteraction :: Runtime event -> FFI.Object -> IO ()
installInteraction runtime@Runtime {app, canvas, camera, drag} root = do
  FFI.enableStageEvents app
  down <- registerPermanent runtime $ \event -> do
    readIORef runtime.pending >>= traverse_ (\View {interaction} -> when interaction.pan $ do
      pointerId <- FFI.pointerId event
      x <- FFI.globalX event
      y <- FFI.globalY event
      writeIORef drag $ Just (pointerId, x, y, False))
  FFI.onPointerDown app down
  move <- registerPermanent runtime $ \event -> do
    currentView <- readIORef runtime.pending
    activeDrag <- readIORef drag
    case (currentView, activeDrag) of
      (Just View {interaction = Interaction {pan = True}}, Just (activePointer, oldX, oldY, _)) -> do
        pointerId <- FFI.pointerId event
        when (pointerId == activePointer) $ do
          x <- FFI.globalX event
          y <- FFI.globalY event
          Camera {focus = Point focusX focusY, zoom} <- readIORef camera
          let nextCamera = Camera {focus = Point (focusX - (x - oldX) / zoom) (focusY - (y - oldY) / zoom), zoom}
          writeIORef camera nextCamera
          writeIORef drag $ Just (pointerId, x, y, True)
          applyCamera app root nextCamera
      _ -> pure ()
  FFI.onPointerMove app move
  end <- registerPermanent runtime $ \event -> do
    pointerId <- FFI.pointerId event
    readIORef drag >>= traverse_ (\(activePointer, _, _, moved) -> when (pointerId == activePointer) $ do
      writeIORef drag Nothing
      when moved $ readIORef camera >>= runtime.emit . CameraChanged)
  FFI.onPointerEnd app end
  wheelSettled <- registerPermanent runtime $ \_ -> do
    writeIORef runtime.cameraTimer Nothing
    readIORef camera >>= runtime.emit . CameraChanged
  wheel <- registerPermanent runtime $ zoomAtPointer runtime root wheelSettled
  writeIORef runtime.wheelCallback $ Just wheel
  FFI.onWheel canvas wheel

zoomAtPointer :: Runtime event -> FFI.Object -> FFI.Callback -> FFI.Event -> IO ()
zoomAtPointer Runtime {app, canvas, camera, pending, cameraTimer} root settled event = do
  currentView <- readIORef pending
  for_ currentView $ \View {interaction = Interaction {zoomRange}} -> for_ zoomRange $ \(minimumZoom, maximumZoom) -> do
    FFI.preventDefault event
    clientX <- FFI.clientX event
    clientY <- FFI.clientY event
    deltaY <- FFI.deltaY event
    left <- FFI.canvasLeft canvas
    top <- FFI.canvasTop canvas
    canvasWidth <- FFI.canvasWidth canvas
    canvasHeight <- FFI.canvasHeight canvas
    screenWidth <- FFI.screenWidth app
    screenHeight <- FFI.screenHeight app
    let screenX = (clientX - left) * screenWidth / canvasWidth
        screenY = (clientY - top) * screenHeight / canvasHeight
        pointerFromCenterX = screenX - screenWidth / 2
        pointerFromCenterY = screenY - screenHeight / 2
    Camera {focus = Point focusX focusY, zoom = oldZoom} <- readIORef camera
    let newZoom = max minimumZoom $ min maximumZoom $ oldZoom * exp (-deltaY * 0.001)
        worldX = focusX + pointerFromCenterX / oldZoom
        worldY = focusY + pointerFromCenterY / oldZoom
        nextCamera = Camera {focus = Point (worldX - pointerFromCenterX / newZoom) (worldY - pointerFromCenterY / newZoom), zoom = newZoom}
    writeIORef camera nextCamera
    applyCamera app root nextCamera
    readIORef cameraTimer >>= traverse_ FFI.cancelTimeout
    FFI.scheduleTimeout settled 120 >>= writeIORef cameraTimer . Just

destroyRuntime :: Runtime event -> IO ()
destroyRuntime runtime@Runtime {disposed, ready} = do
  writeIORef disposed True
  readIORef ready >>= flip when (cleanupRuntime runtime)

cleanupRuntime :: Runtime event -> IO ()
cleanupRuntime Runtime {app, canvas, mountedNodes, callbacks, wheelCallback, cameraTimer, cleaned} = do
  wasCleaned <- atomicModifyIORef' cleaned $ \old -> (True, old)
  unless wasCleaned $ do
    readIORef wheelCallback >>= traverse_ (FFI.removeWheel canvas)
    readIORef cameraTimer >>= traverse_ FFI.cancelTimeout
    readIORef mountedNodes >>= traverse_ disposeMounted
    writeIORef mountedNodes []
    FFI.applicationCreated app >>= flip when (FFI.destroyApplication app)
    readIORef callbacks >>= traverse_ FFI.freeCallback
