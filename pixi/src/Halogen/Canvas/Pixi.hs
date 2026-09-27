-- | A PixiJS backend for the canvas scene language in "Halogen.Canvas.Core".
--
-- The scene is a 'Halogen.VDom.Types.VDom', so reconciliation is
-- 'Halogen.VDom.DOM.buildVDom' — the same machinery that reconciles HTML.
-- What this module supplies is the surround: an application to render into, a
-- camera, and the pan/zoom interaction that camera needs.
module Halogen.Canvas.Pixi
  ( -- * The scene
    View (..)
  , view
  , PixiEvent
  , PixiNode
  , PixiProp
  , CanvasEvent (..)

    -- * Reading a pointer event
  , pointerId
  , stagePosition
  , localPosition
  , button

    -- * Running it
  , Config (..)
  , defaultConfig
  , renderer
  , rendererWith
  , component
  , componentWith

    -- * Scene vocabulary
  , module Halogen.Canvas.Types
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.UUID
import Data.IORef
import Data.Map.Strict qualified as Map
import Halogen qualified as H
import Halogen.Canvas qualified as Canvas
import Halogen.Canvas.Core
import Halogen.Canvas.Elements (group_)
import Halogen.Canvas.Pixi.FFI qualified as FFI
import Halogen.Canvas.Pixi.Monad
import Halogen.Canvas.Pixi.Prop (buildCanvasProp)
import Halogen.Canvas.Types
import Halogen.VDom.DOM qualified as V
import Halogen.VDom.Machine qualified as V
import Halogen.VDom.Thunk (Thunk)
import Halogen.VDom.Thunk qualified as Thunk
import Halogen.VDom.Types qualified as V
import Protolude

-- | The pointer event a Pixi handler receives.
type PixiEvent = FFI.Event

type PixiNode i = CanvasNode PixiEvent i

type PixiProp i = CanvasProp PixiEvent i

-- | Where the renderer should load PixiJS from. The module must export the
-- PixiJS v8 API. Keeping this in the renderer configuration lets applications
-- use the zero-setup CDN default or point at a self-hosted/bundled module.
newtype Config = Config
  { moduleUrl :: Text
  }
  deriving stock (Eq, Show)

defaultConfig :: Config
defaultConfig =
  Config
    { moduleUrl = "https://cdn.jsdelivr.net/npm/pixi.js@8.20.1/dist/pixi.min.mjs"
    }

-- | What the canvas component raises.
data CanvasEvent i
  = -- | An action from a handler on some element in the scene.
    Fired i
  | -- | The user panned or zoomed. Feed it back as the next 'View' camera to
    -- keep the camera in application state. A view whose camera is the same
    -- as the previous view's leaves the camera on screen where the user put
    -- it (see 'followCamera'), so to move it, change it. With this event fed
    -- back, moving it back to where it was before is a change too.
    CameraChanged Camera
  deriving stock (Eq, Show, Functor)

-- | A scene: what to draw, where the camera is, and how the user may move it.
data View i = View
  { camera :: Camera
  , interaction :: Interaction
  , nodes :: [PixiNode i]
  }

instance Functor View where
  fmap f scene = scene {nodes = map (fmap f) scene.nodes}

view :: Camera -> Interaction -> [PixiNode i] -> View i
view camera interaction nodes = View {camera, interaction, nodes}

----------------------------------------------------------------------
-- Reading a pointer event

pointerId :: PixiEvent -> Int
pointerId = FFI.pointerId

-- | Where the pointer is on the renderer's surface, in stage coordinates.
--
-- Not scene coordinates: the camera is a transform on the container the
-- scene hangs from, and this is measured outside it, so panning or zooming
-- changes this value for a pointer that has not moved. Converting needs the
-- camera and the surface size, neither of which an event carries.
--
-- 'localPosition' is what a handler usually wants, and it is unaffected by
-- the camera.
stagePosition :: PixiEvent -> Point
stagePosition event = Point (FFI.globalX event) (FFI.globalY event)

-- | Where the pointer is relative to the element the handler is attached to.
--
-- Below the camera, so this is in the element's own coordinates whatever the
-- view is doing.
localPosition :: PixiEvent -> Point
localPosition event =
  let target = FFI.currentTarget event
   in Point (FFI.localX target event) (FFI.localY target event)

-- | Which mouse button, in the @MouseEvent.button@ numbering.
button :: PixiEvent -> Int
button = FFI.eventButton

----------------------------------------------------------------------
-- Running it

componentWith :: (MonadUnliftIO m, MonadUUID m) => Config -> H.Component H.VoidF (View i) (CanvasEvent i) m
componentWith = Canvas.component . rendererWith

component :: (MonadUnliftIO m, MonadUUID m) => H.Component H.VoidF (View i) (CanvasEvent i) m
component = componentWith defaultConfig

renderer :: (MonadUnliftIO m) => Canvas.Renderer (View i) (CanvasEvent i) m
renderer = rendererWith defaultConfig

-- | How far the application has got.
--
-- Pixi initializes asynchronously, so the component can be finalized before
-- the renderer exists. Keeping the three states in one cell makes the
-- teardown happen exactly once by construction: it is whoever moves the cell
-- out of 'Live', or finds it already 'Dead', that cleans up.
data Lifecycle
  = -- | The pixi module is still loading.
    Loading
  | -- | Running.
    Live
  | -- | Finalized, or failed to load.
    Dead
  deriving stock (Eq, Show)

-- | The pointers down on the scene, and where each was last: one pans the
-- camera, two pan and zoom it (a pinch).
data Gesture = Gesture
  { pointers :: Map Int Point
  , moved :: Bool
  -- ^ Whether the camera has actually moved, so that a plain click does not
  -- raise a spurious 'CameraChanged'.
  }

noGesture :: Gesture
noGesture = Gesture {pointers = Map.empty, moved = False}

type SceneStep i = V.Step PixiDOM (V.VDom [PixiProp i] (Thunk (CanvasNode PixiEvent) i)) FFI.Object

data Runtime i = Runtime
  { app :: FFI.Application
  , canvas :: FFI.Canvas
  , emit :: CanvasEvent i -> IO ()
  , pending :: IORef (Maybe (View i))
  , scene :: IORef (Maybe (SceneStep i))
  , camera :: IORef Camera
  -- ^ The camera on screen.
  , asked :: IORef (Maybe Camera)
  -- ^ The camera the last view asked for (see 'followCamera').
  , gesture :: IORef Gesture
  , callbacks :: IORef [FFI.Callback]
  , wheelCallback :: IORef (Maybe FFI.Callback)
  , cameraTimer :: IORef (Maybe FFI.Timer)
  , lifecycle :: IORef Lifecycle
  }

rendererWith :: (MonadUnliftIO m) => Config -> Canvas.Renderer (View i) (CanvasEvent i) m
rendererWith Config {moduleUrl} = Canvas.Renderer {mount}
  where
    mount host emit = withRunInIO $ \runInIO -> do
      let canvas = FFI.canvas host
      app <- FFI.newApplication
      pending <- newIORef Nothing
      scene <- newIORef Nothing
      camera <- newIORef defaultCamera
      asked <- newIORef Nothing
      gesture <- newIORef noGesture
      callbacks <- newIORef []
      wheelCallback <- newIORef Nothing
      cameraTimer <- newIORef Nothing
      lifecycle <- newIORef Loading
      let runtime = Runtime {app, canvas, emit = runInIO . emit, pending, scene, camera, asked, gesture, callbacks, wheelCallback, cameraTimer, lifecycle}
      onReady <- registerPermanent runtime $ \_ -> rendererReady runtime
      FFI.initializeApplication app moduleUrl canvas onReady
      pure
        Canvas.MountedRenderer
          { update = updateRuntime runtime
          , destroy = destroyRuntime runtime
          }

-- | A callback that lives as long as the renderer does, freed in one go at
-- teardown. Scene-level listeners are not these — those belong to the props
-- that installed them.
registerPermanent :: Runtime i -> (FFI.Event -> IO ()) -> IO FFI.Callback
registerPermanent Runtime {callbacks} handler = do
  callback <- FFI.mkCallback handler
  modifyIORef' callbacks (callback :)
  pure callback

rendererReady :: Runtime i -> IO ()
rendererReady runtime = do
  started <- FFI.applicationReady runtime.app
  previous <- atomicModifyIORef' runtime.lifecycle $ \old ->
    (if old == Loading && started then Live else Dead, old)
  case (previous, started) of
    (Loading, True) -> do
      FFI.enableStageEvents runtime.app
      installInteraction runtime
      readIORef runtime.pending >>= traverse_ (renderView runtime)
    -- Finalized while the module was still loading, or it never loaded:
    -- either way nobody else is going to clean up after us.
    (Loading, False) -> cleanupRuntime runtime
    (Dead, _) -> cleanupRuntime runtime
    (Live, _) -> pass

updateRuntime :: (MonadIO m) => Runtime i -> View i -> m ()
updateRuntime runtime scene = liftIO $ do
  writeIORef runtime.pending $ Just scene
  live <- (== Live) <$> readIORef runtime.lifecycle
  when live $ renderView runtime scene

-- | The vdom spec the scene reconciles through.
--
-- All of it in 'PixiDOM': a canvas scene has no component slots — its only
-- widgets are thunks — so there is nothing below here that needs the monad
-- the application chose.
pixiSpec :: Runtime i -> V.VDomSpec PixiDOM [PixiProp i] (Thunk (CanvasNode PixiEvent) i)
pixiSpec runtime =
  V.VDomSpec
    { buildWidget = Thunk.buildThunk unCanvasNode
    , buildAttributes = buildCanvasProp (liftIO . runtime.emit . Fired) runtime.app
    , document = runtime.app
    }

-- | The scene is wrapped in one container, which is also what the camera
-- transform is applied to: panning and zooming move the world, not the stage.
renderView :: Runtime i -> View i -> IO ()
renderView runtime View {camera = viewCamera, nodes} = do
  -- A view rendered mid-drag still carries the camera from before the drag;
  -- only a camera the application changed moves the one on screen.
  lastAsked <- atomicModifyIORef' runtime.asked (Just viewCamera,)
  nextCamera <- followCamera lastAsked viewCamera <$> readIORef runtime.camera
  writeIORef runtime.camera nextCamera
  let world = unCanvasNode (group_ nodes)
  existing <- readIORef runtime.scene
  next <- runPixiDOM $ case existing of
    Nothing -> V.buildVDom (pixiSpec runtime) world
    Just previous -> V.step previous world
  writeIORef runtime.scene $ Just next
  -- Re-adding the sole child of the stage is a no-op, so this needs no
  -- first-render case; it matters only when a patch replaced the root.
  FFI.addToStage runtime.app (V.extract next)
  applyCamera runtime.app (V.extract next) nextCamera

currentWorld :: Runtime i -> IO (Maybe FFI.Object)
currentWorld runtime = fmap V.extract <$> readIORef runtime.scene

applyCamera :: FFI.Application -> FFI.Object -> Camera -> IO ()
applyCamera app world Camera {focus = Point focusX focusY, zoom} = do
  width <- FFI.screenWidth app
  height <- FFI.screenHeight app
  FFI.setPosition world (width / 2 - focusX * zoom) (height / 2 - focusY * zoom)
  FFI.setScale world zoom zoom

-- | Push a camera the user moved back out to the application, and repaint.
moveCamera :: Runtime i -> Camera -> IO ()
moveCamera runtime nextCamera = do
  writeIORef runtime.camera nextCamera
  currentWorld runtime >>= traverse_ (\world -> applyCamera runtime.app world nextCamera)

----------------------------------------------------------------------
-- Pan and zoom

installInteraction :: Runtime i -> IO ()
installInteraction runtime@Runtime {app, canvas, camera, gesture} = do
  down <- registerPermanent runtime $ \event ->
    whenPanning runtime $ \_ ->
      modifyIORef' gesture $ \g ->
        g {pointers = Map.insert (FFI.pointerId event) (Point (FFI.globalX event) (FFI.globalY event)) g.pointers}
  FFI.onPointerDown app down

  move <- registerPermanent runtime $ \event -> whenPanning runtime $ \scene -> do
    g <- readIORef gesture
    let pointer = FFI.pointerId event
        here = Point (FFI.globalX event) (FFI.globalY event)
    for_ (Map.lookup pointer g.pointers) $ \before -> do
      let after = Map.insert pointer here g.pointers
      writeIORef gesture g {pointers = after, moved = True}
      width <- FFI.screenWidth app
      height <- FFI.screenHeight app
      current <- readIORef camera
      moveCamera runtime $ case (Map.elems g.pointers, Map.elems after) of
        -- Two fingers: the first two down, as they were and as they are.
        (a : b : _, a' : b' : _) -> pinch (Point (width / 2) (height / 2)) scene.interaction.zoomRange (a, b) (a', b') current
        _ -> pan before here current
  FFI.onPointerMove app move

  end <- registerPermanent runtime $ \event -> do
    g <- readIORef gesture
    when (Map.member (FFI.pointerId event) g.pointers) $ do
      let left = Map.delete (FFI.pointerId event) g.pointers
      -- A finger that stays down after a pinch pans on from where it is.
      writeIORef gesture g {pointers = left, moved = g.moved && not (Map.null left)}
      when (Map.null left && g.moved) $ readIORef camera >>= runtime.emit . CameraChanged
  FFI.onPointerEnd app end

  -- A wheel raises one event per notch, so the camera is reported once the
  -- gesture has stopped rather than on every notch.
  settled <- registerPermanent runtime $ \_ -> do
    writeIORef runtime.cameraTimer Nothing
    readIORef camera >>= runtime.emit . CameraChanged
  wheel <- registerPermanent runtime $ zoomAtPointer runtime settled
  writeIORef runtime.wheelCallback $ Just wheel
  FFI.onWheel canvas wheel

-- | The camera after a pointer moved from one point on the screen to
-- another: the world under it moves with it.
pan :: Point -> Point -> Camera -> Camera
pan (Point x0 y0) (Point x1 y1) Camera {focus = Point focusX focusY, zoom} =
  Camera {focus = Point (focusX - (x1 - x0) / zoom) (focusY - (y1 - y0) / zoom), zoom}

-- | The camera after two pointers moved (screen points, the screen's centre
-- given): zoomed by how far apart they went, within the range if there is
-- one (none: no zoom), and panned so that the world point between them
-- stays between them.
pinch :: Point -> Maybe (Double, Double) -> (Point, Point) -> (Point, Point) -> Camera -> Camera
pinch (Point centreX centreY) range (a, b) (a', b') Camera {focus = Point focusX focusY, zoom} =
  Camera {focus = Point (worldX - (midX' - centreX) / zoom') (worldY - (midY' - centreY) / zoom'), zoom = zoom'}
  where
    Point midX midY = midpoint a b
    Point midX' midY' = midpoint a' b'
    worldX = focusX + (midX - centreX) / zoom
    worldY = focusY + (midY - centreY) / zoom
    zoom' = case range of
      Just (low, high) | distance a b > 0 -> max low (min high (zoom * distance a' b' / distance a b))
      _ -> zoom
    midpoint (Point x0 y0) (Point x1 y1) = Point ((x0 + x1) / 2) ((y0 + y1) / 2)
    distance (Point x0 y0) (Point x1 y1) = sqrt ((x1 - x0) ^ (2 :: Int) + (y1 - y0) ^ (2 :: Int))

whenPanning :: Runtime i -> (View i -> IO ()) -> IO ()
whenPanning runtime act = do
  current <- readIORef runtime.pending
  for_ current $ \scene -> when scene.interaction.pan $ act scene

zoomAtPointer :: Runtime i -> FFI.Callback -> FFI.Event -> IO ()
zoomAtPointer runtime@Runtime {app, canvas, camera, pending, cameraTimer} settled event = do
  current <- readIORef pending
  for_ current $ \scene -> for_ scene.interaction.zoomRange $ \(minimumZoom, maximumZoom) -> do
    FFI.preventDefault event
    left <- FFI.canvasLeft canvas
    top <- FFI.canvasTop canvas
    canvasWidth <- FFI.canvasWidth canvas
    canvasHeight <- FFI.canvasHeight canvas
    screenWidth <- FFI.screenWidth app
    screenHeight <- FFI.screenHeight app
    let screenX = (FFI.clientX event - left) * screenWidth / canvasWidth
        screenY = (FFI.clientY event - top) * screenHeight / canvasHeight
        pointerFromCenterX = screenX - screenWidth / 2
        pointerFromCenterY = screenY - screenHeight / 2
    Camera {focus = Point focusX focusY, zoom = oldZoom} <- readIORef camera
    let newZoom = max minimumZoom $ min maximumZoom $ oldZoom * exp (-FFI.deltaY event * 0.001)
        worldX = focusX + pointerFromCenterX / oldZoom
        worldY = focusY + pointerFromCenterY / oldZoom
    -- Keep whatever is under the pointer under the pointer.
    moveCamera runtime $
      Camera {focus = Point (worldX - pointerFromCenterX / newZoom) (worldY - pointerFromCenterY / newZoom), zoom = newZoom}
    readIORef cameraTimer >>= traverse_ FFI.cancelTimeout
    FFI.scheduleTimeout settled 120 >>= writeIORef cameraTimer . Just

----------------------------------------------------------------------
-- Teardown

destroyRuntime :: (MonadIO m) => Runtime i -> m ()
destroyRuntime runtime = liftIO $ do
  previous <- atomicModifyIORef' runtime.lifecycle $ \old -> (Dead, old)
  -- If it is still 'Loading' the cleanup is 'rendererReady''s to do, once it
  -- finds the cell already dead; there is nothing to tear down before then.
  when (previous == Live) $ cleanupRuntime runtime

cleanupRuntime :: Runtime i -> IO ()
cleanupRuntime Runtime {app, canvas, scene, callbacks, wheelCallback, cameraTimer} = do
  readIORef wheelCallback >>= traverse_ (FFI.removeWheel canvas)
  readIORef cameraTimer >>= traverse_ FFI.cancelTimeout
  -- Halting the scene frees every listener the props installed; destroying
  -- the application takes the display objects with it.
  readIORef scene >>= traverse_ (runPixiDOM . V.halt)
  writeIORef scene Nothing
  FFI.applicationCreated app >>= flip when (FFI.destroyApplication app)
  readIORef callbacks >>= traverse_ FFI.freeCallback
