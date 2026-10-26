-- | A Three.js backend for three-dimensional scenes.
--
-- A scene is a 'Halogen.VDom.Types.VDom' of Three.js objects, built from
-- "Halogen.Three.Elements" and "Halogen.Three.Properties", so it reconciles
-- through 'Halogen.VDom.DOM.buildVDom', the machinery that reconciles HTML
-- and the PixiJS canvas. What this module supplies is the surround: a
-- renderer to draw into, a camera, the orbit interaction that moves it, and
-- the component that owns the canvas, which is core's
-- 'Halogen.Canvas.component'.
--
-- The scene is drawn on demand: once in the frame after a view or the
-- camera changed, however many changes came before it, and when the canvas
-- changes size; never in a loop of its own. A scene that moves on every
-- frame is one whose view changes on every frame
-- ('Halogen.Query.Event.Window.animationFrame').
module Halogen.Three
  ( -- * The scene
    View (..)
  , view
  , Node
  , Prop
  , SceneEvent (..)

    -- * Reading a pointer event
  , ThreeEvent
  , hitPoint
  , hitName
  , button
  , Modifiers (..)
  , noModifiers
  , modifiers
  , commandKey

    -- * Running it
  , Config (..)
  , defaultConfig
  , renderer
  , rendererWith
  , component
  , componentWith

    -- * Scene vocabulary
  , module Halogen.Three.Types
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.UUID
import Data.IORef
import Halogen qualified as H
import Halogen.Canvas qualified as Canvas
import Halogen.JSBits (JSVal)
import Halogen.Three.Camera
import Halogen.Three.Core
import Halogen.Three.Elements (group_)
import Halogen.Three.FFI qualified as FFI
import Halogen.Three.Monad
import Halogen.Three.Prop (buildProp)
import Halogen.Three.Types
import Halogen.VDom.DOM qualified as V
import Halogen.VDom.Machine qualified as V
import Halogen.VDom.Thunk (Thunk)
import Halogen.VDom.Thunk qualified as Thunk
import Halogen.VDom.Types qualified as V
import Protolude hiding (Handler)

-- | Where the renderer loads Three.js from: a module exporting Three.js's
-- API (@WebGLRenderer@, @Scene@, the geometries, materials and lights),
-- imported when a scene mounts. The default is jsDelivr's single-file ES
-- module build of a pinned version; point it at a self-hosted or bundled
-- module to need no CDN.
newtype Config = Config
  { moduleUrl :: Text
  }
  deriving stock (Eq, Show)

defaultConfig :: Config
defaultConfig =
  Config
    { moduleUrl = "https://cdn.jsdelivr.net/npm/three@0.186.1/+esm"
    }

-- | What the scene component raises.
data SceneEvent i
  = -- | An action from a handler on an object in the scene.
    Fired i
  | -- | The reader turned or zoomed the camera: raised once a drag ends,
    -- and once the wheel has stopped. Feed it back as the next view's
    -- camera to keep the camera in the application's state. A view whose
    -- camera is the same as the previous view's leaves the camera on screen
    -- where the reader put it, so to move it, change it.
    CameraMoved Camera
  deriving stock (Eq, Show, Functor)

-- | A scene: where the camera is, what is behind everything, how the reader
-- may move the camera, and what there is.
data View i = View
  { camera :: Camera
  , background :: Maybe Color
  -- ^ 'Nothing': the page behind the canvas shows through.
  , interaction :: Interaction
  , nodes :: [Node i]
  }

instance Functor View where
  fmap f scene = scene {nodes = map (fmap f) scene.nodes}

-- | A view with no background.
view :: Camera -> Interaction -> [Node i] -> View i
view camera interaction nodes = View {camera, background = Nothing, interaction, nodes}

----------------------------------------------------------------------
-- Reading a pointer event

-- | Where the ray from the pointer met the mesh, in the scene's
-- coordinates.
hitPoint :: ThreeEvent -> Vec3
hitPoint event = event.point

-- | The name of the mesh the ray met ("" for one without a
-- 'Halogen.Three.Properties.name'): for a handler on a group, which of its
-- meshes it was.
hitName :: ThreeEvent -> Text
hitName event = event.object

-- | Which mouse button, in the @MouseEvent.button@ numbering.
button :: ThreeEvent -> Int
button event = event.pressed

modifiers :: ThreeEvent -> Modifiers
modifiers event = event.held

-- | The key shortcuts go with: Ctrl, or Cmd on a Mac.
commandKey :: Modifiers -> Bool
commandKey Modifiers {ctrlKey, metaKey} = ctrlKey || metaKey

----------------------------------------------------------------------
-- Running it

componentWith :: (MonadUnliftIO m, MonadUUID m) => Config -> H.Component H.VoidF (View i) (SceneEvent i) m
componentWith = Canvas.component . rendererWith

component :: (MonadUnliftIO m, MonadUUID m) => H.Component H.VoidF (View i) (SceneEvent i) m
component = componentWith defaultConfig

renderer :: (MonadUnliftIO m) => Canvas.Renderer (View i) (SceneEvent i) m
renderer = rendererWith defaultConfig

-- | How far the renderer has got. Three.js loads asynchronously, so the
-- component can be finalized before the renderer exists; whoever moves the
-- cell out of 'Live', or finds it 'Dead' already, cleans up, once.
data Lifecycle = Loading | Live | Dead
  deriving stock (Eq, Show)

-- | Whether a render is under way, and whether a newer view came meanwhile.
data Drawing = Idle | Drawing | DrawingStale
  deriving stock (Eq, Show)

-- | A pointer turning the camera: where it went down and where it was
-- last, in the page's pixels.
data Drag = Drag
  { pointer :: Int
  , start :: (Double, Double)
  , seen :: (Double, Double)
  , going :: Bool
  -- ^ Whether it has gone further than 'slop' from where it went down:
  -- until then the camera stays, so that a click with a trembling hand is a
  -- click.
  , moved :: Bool
  }

-- | How far a pointer goes, in the page's pixels, before it turns the
-- camera.
slop :: Double
slop = 8

type SceneStep i = V.Step ThreeDOM (V.VDom [Prop i] (Thunk Node i)) FFI.Object

data Runtime i = Runtime
  { holder :: FFI.Holder
  , emit :: SceneEvent i -> IO ()
  , pending :: IORef (Maybe (View i))
  , scene :: IORef (Maybe (SceneStep i))
  , camera :: IORef Camera
  -- ^ The camera on screen.
  , asked :: IORef (Maybe Camera)
  -- ^ The camera the last view asked for (see 'followCamera').
  , drag :: IORef (Maybe Drag)
  , swallowClick :: IORef Bool
  -- ^ Set when a drag that turned the camera ends, until the next pointer
  -- goes down: the click its release would make is not one.
  , callbacks :: IORef [FFI.Callback]
  , wheelTimer :: IORef (Maybe FFI.Timer)
  , lifecycle :: IORef Lifecycle
  , drawing :: IORef Drawing
  }

rendererWith :: (MonadUnliftIO m) => Config -> Canvas.Renderer (View i) (SceneEvent i) m
rendererWith Config {moduleUrl} = Canvas.Renderer {mount}
  where
    mount host emit = withRunInIO $ \runInIO -> do
      holder <- FFI.newHolder
      pending <- newIORef Nothing
      scene <- newIORef Nothing
      camera <- newIORef defaultCamera
      asked <- newIORef Nothing
      drag <- newIORef Nothing
      swallowClick <- newIORef False
      callbacks <- newIORef []
      wheelTimer <- newIORef Nothing
      lifecycle <- newIORef Loading
      drawing <- newIORef Idle
      let runtime = Runtime {holder, emit = runInIO . emit, pending, scene, camera, asked, drag, swallowClick, callbacks, wheelTimer, lifecycle, drawing}
      onReady <- registerPermanent runtime $ \_ -> rendererReady runtime
      FFI.initialize holder moduleUrl (FFI.canvas host) onReady
      pure
        Canvas.MountedRenderer
          { update = updateRuntime runtime
          , destroy = destroyRuntime runtime
          }

-- | A callback that lives as long as the renderer, freed at teardown. The
-- objects' handlers are not these: they belong to the props that made them.
registerPermanent :: Runtime i -> (JSVal -> IO ()) -> IO FFI.Callback
registerPermanent Runtime {callbacks} handler = do
  callback <- FFI.mkCallback handler
  modifyIORef' callbacks (callback :)
  pure callback

rendererReady :: Runtime i -> IO ()
rendererReady runtime = do
  started <- FFI.ready runtime.holder
  previous <- atomicModifyIORef' runtime.lifecycle $ \old ->
    (if old == Loading && started then Live else Dead, old)
  case (previous, started) of
    (Loading, True) -> do
      installInteraction runtime
      renderLatest runtime
    -- Finalized while Three.js was loading, or it never came up: either way
    -- nobody else is going to clean up.
    (Loading, False) -> cleanupRuntime runtime
    (Dead, _) -> cleanupRuntime runtime
    (Live, _) -> pass

updateRuntime :: (MonadIO m) => Runtime i -> View i -> m ()
updateRuntime runtime next = liftIO $ do
  writeIORef runtime.pending (Just next)
  live <- (== Live) <$> readIORef runtime.lifecycle
  when live $ renderLatest runtime

-- | The vdom spec the scene reconciles through, all of it in 'ThreeDOM': a
-- scene's only widgets are thunks.
threeSpec :: Runtime i -> V.VDomSpec ThreeDOM [Prop i] (Thunk Node i)
threeSpec runtime =
  V.VDomSpec
    { buildWidget = Thunk.buildThunk unNode
    , hydrateWidget = Thunk.hydrateThunk unNode
    , buildAttributes = buildProp (liftIO . runtime.emit . Fired) (readIORef runtime.swallowClick) runtime.holder
    , document = runtime.holder
    }

-- | Render the latest view ('pending'), one render at a time: a callback
-- the page makes during a render lets other Haskell threads run, and a
-- second render started meanwhile would build a second world. A render
-- asked for while one is under way is left to it, and it renders the latest
-- view again when done.
renderLatest :: Runtime i -> IO ()
renderLatest runtime = do
  start <- atomicModifyIORef' runtime.drawing $ \case
    Idle -> (Drawing, True)
    _ -> (DrawingStale, False)
  when start go
  where
    go = do
      (readIORef runtime.pending >>= traverse_ (renderView runtime))
        `onException` writeIORef runtime.drawing Idle
      again <- atomicModifyIORef' runtime.drawing $ \case
        DrawingStale -> (Drawing, True)
        _ -> (Idle, False)
      when again go

renderView :: Runtime i -> View i -> IO ()
renderView runtime View {camera = viewCamera, background, nodes} = do
  -- A view rendered mid-drag still carries the camera from before it; only
  -- a camera the application changed moves the one on screen.
  lastAsked <- atomicModifyIORef' runtime.asked (Just viewCamera,)
  nextCamera <- followCamera lastAsked viewCamera <$> readIORef runtime.camera
  writeIORef runtime.camera nextCamera
  let world = unNode (group_ nodes)
  existing <- readIORef runtime.scene
  next <- runThreeDOM $ case existing of
    Nothing -> V.buildVDom (threeSpec runtime) world
    Just previous -> V.step previous world
  writeIORef runtime.scene (Just next)
  -- Setting the root again is a no-op unless a patch replaced it.
  FFI.setRoot runtime.holder (V.extract next)
  FFI.setBackground runtime.holder (fromMaybe (-1) background)
  applyCamera runtime.holder nextCamera
  FFI.requestRender runtime.holder

applyCamera :: FFI.Holder -> Camera -> IO ()
applyCamera holder = \case
  Perspective {fov, near, far, position = Vec3 px py pz, target = Vec3 tx ty tz} ->
    FFI.setPerspective holder fov near far px py pz tx ty tz
  Orthographic {height, near, far, position = Vec3 px py pz, target = Vec3 tx ty tz} ->
    FFI.setOrthographic holder height near far px py pz tx ty tz

-- | A camera the reader moved, drawn in the next frame.
moveCamera :: Runtime i -> Camera -> IO ()
moveCamera runtime next = do
  writeIORef runtime.camera next
  applyCamera runtime.holder next
  FFI.requestRender runtime.holder

----------------------------------------------------------------------
-- Orbit and zoom

-- | The interaction the latest view asks for.
currentInteraction :: Runtime i -> IO (Maybe Interaction)
currentInteraction runtime = fmap (.interaction) <$> readIORef runtime.pending

installInteraction :: Runtime i -> IO ()
installInteraction runtime@Runtime {holder, camera, drag, swallowClick} = do
  down <- registerPermanent runtime $ \event -> do
    writeIORef swallowClick False
    orbiting <- maybe False (.orbit) <$> currentInteraction runtime
    primary <- FFI.isPrimary event
    pressed <- FFI.button event
    existing <- readIORef drag
    when (orbiting && primary && pressed == 0 && isNothing existing) $ do
      pointer <- FFI.pointerId event
      here <- clientPoint event
      writeIORef drag $ Just Drag {pointer, start = here, seen = here, going = False, moved = False}
  FFI.onGesture holder "down" down

  move <- registerPermanent runtime $ \event -> do
    pointer <- FFI.pointerId event
    dragging <- readIORef drag
    for_ dragging $ \d -> when (d.pointer == pointer) $ do
      -- A view that took the orbit away mid-drag (one that put the camera
      -- somewhere, say) ends the drag where it is: the pointer, still
      -- down, turns the camera no further.
      orbiting <- maybe False (.orbit) <$> currentInteraction runtime
      here <- clientPoint event
      if not orbiting
        then endDrag d
        else
          if not (d.going || far d.start here)
            then writeIORef drag (Just d {seen = here})
            else do
              -- Past the slop, the first move catches up from where the
              -- pointer went down. A full drag across the canvas's height
              -- turns the camera once around, as OrbitControls do.
              let (fromX, fromY) = if d.going then d.seen else d.start
                  (toX, toY) = here
              height <- FFI.canvasHeight holder
              current <- readIORef camera
              let turn = if height > 0 then 2 * pi / height else 0
                  next = orbitBy (-((toX - fromX) * turn)) (-((toY - fromY) * turn)) current
              writeIORef drag (Just d {seen = here, going = True, moved = d.moved || next /= current})
              when (next /= current) $ moveCamera runtime next
  FFI.onGesture holder "move" move

  up <- registerPermanent runtime $ \event -> do
    pointer <- FFI.pointerId event
    dragging <- readIORef drag
    for_ dragging $ \d -> when (d.pointer == pointer) $ endDrag d
  FFI.onGesture holder "up" up

  -- The wheel raises an event a notch, so the camera is reported once the
  -- wheel has stopped rather than on every notch.
  settled <- registerPermanent runtime $ \_ -> do
    writeIORef runtime.wheelTimer Nothing
    readIORef camera >>= runtime.emit . CameraMoved
  wheel <- registerPermanent runtime $ \event -> do
    interaction <- currentInteraction runtime
    for_ (interaction >>= (.zoomRange)) $ \range -> do
      FFI.preventDefault event
      delta <- FFI.deltaY event
      current <- readIORef camera
      let next = zoomBy (Just range) (exp (delta * 0.001)) current
      when (next /= current) $ moveCamera runtime next
      readIORef runtime.wheelTimer >>= traverse_ FFI.cancelTimeout
      FFI.scheduleTimeout settled 120 >>= writeIORef runtime.wheelTimer . Just
  FFI.onGesture holder "wheel" wheel
  where
    clientPoint event = (,) <$> FFI.clientX event <*> FFI.clientY event
    far (x0, y0) (x1, y1) = sqrt ((x1 - x0) ^ (2 :: Int) + (y1 - y0) ^ (2 :: Int)) > slop
    -- The end of a drag that turned the camera is no click, and the camera
    -- is reported once, as the drag ends.
    endDrag d = do
      writeIORef drag Nothing
      when d.moved $ do
        writeIORef swallowClick True
        readIORef camera >>= runtime.emit . CameraMoved

----------------------------------------------------------------------
-- Teardown

destroyRuntime :: (MonadIO m) => Runtime i -> m ()
destroyRuntime runtime = liftIO $ do
  previous <- atomicModifyIORef' runtime.lifecycle $ \old -> (Dead, old)
  -- Still loading: the cleanup is 'rendererReady''s, once it finds the
  -- cell dead.
  when (previous == Live) $ cleanupRuntime runtime

cleanupRuntime :: Runtime i -> IO ()
cleanupRuntime Runtime {holder, scene, callbacks, wheelTimer} = do
  readIORef wheelTimer >>= traverse_ FFI.cancelTimeout
  -- Halting the scene takes every object out, disposing of its geometry
  -- and material, and frees the handlers the props installed; then the
  -- renderer goes, with its WebGL context. The scene is taken out of its
  -- cell first: halting can let the page run, and a render meanwhile would
  -- patch objects already gone.
  atomicModifyIORef' scene (Nothing,) >>= traverse_ (runThreeDOM . V.halt)
  FFI.destroy holder
  readIORef callbacks >>= traverse_ FFI.freeCallback
