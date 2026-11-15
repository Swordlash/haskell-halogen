# haskell-halogen-three

Render three-dimensional scenes in Halogen with [Three.js](https://threejs.org). Describe meshes and lights using an API similar to Halogen HTML. The same reconciler updates HTML, PixiJS canvases and these scenes.

```haskell
import Halogen.Three qualified as Three
import Halogen.Three.Elements qualified as TE
import Halogen.Three.Properties qualified as TP

data Action = Scene (Three.SceneEvent ())

scene :: Bool -> Three.View ()
scene lit =
  Three.view
    Three.defaultCamera
    Three.defaultInteraction
    [ TE.ambientLight 0xffffff 0.6
    , TE.directionalLight 0xffffff 2.5 (Three.Vec3 3 5 4)
    , TE.mesh (Three.Box 1 1 1) (Three.standard (if lit then 0xf59e0b else 0x64748b)) [TP.onClick (\_ -> Just ())]
    ]

render lit = HH.div [HP.style (C.width (C.px 400) >> C.height (C.px 300))] [HH.slot "scene" () Three.component (scene lit) Scene]
```

`Three.component` is a child component that takes a scene as input and owns a `<canvas>` sized to its parent. It uses core's `Halogen.Canvas.component` with this package's renderer.

## The scene

A `Three.View` contains the camera, background, interaction settings and scene nodes. Set the background to `Nothing` to let the page show through.

- `TE.group`, `TE.group_` and `TE.keyedGroup` move, turn and scale objects together; a keyed group's children keep their objects when reordered.
- `TE.mesh geometry material props` is a shape (`Box`, `Sphere`, `Plane`, `Cylinder`, `Torus`) made of a material (`Basic`, unlit; `Standard`, lit physically, with metalness and roughness; `Phong`, with a highlight; `Three.basic`, `Three.standard` and `Three.phong` make one of a colour).
- `TE.ambientLight`, `TE.directionalLight` and `TE.pointLight` light the scene: a `Standard` or `Phong` mesh without light is black. Three.js's lighting is physically correct, so a point light falls off with the square of the distance and needs an intensity of tens a few units away.
- `TE.axes` draws the axes, for finding one's way while writing a scene.
- `TE.memoized` and `TE.lazy` skip a part of the scene while its input is the same, as `Halogen.Canvas.Elements` does.

`TP.position`, `TP.rotation`, `TP.scale`, `TP.visible` and `TP.name` control an object's placement, visibility and name. Rotation uses an `Euler` value in radians. Removing a prop restores its default behaviour.

Geometry and material are properties of a mesh node. Changing a material's colour, opacity or other fields updates it in place. Changing its kind replaces it, and changing geometry replaces the geometry. The renderer disposes replaced resources and resources belonging to removed objects, releasing their GPU allocations. Removing the component disposes the entire scene, renderer and WebGL context.

## Pointer events

The renderer handles pointer events by listening on the canvas and casting a ray into the scene with `THREE.Raycaster`. It delivers each event to the nearest visible mesh hit by the ray, or to its nearest ancestor listening for that event. Supported handlers are `TP.onClick`, `TP.onPointerDown`, `TP.onPointerUp`, `TP.onPointerOver` and `TP.onPointerOut`. A handler receives a `ThreeEvent` with `Three.hitPoint` in scene coordinates, `Three.hitName`, `Three.button` and `Three.modifiers`. The component emits its action as `Three.Fired`.

A click requires a press and release on the same listener without a camera drag in between. Releasing after a drag does not trigger a click.

## The camera

`Three.defaultCamera` places a perspective camera at (4, 3, 6), looking at the origin. With `Three.defaultInteraction`, dragging orbits the target, and scrolling zooms between 1 and 50 units away. A drag across the canvas's full height makes one complete turn. `Three.noInteraction` disables these controls.

The component emits `Three.CameraMoved` after a drag or wheel gesture ends. Feed that value into the next view to keep the camera in application state. If a new view has the same camera value as the previous view, the renderer preserves the user's current position. This prevents unrelated renders from resetting an ongoing drag. `Halogen.Three.Camera` also exposes orbit and zoom functions for keyboard or timer controls.

## Drawing

The renderer schedules one draw in the next frame after a view or camera change, combining changes that happen before that frame. Resizing the canvas draws immediately. To animate continuously, update the view on each `Halogen.Query.Event.Window.animationFrame` event.

## Loading Three.js

`Three.component` loads Three.js when mounted. By default, `defaultConfig.moduleUrl` points to the pinned, single-file ES module at `https://cdn.jsdelivr.net/npm/three@0.186.1/+esm`. To host Three.js yourself, either serve both `build/three.module.js` and its `three.core.js` dependency, or bundle them:

```sh
esbuild node_modules/three/build/three.module.js --bundle --format=esm --minify --outfile=public/three.mjs
```

Then use `Three.componentWith (Three.Config {moduleUrl = "/three.mjs"})`. The test suite uses this approach in `test/web/bundle.sh`, so it needs no network access.

## Tests

`Halogen-three-test` checks camera orbit, zoom and generated VDom natively and under Node on both browser backends. `Halogen-three-browser-test` runs in headless Chromium with SwiftShader, so no GPU is required. It checks the first frame, mesh clicks, camera dragging without accidental clicks, material updates and resource disposal. Disposal checks wrap Three.js's `dispose` methods to count calls.
