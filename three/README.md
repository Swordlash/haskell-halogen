# haskell-halogen-three

A three-dimensional scene for a Halogen component, rendered with
[Three.js](https://threejs.org): meshes and lights described as HTML is,
and reconciled by the same machinery that reconciles HTML and the PixiJS
canvas of `haskell-halogen-pixi`.

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

`Three.component` is an ordinary child component whose input is the scene
(core's `Halogen.Canvas.component` over this package's renderer). It owns a
`<canvas>` that follows its parent's size.

## The scene

A `Three.View` is a camera, a background (`Nothing`: the page shows
through), how the reader may move the camera, and the nodes:

- `TE.group`, `TE.group_` and `TE.keyedGroup` move, turn and scale objects
  together; a keyed group's children keep their objects when reordered.
- `TE.mesh geometry material props` is a shape (`Box`, `Sphere`, `Plane`,
  `Cylinder`, `Torus`) made of a material (`Basic`, unlit; `Standard`, lit
  physically, with metalness and roughness; `Phong`, with a highlight;
  `Three.basic`, `Three.standard` and `Three.phong` make one of a colour).
- `TE.ambientLight`, `TE.directionalLight` and `TE.pointLight` light the
  scene: a `Standard` or `Phong` mesh without light is black. Three.js's
  lighting is physically correct, so a point light falls off with the square
  of the distance and needs an intensity of tens a few units away.
- `TE.axes` draws the axes, for finding one's way while writing a scene.
- `TE.memoized` and `TE.lazy` skip a part of the scene while its input is
  the same, as `Halogen.Canvas.Elements` does.

`TP.position`, `TP.rotation` (an `Euler`, radians), `TP.scale`,
`TP.visible` and `TP.name` place and name an object. Each prop undoes itself
when a render drops it.

The geometry and the material are props of the mesh's node, not nodes of
their own. A material whose colour, opacity or another field changed is
patched in place; one of another kind replaces it. A geometry that changed
is replaced. Whatever is replaced, and whatever a removed object had, is
disposed of: Three.js keeps a geometry's buffers and a material's program on
the GPU until told to let them go. When the component goes, everything left
in the scene is disposed of, and the renderer with its WebGL context.

## Pointer events

Three.js has no events of its own. The renderer listens on the canvas, casts
a ray from the pointer into the scene (`THREE.Raycaster`) and hands the
event to the nearest visible mesh the ray meets, or to the nearest of its
ancestors listening for it: `TP.onClick`, `TP.onPointerDown`,
`TP.onPointerUp`, `TP.onPointerOver`, `TP.onPointerOut`. A handler gets a
`ThreeEvent`: `Three.hitPoint` (where the ray met the mesh, in the scene's
coordinates), `Three.hitName` (the mesh's name), `Three.button` and
`Three.modifiers`. Its action is raised as `Three.Fired`.

A click is a press and a release on the same listener without turning the
camera in between: the release of a drag is no click.

## The camera

`Three.defaultCamera` is a perspective camera at (4, 3, 6) looking at the
origin. With `Three.defaultInteraction` a drag turns the camera about its
target (a drag across the canvas's height is a full turn) and the wheel
brings it nearer or takes it further, between 1 and 50 units;
`Three.noInteraction` keeps it where the view puts it. The component raises
`Three.CameraMoved` once a drag ends and once the wheel stops: feed it back
as the next view's camera to keep the camera in the application's state. A
view whose camera is the same as the last view's leaves the camera on screen
where the reader put it, so a view rendered mid-drag for another reason does
not put it back. `Halogen.Three.Camera` has the same orbit and zoom as
functions, for moving the camera from keys or a timer.

## Drawing

The scene is drawn on demand, not in a loop: once in the frame after a
view or the camera changed, however many changes came before it, and at
once when the canvas changes size. A scene that moves on every frame is one
whose view changes on every frame (`Halogen.Query.Event.Window.animationFrame`).

## Loading Three.js

`Three.component` imports Three.js when a scene mounts, from jsDelivr by
default: `defaultConfig`'s `moduleUrl` is the single-file ES module build of
a pinned version (`https://cdn.jsdelivr.net/npm/three@0.186.1/+esm`).
Three.js's own `build/three.module.js` imports `three.core.js` beside it, so a
self-hosted copy is either both files or one bundled module:

```sh
esbuild node_modules/three/build/three.module.js --bundle --format=esm --minify --outfile=public/three.mjs
```

and `Three.componentWith (Three.Config {moduleUrl = "/three.mjs"})`. The test
suite does exactly that (`test/web/bundle.sh`), so it needs no network.

## Tests

`Halogen-three-test` runs natively and under Node on both browser backends:
the camera's orbit and zoom, and the VDom the elements build.
`Halogen-three-browser-test` mounts a scene in headless Chromium (WebGL
through SwiftShader, so no GPU is needed): the canvas and the first frame,
a click on the mesh heard by the parent, a drag that turns the camera and is
no click, a colour patched into the material, and the disposal of a removed
mesh and of everything when the component goes, counted by wrapping Three.js's
`dispose`.
