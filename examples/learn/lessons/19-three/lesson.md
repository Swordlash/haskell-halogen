title: A scene in three dimensions
layers: three
args: -package haskell-halogen-three

`haskell-halogen-three` renders three-dimensional scenes with Three.js. As with Pixi in lesson 11, elements and properties build a `VDom` tree. The reconciler updates Three.js objects instead of HTML elements.

Import `Halogen.Three.Elements` as `TE` and `Halogen.Three.Properties` as `TP` for meshes, lights, transforms and event handlers.

## The scene

`scene` converts state into a `Three.View` containing a camera, interaction settings and nodes. `Three.defaultCamera` looks at the origin from above and to the right. `Three.defaultInteraction` lets you orbit by dragging and zoom by scrolling.

The y-axis points upward. To make the floor horizontal, rotate `Three.Plane` around x with `TP.rotation (Three.Euler (-pi / 2) 0 0)` and place it one unit below the origin.

A mesh combines geometry and material. `TE.mesh (Three.Sphere 1 48 24) (Three.standard (hex st.colour)) [...]` creates a sphere of radius 1 with a lit material. The scene uses `TE.ambientLight` to illuminate all sides and `TE.directionalLight` at `Three.Vec3 3 5 4` for sunlight-like lighting toward the origin. Without lighting, the standard material would appear black.

Geometry and material are mesh properties. Clicking the sphere changes only the material's colour; its geometry and other scene objects stay in place.

`Three.component` receives the scene through the `"scene"` slot. `TP.onClick (\_ -> Just ())` produces `Three.Fired ()`, handled as `Scene (Three.Fired ())`. The renderer finds the clicked mesh by raycasting from the pointer. Camera drags do not count as clicks; they emit `Three.CameraMoved`, which this example ignores.

Three.js loads from a CDN on mounting, so the scene may appear shortly after compilation.

## Exercise

Animate the scene:

1. Add `startFrames` to `H.onInitialize`. Import `Halogen.Query.Event.Window qualified as Window` and subscribe with `void (H.subscribe (Frame <$ Window.animationFrame))`.
2. Add an `angle` to state and increase it on each `Frame`. Rotate the cube with `TP.rotation (Three.Euler 0 st.angle 0)`.
3. Move the sphere with `TP.position (sphereAt st)`, where `sphereAt st = Three.Vec3 0 (0.4 * sin (2 * st.angle)) 0`.
4. Add a point light just above it: `TE.pointLight 0xfff1c1 8 (above (sphereAt st))`. Its intensity falls with squared distance, so it needs enough intensity to illuminate nearby objects.

The renderer draws after scene changes, once per frame. Your frame updates now animate the objects while camera dragging remains available.
