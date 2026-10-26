title: A scene in three dimensions
layers: three
args: -package haskell-halogen-three

Lesson 11 drew on a canvas with PixiJS through the same machinery that renders HTML. `haskell-halogen-three` does it in three dimensions, with Three.js: `Halogen.Three.Elements` (`TE`) and `Halogen.Three.Properties` (`TP`) are the elements and properties, `TE.mesh`, `TE.ambientLight`, `TE.directionalLight`, `TP.position`, `TP.rotation`, `TP.onClick`, and what they build is again a `VDom` tree, reconciled as a render's HTML is. Only the last step differs: each node becomes a Three.js object, a mesh or a light, instead of a DOM element.

## The scene

`scene` is a function from the state to a `Three.View`: a camera, how the reader may move it, and the nodes. `Three.defaultCamera` stands above and to the right of the origin and looks at it; `Three.defaultInteraction` lets you drag to turn the camera about what it looks at, and scroll to come nearer or go further. Three.js's axes have y pointing up, so the floor, a `Three.Plane`, is turned by `-pi / 2` about x with `TP.rotation (Three.Euler (-pi / 2) 0 0)` to lie flat, a unit below the origin.

A mesh is a shape and what it is made of: `TE.mesh (Three.Sphere 1 48 24) (Three.standard (hex st.colour)) [...]` is a sphere of radius 1, made of `Three.standard`, a material that light falls on. Without a light it would be black, so the scene has two: `TE.ambientLight`, light from everywhere at once, which shows the sides the other lights do not reach, and `TE.directionalLight`, light from far away like the sun's, from where `Three.Vec3 3 5 4` is, towards the origin.

The shape and the material are properties of the mesh's node. When you click the sphere and its colour changes, the reconciler compares the new material with the last one and only the material's colour is changed: the sphere keeps its geometry, and nothing else in the scene is touched.

`Three.component` is a child component, in the slot `"scene"`, whose input is the scene, as `Pixi.component` was lesson 11's. A handler, `TP.onClick (\_ -> Just ())`, makes it raise `Three.Fired ()`, which arrives as `Scene (Three.Fired ())`. Three.js has no events of its own: the library casts a ray from the pointer into the scene and hands the click to the nearest mesh the ray meets. A drag that turns the camera is not a click, and when it ends the component raises `Three.CameraMoved`, which this one ignores.

Three.js is not in the layer: the library imports it from a CDN when the scene mounts, so the scene appears a moment after the compile.

## Exercise

Bring the scene to life:

1. a program `startFrames`, given to `H.onInitialize`, subscribes to the screen's frames, as lesson 11's ball glided: `void (H.subscribe (Frame <$ Window.animationFrame))`, with `Halogen.Query.Event.Window qualified as Window`;
2. the state gets an `angle`, which each `Frame` increases a little, and the cube turns by it about the vertical axis: `TP.rotation (Three.Euler 0 st.angle 0)`;
3. the sphere floats up and down with the angle, at `TP.position (sphereAt st)`, where `sphereAt st = Three.Vec3 0 (0.4 * sin (2 * st.angle)) 0`;
4. a `TE.pointLight`, a lamp shining in every direction, follows it a little above. Its light falls off with the square of the distance, as a real lamp's does, so it needs an intensity of a few units: `TE.pointLight 0xfff1c1 8 (above (sphereAt st))`.

The scene is drawn again only when something in it changes, once in the next frame: now that is every frame, and the drag still turns the camera while the cube spins.
