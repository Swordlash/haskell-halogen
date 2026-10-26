# Revision history for haskell-halogen-three

## Unreleased

The first release. A component can describe a three-dimensional scene as it
describes HTML, and have it rendered with Three.js: `Halogen.Three.Elements`
and `Halogen.Three.Properties` build a `VDom` of groups, meshes and lights,
which the reconciler that renders HTML builds and patches, through a
`MonadDOM` instance whose nodes are Three.js objects. `Halogen.Three.component`
is core's `Halogen.Canvas.component` over this renderer, so a scene is a child
component whose input is the view, as a PixiJS canvas is.

A mesh's geometry and material are props of its node: a change of colour
patches the material, a change of shape replaces the geometry, and whatever is
replaced or taken out is disposed of, since Three.js keeps GPU resources until
told. Pointer events are raycast from the canvas into the scene and handed to
the nearest mesh, or to the nearest of its ancestors listening; a drag turns
the camera about its target and the wheel zooms, and `CameraMoved` reports the
camera once the gesture is over. The scene is drawn on demand, once in the
frame after a change, and follows its parent's size.

Three.js is imported from jsDelivr (version 0.186.1, its single-file ES module
build) when a scene mounts, unless the `Config` names another module.
