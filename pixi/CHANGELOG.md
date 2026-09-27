# Revision history for haskell-halogen-pixi

## Unreleased

* Touch: two pointers pinch the camera. It zooms by how far apart they move,
  within the scene's `zoomRange`, and pans so that the world point between
  them stays between them. With one pointer it pans as before, and a finger
  left down after a pinch pans on from where it is. `CameraChanged` is raised
  once, when the last pointer lifts.
* A scene may hold memoized parts (`Halogen.Canvas.Elements.memoized`,
  `lazy`): the renderer builds them as thunks, and while a part's input is
  equal a render skips building and diffing it and updating its Pixi
  objects' props. Pixi still draws them each frame as before.
* **Fix.** Text in an `AssetFont` is no longer cut off. Pixi measures a font's
  ascent and descent once per font string and caches them, and the asset's
  family was named as soon as the text was set, so Pixi measured it before the
  face had loaded, from the fallback standing in for it, and kept that: a face
  with a taller ascent lost the tops of its glyphs. The text is now drawn in the
  generic `sans-serif` until the face has loaded, and only then in the asset's
  family.

## 0.1.0.0

* Initial release. A PixiJS v8 backend for `Halogen.Canvas`: a `MonadDOM`
  instance over a Pixi display list, so the scene language in
  `Halogen.Canvas.Elements` reconciles through the same vdom machinery as
  HTML, with per-object pointer events and camera pan/zoom.
