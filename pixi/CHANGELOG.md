# Revision history for haskell-halogen-pixi

## 0.1.0.0

* Initial release. A PixiJS v8 backend for `Halogen.Canvas`: a `MonadDOM`
  instance over a Pixi display list, so the scene language in
  `Halogen.Canvas.Elements` reconciles through the same vdom machinery as
  HTML, with per-object pointer events and camera pan/zoom.
