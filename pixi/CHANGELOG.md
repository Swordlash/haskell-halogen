# Revision history for haskell-halogen-pixi

## Unreleased

* **Fix.** Text in an `AssetFont` is no longer cut off. Pixi measures a font's
  ascent and descent once per font string and caches them, and it first
  measured an asset font before the face had loaded, from the fallback that
  stood in for it; the texture stayed sized for the fallback, so a face with a
  taller ascent lost the tops of its glyphs. The cached metrics are now
  cleared when the face arrives, before the text is restyled.

## 0.1.0.0

* Initial release. A PixiJS v8 backend for `Halogen.Canvas`: a `MonadDOM`
  instance over a Pixi display list, so the scene language in
  `Halogen.Canvas.Elements` reconciles through the same vdom machinery as
  HTML, with per-object pointer events and camera pan/zoom.
