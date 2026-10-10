# Revision history for haskell-halogen-pixi

## Unreleased

`Halogen.Canvas.Pixi.FFI` declares its bindings once, without CPP; natively
they are `browserJS`'s inert stubs. `Callback` is `Halogen.JSBits.Callback`,
and `Application`, `Object` and `Timer` have `Inert` instances.

The JavaScript and WebAssembly backends now use the same browser jsbits, embedded
at compile time for WASM. Applications need no additional scripts. WASM builds
must enable shared dependencies for Template Haskell (`shared: True` in the
WASM project); Git source dependencies must also include the `jsbits` package.

- `cacheAsTexture` on a group: Pixi's `cacheAsTexture`, kept up to date. Pixi
  does not notice changes inside a cached group; here every change the
  reconciler makes to a prop, a child added or removed, and a texture or
  font finishing its load marks the cached groups above it, which are drawn
  again before the next frame, once a frame at most. In Paladyn, caching
  the fog of war (hundreds of cloud shapes) took a pan from 7 to 13 frames a
  second (Chromium, software GL, 4× slowed CPU), and the main thread's work
  in it from about 430 to 310 ms.
* **Fix.** Two fingers put down together no longer make the camera jump.
  Pixi hands one event object on from event to event, and the pointer's id
  and position were read from it lazily, after it already held the other
  finger's event: a pinch could start from a wrong distance and zoom far in or
  out at the first move. Every callback now gets a copy of the event made as
  it is dispatched, so a handler's reads (`stagePosition`, `localPosition`,
  `pointerId`, …) are of its own event however late they happen. On the
  JavaScript backend the callbacks are now entered synchronously, as the DOM
  backend's are, so `preventDefault` on a wheel event takes effect.
* `PolygonHit` hit areas, as a Pixi `Polygon`.
* Touch: two pointers pinch the camera. It zooms by how far apart they move,
  within the scene's `zoomRange`, and pans so that the world point between
  them stays between them. With one pointer it pans as before, and a finger
  left down after a pinch pans on from where it is. `CameraChanged` is raised
  once, when the last pointer lifts.
  A scene that zooms but does not pan (`pan = False`) is pinched too, about
  one point for the whole pinch (between where the two fingers went down).
  `CameraChanged` is raised only when the camera has actually moved.
* A pointer moves the camera only once it has gone 8 screen pixels from where
  it went down, so a tap with a trembling finger stays a tap. The tap that
  ends a pan or a pinch is not delivered to `PointerTap` handlers, for any
  finger of it, not only the last to lift: releasing
  a dragged map over an element no longer counts as tapping it.
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
