# Revision history for haskell-halogen-core

## Unreleased

- The browser's own storage: `Web.Storage.Storage` for the Web Storage API,
  `Web.HTML.Window` for `localStorage`, `sessionStorage` and the viewport size,
  and `Web.HTML.Cookie` for cookies one at a time — the parsing and rendering
  of the cookie string are pure, and percent-encode what a cookie cannot carry.
  Off the browser backends a store holds nothing and keeps nothing, so a
  component written for the browser still runs against the in-memory DOM.
- `Halogen.Subscription.lowerEmitter` runs an emitter's registration in `IO`,
  which is what the driver subscribes in. `Halogen.Query.Event.eventListener`
  builds an emitter in the component's monad, so until now the two could not
  be put together.
- `Web.Event.Event` gains `preventDefault`, `stopPropagation` and
  `stopImmediatePropagation`, and every event newtype gains `toEvent`. Off the
  browser backends the three are no-ops rather than a `panic`, so a handler
  written for the browser can still be run against the in-memory DOM.
- **Fix.** A forked program is registered as running for as long as it runs.
  The bookkeeping that strikes a fork off the register was being run *before*
  the program instead of after it, which left `kill` and `join` with nothing to
  find — both were silently no-ops — and let a component's forks carry on after
  the component was finalized.

## 0.10.0 - 2026-09-14

- **Breaking.** `MonadDOM` no longer has an instance at `IO`. Each backend is
  its own newtype over `IO` — `BrowserDOM` for the browser, `MemDOM` for the
  in-memory document, `PixiDOM` in `haskell-halogen-pixi` — so more than one
  can exist in a single build, and a backend can say what its tree is made of.
  Applications that ran the driver directly should wrap the call:
  `runBrowserDOM HA.awaitBody >>= runUI component ()`.
- **Breaking.** The node types are associated type families on `MonadDOM`
  (`DomNode`, `DomElement`, `DomDocument`, `DomEventListener`,
  `DomEventTarget`), which is what lets a non-browser tree be a `MonadDOM` at
  all. The browser-specific operations — document splicing and the window
  globals — moved to `MonadBrowserDOM`, which carries the equalities back to
  the concrete `Node`, `Element` and friends as superclasses. Named attributes
  and properties are `MonadAttributes`, which only an HTML backend implements.
- **Breaking.** `MonadDOM` has `PrimMonad` as a superclass instead of its own
  mutable-cell operations; a `MutVar (PrimState m)` replaces the previous
  associated `Ref`. `log` was dropped.
- **Breaking.** The component monad is the DOM monad. `MonadDOM`,
  `MonadAttributes` and `MonadBrowserDOM` lift through transformers — the
  methods have defaults, and instances for `ReaderT` and `IdentityT` come with
  them — so an application monad is a stack over a backend and derives them:
  @newtype AppM a = AppM (ReaderT Config BrowserDOM a) deriving newtype
  (..., MonadBrowserDOM)@. `StateT` and friends are deliberately absent: a
  listener is called back by the DOM, so `mkEventListener` has to run the
  transformer, and a state update made inside a callback has nowhere to go.
  `runUI` therefore takes no natural transformations, and `runUIWith` is gone.
  `VDomSpec` lost its `dom` parameter and its `runDom` field; `buildProp`
  takes only the handler and the element.
- Add `Halogen.Canvas.Types`, `.Core`, `.Elements` and `.Properties`: a
  declarative scene language in the shape of `Halogen.HTML`, backend-neutral,
  and reconciled by `Halogen.VDom.DOM.buildVDom` like any other `VDom`. Shapes
  include an SVG `Path`, sharing the path commands in `Halogen.Svg.Attributes`.
  The cursor is an enumeration of the CSS keywords rather than free text: a
  misspelled keyword is silently ignored by the browser, which is the worst
  way to find out.
- Add `Clay.Extra.Pointer` with `touch-action`, which Clay does not cover.
  `Halogen.Canvas` styles its host element with Clay rather than a CSS string.
- Add `Halogen.Canvas`: a renderer-agnostic component that owns a canvas DOM
  node and delegates mounting, updating and teardown to a `Renderer` record.
  `haskell-halogen-pixi` implements that interface for PixiJS v8.
- Fix duplicate keys in `Halogen.VDom.Utils`. Two props with the same key, or
  two keyed children under the same key, each applied their effect while only
  one was recorded — and the build path recorded the first while the patch
  path recorded the last, so the second patch compared against the wrong one
  and settled on the losing value. Shadowed entries are now dropped before any
  effect runs, so the last one wins throughout. This affects HTML as much as
  the canvas.
- Re-export `~` from `HPrelude` via `Data.Type.Equality`.
- Move into the `haskell-halogen` monorepo alongside `haskell-halogen-material`
  and `haskell-halogen-pixi`. The library now lives in `core/`; the example app
  moved to `examples/vanilla`.

## 0.9.0 - 2025-05-30

- Update `clay`
- Insert `base` and `clay` version bounds
- Release version

## 0.8.0 - 2025-02-04

- Add `Halogen.Portal` for teleporting children across DOM elements (i.e. to body)
- Revert adding input to `Slot` type
- Add Java Swing - inspired CSS layout managers 

## 0.7.0 -- 2025-01-18

Remove dependency on `clock` which doesn't build on darwin

## 0.6.1 -- 2025-01-16

Fix `foreignToBool`

## 0.6.0 -- 2025-01-16

- Add `Halogen.Svg` modules
- Use `RequiredTypeArguments` for slot proxies

## 0.5.1 -- 2025-01-15

Fix `for` HTML property.

## 0.5.0 -- 2025-01-15

Allow monadic `initialState` function.

## 0.4.0 -- 2025-01-10

Fix unnecessary DOM replaces in foreign code

## 0.3.2 -- 2025-01-10

Add `-fexpose-all-unfoldings` to JS build and export `label` and `p` mistakenly hidden from `Halogen.HTML`

## 0.3.1 -- 2025-01-09

Use `isTrue#` for `reallyUnsafePtrEquality#`

## 0.3.0 -- 2025-01-08

Fix bugs related to style and class rendering, add IO specialisation, add `input` type to `H.Slot` to avoid `unsafeCoerce`.

## 0.2.0 -- 2024-12-28

More complete release, with events, properties and Clay integration

## 0.1.0 -- 2024-12-21

* First version. Released on an unsuspecting world.
