# haskell-halogen

A port of [purescript-halogen](https://github.com/purescript-halogen/purescript-halogen/) library to GHC Haskell.

## Running the project

While the library itself compiles under any GHC (tested with 9.6-9.12) to compile the example app you need a `javascript-unknown-ghcjs-ghc-9.12.1`, cross-compiled GHC executable capable of compiling sources to JavaScript (9.10 works as well).

The easiest way to get it is to use `ghcup` precompiled binaries from [here](https://www.haskell.org/ghcup/guide/#cross-support), then run

```
./run_dev_server_minify.sh
```

to compile the JS sources and serve them on localhost. 

You may need to run `npm i -g http-server` if you don't have the `http-server` already installed.

## WebAssembly

With the GHC wasm toolchain installed (the version is pinned in
`cabal-wasm.project`), build the browser bundle and serve it with:

```
./serve-wasm.sh
```

Then open <http://127.0.0.1:8080>. The browser loader follows the same reactor
module and JSFFI post-linking setup as `paladyn-game`.

`serve-wasm.sh` rebuilds the bundle before starting `npx http-server`. Use
`./wasm.sh` directly when only a build is needed.

Run `./run_tests.sh` to exercise the native, JavaScript, and wasm targets.

## Pixi canvas example

The reusable `Halogen.Canvas` component owns the canvas lifecycle and abstracts
over rendering backends. `Halogen.Canvas.Pixi` supplies a PixiJS backend and a
declarative `Drawing event` monad with groups, atomic graphics primitives
(lines, rectangles, circles, ellipses, arcs, and Bézier curves), asset-backed
textures, and interactive sprites. Higher-level drawings such as grids are
ordinary Haskell composition rather than renderer primitives. Drawing events
are raised as typed component outputs; camera changes are reported separately.
The example loads its repository-owned `dev/pixi-tile.svg` through
`Texture "./pixi-tile.svg"`, exercising the same Pixi asset-loading path used
for application textures. It also loads a pinned WOFF2 font from jsDelivr with
`AssetFont`; the same `source` field accepts a relative application asset such
as `"./fonts/game.woff2"`. Use `SystemFont` when no loading is required.

Wrap stable scene items in `keyed key drawing`. Subsequent `View` inputs
reconcile those keys, retain their Pixi display objects and click handlers, and
only apply changed visual properties; removed keys are destroyed. Unkeyed
siblings receive automatic position keys at each scene-tree level, which keeps
duplicates distinct. Explicit keys are only needed when identity must survive
insertion, removal, or reordering. Camera transforms operate directly on the
retained scene. Pan changes are reported when the gesture ends, while wheel
changes are coalesced until the wheel burst has been idle for 120 ms.

Each mounted canvas owns its own Pixi `Application`, renderer, stage, event
system, and GPU canvas context. Browsers cache evaluation of the dynamically
imported Pixi module by URL, so multiple canvases reuse the same module and
Pixi asset cache rather than downloading/evaluating Pixi repeatedly.

The `halogen-pixi-example` executable is intentionally small: it constructs a
scene with the drawing monad and subscribes to its outputs. All mounting,
updates, pointer handling, rendering, and cleanup live in the library modules.

With the GHC wasm toolchain installed, build and serve it with:

```
chmod +x wasm-pixi.sh serve-wasm-pixi.sh
./serve-wasm-pixi.sh
```

The script opens <http://127.0.0.1:8080> automatically. The default renderer
loads its pinned PixiJS module from jsDelivr when mounted, so applications do
not need a Pixi JavaScript shim or global. The example requires network access
when opened. Use `componentWith (Config { moduleUrl = ... })` to load a
self-hosted or bundled PixiJS v8 module instead.

The development server scripts open the selected port in the default browser
automatically. Set `PORT` to choose another port, or `NO_OPEN=1` to suppress
opening the browser (for example in CI).
