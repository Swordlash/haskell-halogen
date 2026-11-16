# haskell-halogen

[![CI Build](https://github.com/Swordlash/haskell-halogen/actions/workflows/build.yml/badge.svg)](https://github.com/Swordlash/haskell-halogen/actions/workflows/build.yml)

A port of [purescript-halogen](https://github.com/purescript-halogen/purescript-halogen/) to GHC Haskell, plus the component and rendering libraries built on top of it.

[The tutorial](https://swordlash.github.io/haskell-halogen/) lets you learn Halogen directly in the browser. Each lesson includes an explanation, an editor and a Compile button. GHC runs as WebAssembly in the page, compiles your code and mounts the component beside the lesson. No compilation server is needed.

Use the playground to write code without a lesson. Its Source button shows which line created the element under your pointer. The tutorial is itself a Halogen app; see [examples/learn](examples/learn/README.md).

AI usage disclaimer: All code until tag `0.9.0` was hand-written. Any later commits might have used Codex or Claude.

## Packages

| Directory | Package | What it is |
| --- | --- | --- |
| [core/](core/) | `haskell-halogen-core` | The Halogen port itself: components, VDom, events, SVG, layouts. |
| [hooks/](hooks/) | `haskell-halogen-hooks` | A port of `purescript-halogen-hooks`: a component as one function. |
| [store/](store/) | `haskell-halogen-store` | Global state: a port of `purescript-halogen-store`, one store changed by a pure reducer and components connected to the part they need. |
| [lazy/](lazy/) | `haskell-halogen-lazy` | Load components on demand with a placeholder. Supports shared libraries on WebAssembly under GHC's dynamic linker. |
| [remote/](remote/) | `haskell-halogen-remote` | Run components remotely and send DOM updates to a browser client, similar to Phoenix LiveView. Includes a WebSocket server and an in-page loopback. |
| [router/](router/) | `haskell-halogen-router` | Routing by the page's address: a port of `purescript-routing-duplex`, joined to the browser's history. |
| [net/](net/) | `haskell-halogen-net` | The browser's `fetch`: a request as a value, its response and body as values, aborted when the component waiting for it goes away. |
| [custom-element/](custom-element/) | `haskell-halogen-custom-element` | A component as a custom element (a Web Component) for any page, React's, Vue's or plain HTML: attributes and properties are its input, outputs are `CustomEvent`s, queries are methods. |
| [devtools/](devtools/) | `haskell-halogen-devtools` | Inspect the component tree, render counts and event log. Outline components and restore earlier states. |
| [worker/](worker/) | `haskell-halogen-worker` | Run expensive functions in Web Workers using static pointers and binary-encoded arguments and results. Falls back to local execution when workers are unavailable. |
| [tauri/](tauri/) | `haskell-halogen-tauri` | Tauri v2's IPC for an app in a Tauri webview: Rust commands with aeson JSON, events as emitters, the window's own events; inert in a plain browser. |
| [material/](material/) | `haskell-halogen-material` | Google Material Components bindings. |
| [pixi/](pixi/) | `haskell-halogen-pixi` | A PixiJS v8 canvas rendering backend. |
| [three/](three/) | `haskell-halogen-three` | Describe Three.js scenes with meshes, lights, camera controls and pointer events, using Halogen's reconciler. |
| [jsbits/](jsbits/) | `haskell-halogen-jsbits` | Compile-time embedding of shared browser JavaScript into WASM FFI. |
| [sound/](sound/) | `haskell-halogen-sound` | Music and sound effects for a page: a theme to loop, a shuffled album fetched ahead, effects on top. |
| [hspec-halogen/](hspec-halogen/) | `hspec-halogen` | Test components in a real browser with hspec: mount, click, type, check. |
| [playwright/](playwright/) | `haskell-halogen-playwright` | Test a built program from outside the page, in Haskell: Chromium driven through Playwright, every action waiting on the program's own "idle". |
| [th/](th/) | `haskell-halogen-th` | HTML and CSS as written, with Haskell inside: the `hh` and `clay` quasi-quoters. |
| [th-plugin/](th-plugin/) | `haskell-halogen-th-plugin` | Expand quasi-quotes during parsing so syntax and type errors point to the correct source location. |
| [examples/](examples/) | `halogen-example-*` | One runnable browser app per library, and `learn`, the tutorial, whose code compiles in the page. |

All packages build from the root `cabal.project`, so a change to `core` is checked against its dependents and every example in the same build.

`hooks`, `store`, `router`, `material`, `pixi`, `three`, `hspec-halogen`, `th`, `custom-element`, `devtools`, `lazy` and `remote` depend on `core`. `tauri` uses core's subscription emitters. `sound` and `worker` use `jsbits` without depending on core, and `playwright` is an independent native package that communicates with Halogen through the page's activity probe. Browser packages share JavaScript bindings through `jsbits`.

`th` supports native GHC and WebAssembly. It is disabled on the JavaScript backend because linking the `ghc` library for a quasi-quote exceeds the build's memory budget.

## The monad a component runs in

A component runs in the monad provided by its DOM backend. Each backend wraps `IO`: `BrowserDOM`, `MemDOM`, `PixiDOM` in `haskell-halogen-pixi`, or `ThreeDOM` in `haskell-halogen-three`. Multiple backends can coexist in one build. Associated type families define each backend's node and element types.

To add application-specific effects, wrap a backend in your own monad and derive the DOM classes:

```haskell
newtype AppM a = AppM (ReaderT Config BrowserDOM a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, PrimMonad, MonadDOM, MonadAttributes, MonadBrowserDOM)
```

Class methods have lifted defaults, so transformer instances usually need only associated types and `mkEventListener`. The library provides `ReaderT` and `IdentityT` instances. It deliberately omits `StateT` and similar transformers: a DOM callback must run the transformer, and there is no caller to receive state changes made inside that callback.

The classes describe backend capabilities. `MonadDOM` provides mutable nodes and event listeners, which are all the reconciler needs. `MonadAttributes` adds HTML attributes and properties. `MonadBrowserDOM` adds document operations and window globals, with superclass constraints tying its node types to the concrete browser `Node` and `Element` types.

## Building

Every package compiles natively under any GHC from 9.10 to 9.14; CI builds and tests on 9.10.3, 9.12.4 and 9.14.1. The browser targets need a cross-compiler.

```sh
npm install                  # once, for esbuild, sass and Material Components
npm run build-native         # every package, host GHC
npm run test                 # test suites across native, JavaScript and wasm
```

The cross-backend test scripts use Node 24 or later. They give each run a temporary Web Storage file so storage tests exercise the browser FFI as well as the native backend, without sharing a store between runs.

### WebAssembly

WebAssembly is the default browser and deployment target. First install the [ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta) toolchain. The build scripts source `~/.ghc-wasm/env` and require it to exist:

```sh
cd
git clone https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta.git
cd ghc-wasm-meta 
git checkout 358ea50b8496a69da6ce375c0c58bc049dbcb92d
SKIP_GHC=1 FLAVOUR=9.14 ./setup.sh
source ~/.ghc-wasm/env
ghcup -s "file://$HOME/ghc-wasm-meta/ghcup-wasm-0.0.9.yaml" install ghc "wasm32-wasi-9.14.1.20260731" --set -- $CONFIGURE_ARGS
```

That installs `wasm32-wasi-ghc` and friends under `~/.ghc-wasm`. The exact GHC version this repository builds against is pinned in `cabal-wasm.project`, and `.github/workflows/build.yml` pins the ghc-wasm-meta revision CI bootstraps from — keep the two in step when bumping either.

NOTE: use `cabal` version `3.16.1` on this repository.

With that in place, build and serve any example by name:

```sh
npm run serve-wasm -- pixi        # or: vanilla, hooks, material, th
npm run build-rootfs              # the tutorial's compiler: GHC and our libraries for the page
npm run build-wasm-all            # every example, size-optimised with wasm-opt, the tutorial with its compiler
npm run test-learn                # drive the built tutorial in headless Chromium, every lesson compiled
```

GitHub Pages serves `examples/learn`, with a separate prerendered `index.html` for each lesson and the playground. `haskell-halogen-router` handles paths such as `/02-counter/` and `/playground/`, and the compiler lives beside the app in `rootfs/`. Each route already contains its lesson HTML, so reloads, deep links and Back work on a static host without a 404 fallback.

[examples/learn/README.md](examples/learn/README.md) explains how to build, test and deploy the tutorial and add lessons. [learn/README.md](learn/README.md) explains how GHC runs in the browser. `test-learn` is a native suite in `examples/learn/browser-test`, using `haskell-halogen-playwright`. It requires host GHC and Playwright's Chromium (`npx playwright install chromium`).

`serve-wasm` opens <http://127.0.0.1:8080> automatically. Set `PORT` to choose another port, or `NO_OPEN=1` to suppress opening the browser (for example in CI).

For automatic reloads, install [ghciwatch](https://mercurytechnologies.github.io/ghciwatch/) and run `npm run dev -- <example>`, for example `vanilla`, `hooks`, `pixi`, `material`, `th` or `learn`. This loads the example in WebAssembly browser GHCi and opens <http://127.0.0.1:8080/assets/index.html>. Set `PORT` to use another port.

Saving an example module reloads it and reruns `main`. Changes to a library, Cabal file or the example's `web/` directory restart GHCi and open a new page. The page uses the example's `web/index.html`, with GHCi's script replacing `index.js`. Because the interpreter lives in that page, manually reloading it ends the session until GHCi restarts.

Use `npm run dev -- --repl <example>` to run GHCi without ghciwatch and call `:main` yourself.

### JavaScript backend

Needs a `javascript-unknown-ghcjs-ghc` cross-compiler; the easiest way to get one is the `ghcup` precompiled binaries described [here](https://www.haskell.org/ghcup/guide/#cross-support).

```sh
npm run serve-ghcjs -- vanilla    # cabal build + http-server
npm run build-js                  # material example, bundled into dist/ with esbuild
npm run build-js-dev              # the same, without minification or brotli
```

Build artifacts are kept in `dist-newstyle/native`, `dist-newstyle/javascript`, `dist-newstyle/wasm` and `dist-newstyle/wasm-dev` respectively.

## Adding an example

Create `examples/<name>/` with a `halogen-example-<name>.cabal` file, an executable named `halogen-example-<name>`, `Main.hs` and a `web/` directory. Put `index.html` and `index.js` in `web/`. The script should fetch `./app.wasm`, and the HTML should contain an empty `<div id="app"></div>` for `Main.hs` to find with `selectElement` and mount into.

The `examples/*` glob in `cabal.project` includes the package automatically, and `toolchain/build-wasm-all.sh` discovers its directory. If the example needs bundling, add a `bundle.sh` beside the Cabal file. The build script calls it with the output directory as its argument.

For `npm run dev`, copy the `interactive` flag used by the existing examples. It compiles the example's modules into the executable target so GHCi can reload them, and removes the reactor options and `hs_start` export.

Start `main` with `Halogen.IO.Util.endPreviousRun`, then register cleanup with `keepForNextRun`: usually the socket's `dispose` and, if applicable, `Halogen.Router.stop`. GHCi reuses the page between `:main` calls, so cleanup must remove the previous component before the next run mounts. See `examples/vanilla` for a basic setup and `examples/learn` for one with a router.

To show content before `app.wasm` loads, add a native executable named `halogen-example-<name>-prerender` and disable it for browser backends. Use `Halogen.Static.renderToHtml` to render the component into `#app` in `index.html`. `build-wasm.sh` builds this executable with host GHC and runs it as `<exe> examples/<name>/web/index.html <output dir>`.

`examples/vanilla/Prerender.hs` is a minimal example. `examples/learn/Prerender.hs` writes one page per route, using `HALOGEN_BASE_PATH` as the base path (`/` by default). In the browser, call `hydrateUI` instead of `runUI` to adopt the existing HTML and attach listeners.

For a build that has only the wasm toolchain, `Halogen.Static` also works as a standalone WASI command. Enable `wasi` in **both** `haskell-halogen-core` and `haskell-halogen-jsbits`, as `cabal-wasi.project` does. This selects MemDOM and inert browser bindings; it does not need JSFFI glue or a JavaScript engine at runtime. The vanilla example enables its prerenderer with the same flag:

```sh
mkdir -p dist-newstyle/wasi/prerender
sh toolchain/run-wasi.sh halogen-example-vanilla:exe:halogen-example-vanilla-prerender \
  examples/vanilla/web/index.html dist-newstyle/wasi/prerender
```

The script builds with the wasm compiler and runs the result through `wasmtime`, granting access to the repository directory. To use it for another project, enable its prerender executable on wasm, copy the two package flag settings, and use a separate build directory. Browser and WASI builds have different value representations and cannot share compiled core/jsbits modules. A component that needs a browser binding during initialization must provide a non-browser path, just as for native rendering (`inBrowser` is false).

WASI replaces the native prerender executable, not the need to compile the prerender's dependency closure: its MemDOM build remains separate from the browser build. It is useful on machines without native GHC; when both builds are already cached, measure the complete pipeline before switching for speed. The existing browser build script still uses its native prerender by default. `npm run test-wasi` checks the renderer and driver under standalone WASI.

Keep the example's component in `src/Example/<Name>.hs`, with `Main.hs` responsible only for starting it. GitHub Pages deploys the tutorial, so add a lesson in `examples/learn` to make the example available there. [Its README](examples/learn/README.md) explains how.

## Routing

`haskell-halogen-router` ports `purescript-routing-duplex`. A route codec parses URLs and prints routes back as URLs. For a sum type, combine one codec per constructor:

```haskell
data Route = Home | Post Int deriving stock (Generic)

route :: RouteDuplex' Route
route = root $ sum $ #Home .== noArgs .+ #Post .== path "post" (int segment)
```

`Halogen.Router` connects the codec to browser history. Create a router with `newRouter base fallback route`; `pageBase` reads the base path from `<base href>`. Use `current` to read the route, `navigate` to push an entry and `redirect` to replace one. The `changes` emitter reports these changes and Back/Forward navigation through `popstate`. `link` and `follow` handle links without reloading the page.

Use `newHashRouter` for fragment-based routing. On the native backend, the URL is stored in memory so `Halogen.Static` can render each route, as the tutorial's prerenderer does.

## Global state

`haskell-halogen-store` ports `purescript-halogen-store`. The application has one store, updated by actions through a pure reducer. Components declare a `MonadStore` constraint, call `updateStore` to dispatch actions, and use selectors to subscribe to specific values. A connected component renders again only when its selected value changes:

```haskell
data Store = Store {count :: Int, name :: Text}
data Action = Increment | Rename Text

countView :: forall q o m. (Store.MonadStore Action Store m) => H.Component q () o m
countView = Store.connect (Store.selectEq (.count)) display   -- display gets a Store.Connected Int ()

main = runBrowserDOM $ do
  root <- Store.runStoreT (Store 0 "Ada") reduce App.component
  ...
```

The component tree uses `StoreT`, which `runStoreT` removes before `runUI` mounts it. `Store.subscribe` converts selected value changes into component actions. Store operations use `H.liftEffect`, so updates and notifications happen within the action that triggered them. See [store/README.md](store/README.md) and tutorial lesson 15.

## Components that arrive later

`haskell-halogen-lazy` displays a placeholder while a component loads. Put `Lazy.lazy loading loader` in a slot: it shows `loading.placeholder`, runs the loader, then mounts the loaded component with its normal input, outputs and queries. This is similar to React's `lazy` and `Suspense`. Use `Lazy.memo` to share one load across mounts.

```haskell
HH.slot "editor" () (Lazy.lazy Lazy.defaultLoading loadEditor) state.document Edited
```

`Halogen.Lazy.Dyld.load` can load a component from a shared library when the WebAssembly program was started with GHC's dynamic linker. The page fetches the library, dyld links it, and the entry point returns the component. Tutorial lesson 18 demonstrates this. [lazy/README.md](lazy/README.md) explains the setup and its costs.

## The DOM over a wire

`haskell-halogen-remote` runs components outside the page that displays them, similar to Phoenix LiveView. Its `RemoteDOM` backend mirrors the page and records DOM operations. A session sends those operations as JSON to `remote/jsbits/remote.js`, and the client sends browser events back to the component. Existing polymorphic components work unchanged; the entry point chooses `RemoteDOM`.

```haskell
Server.serve Server.defaultSettings {Server.pages = [("/", "web/live.html", "text/html")]} board ()
```

`Halogen.Remote.Server` serves components over WebSockets, with one session per page. `Halogen.Remote.Loopback` runs the same pipeline inside one page and exposes the messages for inspection, as tutorial lesson 21 demonstrates. [remote/README.md](remote/README.md) describes the protocol, supported DOM operations and limitations. `examples/live` implements a board shared across tabs.

## Fetching

`haskell-halogen-net`'s `Halogen.Net.Fetch` is the browser's `fetch`. A component waits for a response in `lift`, and the request is aborted if the component goes away first:

```haskell
Load -> do
  outcome <- fetch (request GET "/api/notes") >>= either (pure . Left) text
  modify $ \st -> st {notes = outcome}
```

`start` begins a request inside an event's dispatch (`H.liftEffect`) and gives a handle to wait for or abort later. A rejection (no response, or an abort) is a `Left JSError`; an HTTP error status is a response whose `ok` is `False`.

## Custom elements

`haskell-halogen-custom-element` makes a component a custom element, which a page that is not a Halogen app uses as it uses any element: plain HTML, React, Vue, Svelte, Astro, HTMX.

```haskell
registration <-
  CE.define "x-counter"
    CE.defaultDefinition
      { input = \props -> fromMaybe 0 (CE.prop "start" props)
      , observed = ["start"]
      , output = \n -> Just ("count", toJSON n)
      , methods = [CE.tellMethod "reset" (\() -> Reset)]
      }
    counter
HA.keepForNextRun registration.dispose
```

Each `<x-counter>` mounts the component in its own shadow root. Changes to observed attributes or properties arrive through `H.onReceive`. Component outputs become `CustomEvent`s, and `counter.reset()` sends a query.

Removing the element disposes its component; moving it preserves state. Defining the same name again replaces the component in every matching element. Use `CE.onEvent` to decode the `detail` of events from other custom elements. `examples/custom-element` shows a plain JavaScript page controlling two Halogen elements.

## Watching a program: the trace and the panel

When a listener subscribes to `Halogen.Trace`, component trees report mounts, disposal, render timings, actions, state changes, queries, outputs, subscriptions and forks. Each event includes the component ID, parent ID and label. The default label is the state type's name; override it with `H.named`. Without listeners, the driver does not construct trace events.

```haskell
stop <- Trace.subscribe $ \event -> putStrLn (show event.tag.id <> " " <> Trace.summary event.what)
```

`haskell-halogen-devtools` displays the trace in a panel. `DevTools.open` shows the component tree, counters and recent events. Hovering a row outlines its component on the page. Register a state type with `DevTools.showing @State` to display its values and restore earlier states with `Trace.restoreState`. `hspec-halogen` also uses the trace to explain test failures. Tutorial lesson 16 opens the panel beside the reader's component.

```haskell
main = runBrowserDOM $ do
  DevTools.showing @Counter.State
  tools <- DevTools.open
  body <- HA.awaitBody
  socket <- runUI Counter.component () body
  HA.keepForNextRun (socket.dispose >> tools.dispose)
```

## Work off the page's thread

A long calculation can freeze the page because GHC's WebAssembly threads share its JavaScript thread. `haskell-halogen-worker` moves the calculation to a Web Worker running another copy of the program. The worker finds the job by its GHC static pointer. Arguments and results are encoded with `Data.Binary` and sent in transferred `ArrayBuffer`s:

```haskell
primes :: StaticPtr (Worker.Job '[Int] Int)
primes = static (Worker.job (\n -> pure (countPrimes n)))

primesBetween :: StaticPtr (Worker.Job '[Int, Int] Int)
primesBetween = static (Worker.job (\lo hi -> pure (countBetween lo hi)))

main = Worker.program $ runBrowserDOM $ do
  pool <- Worker.newPool 2
  ...
-- and in a component:
  n <- liftIO (Worker.run pool primes 2000000)
-- or several at once, across the pool's workers:
  counts <- liftIO (mapConcurrently (uncurry (Worker.start pool primesBetween)) parts)
```

A job's argument types are given as a type-level list (needs `DataKinds`). Pass the arguments to `Worker.run` and `Worker.start` after the job, in order. `Worker.program` handles jobs inside a worker and starts the UI on the page. Configure the program's files in `globalThis.halogenWorker` before startup; see [worker/README.md](worker/README.md). Without that configuration, on native GHC or in GHCi, jobs run inside the current program using the same encoding.

Use `Worker.cancel` to stop a job at its next `Worker.checkpoint`, or `Worker.kill` to terminate its worker immediately. `Worker.run` cancels its job if its wait is interrupted. For many independent tasks, `Halogen.Worker.Par` batches them across the pool: `runPar pool (traverse (uncurry (task primesBetween)) parts)`.

`examples/worker` compares prime counting on the page, in one worker, across two workers and as sixteen batched tasks. It also demonstrates cancellation, termination and races while a spinner shows UI responsiveness. Tutorial lesson 17 teaches the API.

## Tauri

`haskell-halogen-tauri` is for an app that runs in a [Tauri](https://tauri.app) v2 webview (with `app.withGlobalTauri`): `invoke` calls a Rust command with aeson-encoded arguments and decodes its result, `listen` is an emitter of an event's payloads for `H.subscribe`, and `emit` sends one.

```haskell
Initialize -> void (H.subscribe (Progress <$> listen "download-progress"))
Save -> do
  outcome <- invoke "save_note" SaveNote {noteId = 7, text = "…"}
  modify $ \st -> st {saved = outcome}
```

In a plain browser `isTauri` is `False`, a command gives `Left NotInTauri` and an emitter never fires, so one program runs in both.

## Hooks

`haskell-halogen-hooks` writes a component as one function from its input to its HTML, asking for state, effects, memoised values and a query handler as it goes, instead of spreading them across `initialState`, `render` and `handleAction`:

```haskell
counter :: H.Component H.VoidF () Void BrowserDOM
counter = Hooks.component @Empty $ \_input -> Hooks.do
  (count, countId) <- Hooks.useState (0 :: Int)

  Hooks.useTickEffect count $ do
    liftIO $ putStrLn ("count is now " <> show count)
    pure Nothing

  Hooks.pure $
    HH.div_
      [ HH.button [HE.onClick $ \_ -> Hooks.modify_ countId (+ 1)] [HH.text "more"]
      , HH.text (show count)
      ]
```

`Hooks.do` uses `QualifiedDo` because hook programs are indexed by a type-level list of their hooks. This enforces consistent hook order at compile time: putting `useState` inside a conditional does not type-check. The interpreter uses the same list to access its state cells. Define a composite hook as a type synonym over that list:

```haskell
type UseCounter hooks = UseState Int : UseEffect Int : hooks
```

`Halogen.Hooks.Extra.Hooks` includes [purescript-halogen-hooks-extra](https://github.com/JordanMartinez/purescript-halogen-hooks-extra) in the same package. It provides `useDebouncer`, `useThrottle`, `useGet`, `useEvent`, the `useStateFn` family and event helpers such as `preventDefault`. `usePrevious` and the `useLocalStorage` family come from the original library's examples.

Storage uses `MonadBrowserDOM`, so the in-memory backend supports it too. Each key has a prefixed entry containing a base64 value. `Web.Storage.Serialize` defines how values become bytes, using JSON by default for types with aeson instances. Each extra hook is composed from the basic hooks and can serve as an implementation example. [examples/hooks/](examples/hooks/) demonstrates all of them.

This port uses GHC's type system for several guarantees that the PureScript original handles at runtime. The hook list and cell store share a type-level index, so cells are read at their original types without coercion. State handles are tied to their owning component, much like `STRef` values are tied to an `ST` region, and cannot escape through outputs or longer-lived storage.

Effect and memo dependencies are ordinary values compared with `==`. Use `useTickEffectBy` or `useMemoBy` for a custom comparison. For example, `Hooks.captures {x, y} Hooks.useTickEffect` becomes `Hooks.useTickEffect (x, y)`. The query algebra is also part of the hook program's type, so there is no separate `componentWithQuery` or query token.

## Canvas rendering

`Halogen.Canvas` (in `core`) is a component that owns a canvas DOM node and delegates to a `Renderer` record, which a backend implements by supplying `mount`, `update` and `destroy`.

Describe scenes with `Halogen.Canvas.Elements` and `Halogen.Canvas.Properties`. Their API resembles Halogen HTML: elements include `group`, `line`, `rectangle`, `circle`, `ellipse`, `arc`, Bézier curves, `path`, `text` and `sprite`. Each takes properties and has a `_` variant without styling. Paths use the same commands from `Halogen.Svg.Attributes` as an SVG `d` attribute.

Properties control transforms, cursors, hit areas and pointer handlers: `onClick`, `onPointerDown`, `onPointerUp`, `onPointerOver`, `onPointerOut` and `onPointerMove`. `outline` draws a border using the backend's measured hit-test bounds. This lets it size labels and sprites correctly, including after textures load, and shows the clickable area.

Build higher-level drawings such as grids with ordinary Haskell functions. Handler actions become typed component outputs; camera changes are reported separately.

The vocabulary the scene is written in (`Halogen.Canvas.Types`) is backend-neutral, and a scene is an ordinary `VDom`, so it reconciles through `Halogen.VDom.DOM.buildVDom` — the same machinery that reconciles HTML. `haskell-halogen-pixi` supplies a PixiJS v8 interpretation of it: a `MonadDOM` instance whose nodes are Pixi display objects, plus a prop applicator that paints them.

Give stable scene items keys with `keyedGroup` or `withKeys`. Subsequent `View` inputs reconcile those keys, retain their display objects and listeners, and only repaint what changed; removed keys are destroyed. Unkeyed siblings are matched by position, so explicit keys are only needed when identity must survive insertion, removal or reordering. Camera transforms operate directly on the retained scene. Pan changes are reported when the gesture ends, while wheel changes are coalesced until the wheel burst has been idle for 120 ms.

Each mounted canvas owns its own Pixi `Application`, renderer, stage, event system and GPU canvas context. Browsers cache evaluation of the dynamically imported Pixi module by URL, so multiple canvases reuse the same module and Pixi asset cache rather than downloading and evaluating Pixi repeatedly.

The default renderer loads its pinned PixiJS module from jsDelivr when mounted, so applications need no Pixi JavaScript shim or global — but the example does need network access when opened. Use `componentWith (Config { moduleUrl = ... })` to load a self-hosted or bundled PixiJS v8 module instead.

### In three dimensions

`haskell-halogen-three` renders three-dimensional scenes with Three.js. `Halogen.Three.Elements` and `Halogen.Three.Properties` describe groups, meshes, materials, lights and pointer handlers. The renderer uses raycasting to find the object under the pointer.

Geometry and material are mesh properties. Colour changes update the material; shape changes replace and dispose the geometry. `Halogen.Three.component` uses core's canvas component with this renderer. Dragging orbits the camera, scrolling zooms, and `CameraMoved` reports the resulting position. The renderer draws once in the frame after a change.

```haskell
scene :: Three.View ()
scene =
  Three.view Three.defaultCamera Three.defaultInteraction
    [ TE.ambientLight 0xffffff 0.6
    , TE.directionalLight 0xffffff 2.5 (Three.Vec3 3 5 4)
    , TE.mesh (Three.Sphere 1 48 24) (Three.standard 0xf59e0b) [TP.onClick (\_ -> Just ())]
    ]
```

Like the Pixi renderer it imports a pinned Three.js from jsDelivr when a scene mounts; give `componentWith` a `Config` to load it from elsewhere. Lesson 19 of the tutorial is a scene.

## Releases

One repository, one tag namespace: releases are tagged with a package prefix, such as `core-v0.10.0` or `material-v0.2.0`. Each package keeps its own `CHANGELOG.md` and uploads to Hackage separately.

### Shared browser JavaScript

Both browser backends use the same files under each package's `jsbits/`. The JavaScript backend links them with `js-sources`; WASM embeds them at compile time with [Halogen.JSBits](jsbits/README.md), leaving only callback and representation adapters specific to the backend. Material keeps its existing bundled JS module.

WASM consumers must set `shared: True` in their Cabal project so Template Haskell can load its dependencies. When using a Git source dependency, include `jsbits` alongside the requested packages in `subdir`. No runtime asset loading is added.
