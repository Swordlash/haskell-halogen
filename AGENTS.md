# AGENTS.md

This file provides guidance to AI coding agents working with code in this repository.

## What this is

A port of purescript-halogen to GHC Haskell, as a multi-package cabal project: `core`
(`haskell-halogen-core`, the port itself), `jsbits` (compile-time shared JavaScript embedding), `hooks` (`purescript-halogen-hooks` port), `store` (`purescript-halogen-store` port: one store, a pure reducer, components connected to the part of it they need), `router` (`purescript-routing-duplex` port, with the
browser's history), `net` (the browser's networking: `fetch`, a thin binding on
`Halogen.JSBits.Promise` that aborts its request when the waiting fiber is killed, and
WebSocket and server-sent events, whose events arrive through `Halogen.Subscription` emitters),
`custom-element` (`haskell-halogen-custom-element`: a component as a custom element, a Web
Component any page can use, its attributes and properties the component's input, its outputs
`CustomEvent`s and its queries methods; and `onEvent` for others' custom elements), `devtools`
(`haskell-halogen-devtools`: a panel over the page, read from core's `Halogen.Trace`, with the tree
of components, their counts, a log of events, the hovered component outlined and earlier states
put back), `tauri`
(Tauri v2's IPC through its global API `window.__TAURI__`: commands with aeson JSON, events as
emitters, the current window's events), `material`
(Material Components bindings), `pixi` (PixiJS v8 canvas backend), `sound` (music and sound
effects: a player over an abstract backend, and a browser backend), `hspec-halogen`
(an hspec harness that tests components in a real browser), `th` (`haskell-halogen-th`: the `hh`
and `clay` quasi-quoters, HTML and CSS with Haskell inside, parsed by ghc-lib-parser), and
`examples/*` (one browser app each), among them `examples/learn`, the tutorial and what GitHub Pages
deploys: lessons as routes, each with its text, an editor and the result of compiling the reader's code
in the page. `learn/` is no package: it is GHC itself running in a page, compiling and mounting a
component the reader wrote (the compiler the tutorial runs on, the rootfs, and a bare page for trying it;
its `README.md` has the rootfs, its manifest and the measurements). `hooks`, `store`, `router`, `net` (for `Halogen.Subscription`), `material`, `pixi`, `hspec-halogen` and `th` depend only on `core`; `tauri` on `core` (for `Halogen.Subscription`'s emitters) and `jsbits`; `custom-element` and `devtools` on `core` and `jsbits`; `sound` depends on none of them, only on `jsbits`. Everything builds from the root
`cabal.project`, so a `core` change is type-checked against every dependent and example.

Every package is compiled for three targets: native GHC, the GHC JavaScript backend, and GHC
WebAssembly (the default browser and deployment target), except `th`: `ghc-lib-parser` does not
build with the JavaScript backend, so `th` is not buildable there and `examples/th` shows a note
instead (its `src-js/`).

## Commands

```sh
npm run build-native            # all packages, host GHC, into dist-newstyle/native
npm run test-native             # native test suites
npm run test-ghcjs              # toolchain/test-ghcjs.sh — JS backend
npm run test-wasm               # toolchain/test-wasm.sh — wasm backend
npm run test                    # toolchain/run-tests.sh — all three in sequence
npm run serve-wasm -- <example> # build one example to wasm and serve it on :8080
npm run build-rootfs            # the compiler the tutorial runs (GHC, our libraries) and learn/'s page, into dist-newstyle/wasm/learn
npm run build-wasm-all          # every example, wasm-opt'd (the rootfs first if missing); Pages deploys only public/learn
npm run test-learn              # the built tutorial, rootfs inside, in headless Chromium: every lesson compiled in the page
npm run test-playground         # learn/'s page on the rootfs in headless Chromium: a program for every layer
npm run build-js                # JS backend: build all, bundle the material example into dist/
npm run dev -- <example>        # an example in browser GHCi, reloaded and rerun on save
npm run dev-test -- <package>   # a package's browser test suite in browser GHCi, rerun on save
npm run format                  # fourmolu over core, hooks, store, router, net, custom-element, devtools, tauri, pixi, hspec-halogen, th, material/test, examples
```

`core`, `hooks`, `store`, `router`, `net`, `tauri` and `sound` have test suites that run under Node (`Halogen-core-test`,
`Halogen-hooks-test`, `Halogen-store-test`, `Halogen-router-test`, `Halogen-net-test`, `Halogen-tauri-test`, `Halogen-sound-test`), and `material`, `net`, `custom-element`, `devtools`, `tauri` and `hspec-halogen` have ones that run in a browser
(`Halogen-material-test`, `Halogen-net-browser-test`, `Halogen-custom-element-test`, `Halogen-devtools-test`, `Halogen-tauri-browser-test`, `hspec-halogen-test`; on wasm and JS, see below).
`Halogen-net-browser-test` fetches the files in `net/test/web/` from the runner's server, and
what that server cannot do (a request held until the test lets it go, a body that stops halfway,
a response echoing its request) from `net/test/web/network-double.js`, which wraps the page's
`fetch`; its WebSocket and EventSource tests drive `net/test/web/socket-double.js`, which stands in
for the servers for addresses on the host `double` and leaves every other address to the browser.
`Halogen-tauri-browser-test` runs against `tauri/test/web/tauri-double.js`, a stand-in
for Tauri's global API (a table of commands and an event bus), since there is no Tauri in CI.
`hspec-halogen-test` is also
the harness's examples, and where core's browser behaviour (events, properties the page changes,
keyed moves, refs, forks, subscriptions, the window's emitters) is tested for real. All are hspec, `main-is: Test.hs` (net's and tauri's browser suites: `Browser.hs`) which aggregates `Test.*` specs. Run one suite or one test with hspec's
`--match`:

```sh
cabal test haskell-halogen-core:test:Halogen-core-test --builddir=dist-newstyle/native \
  --test-options='--match "/storage/"'
sh toolchain/test-wasm.sh --test-options='--match "/GHCJS FFI/"'   # extra args pass through to cabal
```

A new test module must be added to `other-modules` of the test suite in the package's `.cabal` file
and wired into `test/Test.hs`.

### Toolchain pins (these cause most local failures)

- Use **cabal 3.16.1.0** (CI's `CABAL_VERSION`). cabal 3.18 rejects the pinned wasm GHC: its
  `--info` reports a `ghc-internal Unit Id` that differs from the one in its package DB, so the
  solver fails with "requires installed instance with unit id ghc-internal-…".
- wasm GHC: `wasm32-wasi-ghc-9.14.1.20260731` (pinned in `cabal-wasm.project`); ghc-wasm-meta revision
  pinned in `.github/workflows/build.yml` — keep them in step. Scripts source `~/.ghc-wasm/env`.
- JS GHC: `javascript-unknown-ghcjs-ghc-9.12.2` (pinned in `cabal-ghcjs.project` and `GHCJS_VERSION` in
  CI).
- Node 24+ for the cross-backend tests.
- Always go through the `toolchain/` scripts for cross builds: they pass `--with-compiler`/`--with-hc-pkg`
  explicitly and delete `<builddir>/cache/compiler`, because cabal otherwise reuses the host `ghc-pkg`
  when switching toolchains. Build dirs: `dist-newstyle/{native,javascript,wasm}`.

### How cross-backend tests run

- Test binaries run under Node via `--test-wrapper`: `toolchain/ghcjs-test-wrapper.sh` for JS,
  `toolchain/wasm-test-wrapper.sh` (post-links the JSFFI glue, then `wasm-test-runner.mjs`) for wasm.
- `toolchain/browser-test-env.sh` gives each run its own Node Web Storage file and sets
  `globalThis.window`, so storage tests exercise the real browser FFI on JS/wasm and the in-memory
  store on native.
- `wasm-test-runner.mjs` must `process.exit` as soon as `wasi.start` returns: a JS→Haskell callback
  can leave `rts_schedulerLoop` queued on `setImmediate`, which crashes with "RTS is not initialised"
  if it runs after the RTS has shut down. `toolchain/test-wasm-runner.sh` checks exit codes survive.
  A command's `main` cannot wait for a promise (it throws `WouldBlockException`), so a Node suite
  that awaits one (`jsbits-promise-test`) is a reactor exporting `main` as the async export
  `hs_test`, which the runner calls, awaits, and exits 0 or 1 after.
- A browser test suite (built with `hspec-halogen`, e.g. material's) is a wasm *reactor* exporting
  `hs_start`, whose `main` is `runBrowserTests spec`. `wasm-test-wrapper.sh` recognises the export
  and hands the binary to the native `hspec-halogen` executable (`hspec-halogen test`, built by
  `toolchain/build-hspec-halogen.sh`), which loads it into headless Chromium with the package's
  `test/web/` assets (and whatever `test/web/bundle.sh` builds), relays its output, and performs
  its `click`/`typeText`/`press` through Playwright as trusted input. Its JavaScript lives in
  `hspec-halogen/js/` and is compiled into the executable. So `npm run test-wasm` needs the host
  GHC and Chromium. The suite's cabal `interactive` flag drops the reactor options, and
  `toolchain/dev-test.sh` runs it in browser GHCi under ghciwatch (wasm only), in a window
  `hspec-halogen open` opened, rerunning `:main` on every save.
  On the JS backend the same suites run in Chromium too: `ghcjs-test-wrapper.sh` recognises a
  program that uses hspec-halogen's page functions and hands it to `hspec-halogen test`, which
  bundles `<suite>.jsexe/all.js` with esbuild and loads it; its FFI is
  `Test.Hspec.Halogen.Internal.Page` (bindings returning a `Promise` for the async calls). Both
  browser suites therefore also test core's JS-backend DOM code (`core/jsbits/monad_dom.js`).
  Natively `hspec-halogen` builds (its page functions do nothing, see
  `Test.Hspec.Halogen.Internal.Page`) so suites type-check for HLS; there `runBrowserTests`
  only reports a skip.
- `core/jsbits/polyfills.js`, one of core's `js-sources`, defines the `h$` functions other
  libraries import that GHC's JS runtime lacks (from `unix` and `splitmix`, which hspec pulls in),
  so every JS program linking core has them, test suites included. `POLYFILLS.md` lists each one
  with where its real fix belongs upstream; add a polyfill there, not in a package's own jsbits.
- Two wasm facts the harness depends on: an async (`safe`) JSFFI import returns a *thunk*, and the
  calling thread only waits for the promise when it is forced. `Halogen.JSBits.Promise.await` is
  where a binding's wait is forced (its own are the only `safe` bindings), so the harness's page
  actions return a `Promise` and await it, and the test does not run on while the page still acts.
  And a JS→Haskell callback is not run on the spot: the glue schedules the RTS scheduler loop as a
  task (`scheduler.postTask` in Chromium), which is why every harness action ends with `settle`.
- `toolchain/test-learn.mjs` tests the deployed artefact rather than a component: it serves the built
  `dist-newstyle/wasm/public/learn`, the rootfs in its `rootfs/`, under `HALOGEN_BASE_PATH` as Pages
  serves it, and checks every lesson's prerendered page, the hydration, the compiler loading in the
  page, every lesson's `Start.hs` and `Solution.hs` compiling there, back and forward, the Cache API
  and a narrow screen (`LEARN_ROOTFS_DIR` serves another rootfs in place of the tree's).
  `toolchain/test-playground.mjs` drives `learn/`'s page on the rootfs, with a program for every
  layer. Both run in CI and `test-learn` gates the Pages deploy; the workflows install Chromium with
  `npx playwright install --with-deps --only-shell chromium`, and the pages need the network
  only for PixiJS from jsDelivr and the Material Icons font: the tutorial bundles its
  browser_wasi_shim, and the compiler's is `learn/vendor/`'s, shipped in the rootfs.

## Architecture

### The component monad is the DOM monad

A component evaluates in the monad the DOM is spoken in. Each backend is a newtype over `IO`:
`BrowserDOM` (instances in `Halogen.VDom.DOM.Monad.JSFFI`, one module for both browser backends),
`MemDOM` (in-memory tree, instance in `.Native`, native only — this is what native tests render into),
and `PixiDOM` in `pixi`. `Halogen.VDom.DOM.Monad` picks the backend with CPP **and** the core `.cabal`
file picks the matching `exposed-modules` per `arch(...)` — keep the two in step.

The class hierarchy (`Halogen.VDom.DOM.Monad.Class`) is split by capability: `MonadDOM` (mutable tree
+ listeners; all the reconciler needs), `MonadAttributes` (HTML attributes/properties), and
`MonadBrowserDOM` (document splicing, window globals, storage). Methods have lifted defaults, so
`ReaderT`/`IdentityT` instances are small; `StateT` is deliberately unsupported because
`mkEventListener` has to *run* the transformer inside a DOM callback. IndexedDB is a class of its own,
`MonadIndexedDB` in `Web.Storage.IndexedDB` (asynchronous, bytes), with its `BrowserDOM` and
`MemDOM` instances in that module rather than in the backend modules: it needs no DOM, so neither
instance is an orphan, and both exist on every architecture (natively `BrowserDOM`'s reports
`Unavailable`).

### Flow of a component

`HalogenM` (`Halogen.Query.HalogenM`) is a free monad over `HalogenF` (State, Subscribe, Fork, Kill,
Unlift, ChildQuery, Raise, …). `Halogen.IO.Driver.Eval.evalM` interprets it against a
`DriverState` IORef: a state change triggers a render (done before the program goes on); forks are
tracked in a map so `kill`/`join` and finalization can find them. Rendering produces `VDom`,
reconciled by the machine-based `Halogen.VDom.DOM.buildVDom` through whatever `MonadDOM` instance is
in play.

A mounted tree runs on one loop (`Halogen.IO.Driver.Runtime`), as purescript-halogen runs on Aff:
its state, children, renders and lifecycle are touched only there, one piece of work at a time, by
whichever thread brings it work while it is idle. A component's program is a fiber: its
synchronous parts run on the loop, and it waits only in `lift`/`liftIO`, which run on a worker
thread while the rest of the tree goes on (as `liftAff`). What must happen at once — `preventDefault`,
focusing, reading the DOM as it is, a synchronous API such as `localStorage`, a scene handed to a
canvas in order — goes through `H.liftEffect` (hooks: `Hooks.liftEffect`), which must not wait. An
event such an effect dispatches is handled before the dispatch returns, nested in it
(`enterCallback`), as the browser does, whether an element's handler or a subscription to a DOM
listener (`HQE.eventListener`) hears it; that holds only on the thread of a callback the page called
(`Halogen.JSBits.inSyncCallback`), so a worker's or a timer's notification is queued. "At once" means at once in the action, which is inside the
browser's dispatch of the event when the loop was free as the event arrived. On wasm it is also
when the loop was held by a thread that is runnable but not running: the glue's scheduler hands
the page back after about 15 ms with threads still runnable, and a callback the page makes
returns with the threads it woke still runnable, so a long render can be left unfinished between
two tasks with an event dispatched in between. The listener then yields to the loop's
runner, inside the dispatch, until the work it queued has run (`letRunnerFinish` in the runtime;
the runner's `threadStatus` is `ThreadRunning`). The page waits for that work inside the dispatch,
and work that mostly computes takes a few times as long to finish this way, since a yield hands
over only a short slice. Otherwise the action is queued and the browser has acted on the event by
the time it runs: when the loop's runner is the one that dispatched the event outside a
synchronous effect (a render that focuses an element; its status is then `BlockedOnForeignCall`,
under the listener on the JavaScript stack), and on the JavaScript backend, where a callback that yields is carried on
asynchronously, so a listener there never waits. A handler nests in a synchronous effect under
way (`withinEffect`, an effect that clicks an element) only when the effect's thread is that
blocked runner: on wasm an effect can
also be preempted between tasks, and a handler arriving then waits for it like for any other work.
An effect that counts on the dispatch still being under way (preventing the default, reading a
form's value before the browser changes it, focusing in response to a click) is then too late. For
the two that matter most there is a declaration on
the property instead: a handler that must prevent the default or stop propagation whatever the
loop is doing says so with `HE.preventingDefault` or `HE.stoppingPropagation`, and the listener
does it before handing the action over (`Halogen.VDom.DOM.Prop`); `Halogen.Router`'s links do. A component counts as initialized once its `Initialize` has run its
synchronous part (reached its first wait, or ended), after its children's have: then the outputs
kept for it are delivered and its parent's `Initialize` starts. Waiting in `Initialize` waits only
for that component; its ancestors, the program whose `modify` rendered it and `runUI`/`hydrateUI`
go on (purescript-halogen waits for initializers to end; this deliberately does not). Only
`Halogen.Static`, through `runUIInitialized` (in the unexposed `Halogen.IO.Driver.Mount`), waits for every `Initialize` to end. A cancelled
fiber runs nothing more, not even the rest of
the synchronous work that cancelled it; its worker, and the fibers of its parallel branches, go with
it. A component that goes away cancels its fibers; a render pass that fails takes back the children
it made, and the VDom backend rebuilds a machine whose patch failed. The loop and a fiber's worker
change hands masked. `core/test/Test/DriverContract.hs` and `core/test/Test/Runtime.hs` state the
contract as tests.

What a tree does is reported to `Halogen.Trace` (in `Halogen.IO.Driver.Mount` and `.Eval`):
components made and gone, renders with their time, actions, state changes, queries, outputs,
subscriptions and forks, each tagged with the component's id, its parent's and its label (its state
type's name from the `Typeable` that `mkComponent` asks for, or `H.named`'s). The driver asks
`listening` before it builds an event, and keeps a registry of live components (for `restoreState`
and `rootNode`) only for those made while someone listens, so a program with no listener pays one
read of a reference per hook point. `Component`'s constructor carries a `ComponentInfo` (the label
and the state's `TypeRep`, `Nothing` for a state that cannot be `Typeable`, such as one holding an
`m` action, as `Portal`, `Canvas`, material's `List`/`Tabs`, custom-element's wrapper and hooks
have). `hspec-halogen` prints a test's last 50 events when it fails; `devtools` shows them live.

The canvas layer reuses this: a scene (`Halogen.Canvas.Elements`/`Properties`, in core) is an
ordinary `VDom` whose nodes, in `pixi`, are Pixi display objects — so HTML and canvas share one
reconciler. Keyed groups (`keyedGroup`, `withKeys`) preserve identity across reorders.

### FFI per backend

JS-backend imports link ordinary JavaScript from each package's `jsbits/` through
`js-sources`; WASM embeds those same files at compile time. Declare the imports
once with `Halogen.JSBits.browserJS`, without CPP: one binding list for every
backend (`wasmJS` on WASM, imports by name on JS, `inert` stubs natively). Types
that differ between the backends are `Halogen.JSBits`' own (`JSVal`, `JSText`,
`Callback`, with `toJSText`, `mkCallback` and the rest), so the only CPP for FFI
is in `jsbits/src/Halogen/JSBits/Value.hs`; a wrapper that must do something else
natively asks `inBrowser`. A result type needs an `Inert` instance (derive it for
a newtype over `Foreign`). Keep browser logic in jsbits and only representation
conversions, callback adapters and scheduling primitives in backend-specific imports. The WASM project
enables shared dependencies because its Template Haskell interpreter loads dynamic
objects. Sources must ship in `extra-source-files`; `addDependentFile` tracks edits.
Material's shared JS is already bundled with its external npm dependencies.
On wasm, `foreign import javascript "wrapper"` callbacks run synchronously until
they block; on JS, `syncCallback1 ContinueAsync` gives the same behaviour.
A binding that starts asynchronous work returns a `Promise` (`Halogen.JSBits.Promise`)
from an `Unsafe` import, and the program `await`s it, which gives a rejection back as a value,
instead of declaring a `Safe` import and forcing its result with `evaluate`.

### The tutorial compiles in the browser

`examples/learn` is an ordinary example (prerendered per route, hydrated), whose lessons are data:
`lessons/NN-slug/` (`lesson.md` with a `key: value` header, `Start.hs`, `Solution.hs`, an optional
`Main.hs`), listed in `lessons/index.txt` and embedded by Template Haskell (`Learn.Lessons`; the text is
a Markdown subset read at compile time, `Learn.Markdown`, which `examples/learn/test` tests natively,
with `Learn.Diagnostics`). Its compiler is `learn/`'s playground: the
page loads the rootfs from `rootfs/` under its base (its `bundle.sh` copies `npm run build-rootfs`'s
output there), keeps the archives in the Cache API under the manifest's `version`, and starts
`libplayground.so` with GHC's `dyld.mjs` (`examples/learn/js/playground.js`). A compile writes the
reader's module to `/tmp/Lesson.hs` from JavaScript and hands the wrapper to the playground's `Run` as
`Main` with `-i/tmp`, so `learn/Playground.hs` is unchanged; the wrapper mounts `Lesson.component` into
`#learn-result`, which the app renders once per lesson (keyed) and never patches inside, and the page
ends that tree (a compile of `main = endPreviousRun`) before taking the element away. The app and the
compiled program are two runtimes on one page. GHC's stderr is parsed into diagnostics
(`Learn.Diagnostics`) for the list under the CodeMirror editor and its lint markers. Lesson 13's own
`Main.hs` runs the lesson's `spec` instead of mounting (`hspec-halogen`'s `runBrowserTestsIn`, the
rootfs' `hspec` layer): with no runner in the page, the harness performs its actions with synthetic
events (`hspec-halogen/jsbits/page.js`'s in-page mode, which `hspec-halogen-test` also runs its
examples through), and hspec's report is the program's stdout.

The deploy is that tree and nothing else: `build-deploy.yml` builds the rootfs (`build-learn-rootfs.sh`,
restored instead from a cache keyed by its `--key`, a hash of the plan's units in the closure of the
packages it packs, our sources, the compiler and the script's inputs), runs `build-wasm-all.sh` with
`HALOGEN_BASE_PATH` from `configure-pages` (`/haskell-halogen`), so every route's page and the
rootfs' address are under the project site's path, gates on `npm run test-learn` and uploads
`dist-newstyle/wasm/public/learn`. `build.yml`'s wasm job does the same on every push and pull request (with
the repository's name as the base path), and `npm run test-playground` too. The rootfs has its own
copies of our libraries, built in the same run: rebuild it (`npm run build-rootfs`) locally after
changing one, as `build-wasm-all.sh` uses one already there. `rootfs.json` is described once, in
`learn/README.md`.

### Hooks

A hook program is an indexed monad (`Hooks.do` is `QualifiedDo`) indexed by a type-level list of the
hooks it uses, which enforces the rules of hooks at compile time. `Halogen.Hooks.Internal.Cells` is a
store indexed by that list; `Internal.Eval` builds it on the first render and steps it afterwards, and
interprets `HookM` into `HalogenM`. `StateId scope s` is branded with its component (role-annotated
nominal on `scope`) so handles cannot escape. `settle` owns a render-plus-effects pass via a
`running`/`dirty` pair updated atomically: reentrant requests only mark another pass due, and state
writes inside a `fork` request a pass immediately. `Halogen.Hooks.Extra.Hooks` (debouncer, throttle,
storage, …) is built only from public hooks.

## Conventions

- `core` uses `NoImplicitPrelude` with its own `HPrelude` (Protolude adapted to UnliftIO); `hooks` uses
  Protolude. Large `default-extensions` lists live in each `.cabal` file (`GHC2021`,
  `OverloadedRecordDot`, `NoFieldSelectors`, `DuplicateRecordFields`, `StrictData`, …).
- Formatting is fourmolu (`fourmolu.yaml`: 2-space, leading commas/arrows).
- **Never commit code with warnings.** Every package has `-Wall -Wextra` in its `.cabal` file;
  `cabal.project` (imported by `cabal-wasm.project` and `cabal-ghcjs.project`) adds `-Werror` for the
  repository's own packages. Keep `-Werror` out of the `.cabal` files: it would ship to Hackage and
  break users' builds when a newer GHC or dependency adds a warning. Before committing, build
  everything you touched for all three backends (`npm run build-native`, `npm run build-js`, and for
  wasm `npm run test-wasm`, plus `npm run build-wasm-all` if an example changed), because CPP gives
  each backend different code and a warning in one doesn't show in the others. Fix a warning rather
  than silencing it; if a `-Wno-…` is ever the right answer, scope it to the module with an
  `OPTIONS_GHC` pragma and say why. GHC doesn't recompile a module when only `-Werror` changes, so after changing warning flags delete
  `<builddir>/build/*/*/<package>-*` to see every warning. `dev-test.sh` loads suites with `-Wwarn`
  so a half-finished edit still reruns the tests; that doesn't make those warnings acceptable to
  commit.
- Each package has its own `CHANGELOG.md` with an `Unreleased` section written as prose for users
  upgrading; releases are tagged per package (`core-v0.10.0`, `material-v0.2.0`).
- Commit subjects are short imperative sentences in plain English ("Make a prop that goes away
  actually go away"), sometimes prefixed with the package (`core: …`, `hooks: …`); bodies explain why.
- Haddocks and comments explain reasoning in full sentences rather than restating the code.
- A component is `H.component initialState render [H.onAction handleAction, …]`, with
  `H.onQuery`, `H.onInitialize`, `H.onFinalize` and `H.onReceive` as needed; the lifecycle
  handlers take a `HalogenM` program, so an `Initialize` action exists only where the component
  wants one. `H.mkComponent H.ComponentSpec {…}` with an `eval` of its own (written out, or
  `H.mkEval` of an `EvalSpec`) is for a component that answers its messages with one function,
  such as one that wraps another's `eval`.
- Don't hardcode a monad: components, specs and helpers are polymorphic in `m` with class
  constraints (`MonadMaterial m`, `MonadUUID m`, `MonadBrowserTest m`, …), and only an entry point
  picks one: `Main.hs`, or a test suite's `main`. A spec takes the monad as a required type argument
  (`spec :: forall m -> (MonadBrowserTest m) => Spec`, called as `spec BrowserDOM`), not through
  `AllowAmbiguousTypes`, a `Proxy` or a type application.
- Never hardcode a `RefLabel`: generate it with `H.newRefLabel "<name>"` in `initialState` and keep
  it in the state. Refs are looked up in the component that rendered them, which includes HTML a
  component renders for its parent (Tabs, List), so a fixed label can collide with a parent's.
- A new example needs only `examples/<name>/` with `halogen-example-<name>.cabal`, `Main.hs` and
  `web/` (`index.html` with a `<div id="app"></div>` that `Main.hs` mounts into, + `index.js`
  fetching `./app.wasm`); the `examples/*` glob and `build-wasm-all.sh` pick it up. An optional
  `bundle.sh` beside it is run with the output directory as its argument (the material example uses
  one to bundle its JS and CSS with esbuild and sass). An optional native executable
  `halogen-example-<name>-prerender` (`buildable: False` on the browser backends) is built with the
  host GHC and run by `build-wasm.sh` as `<exe> examples/<name>/web/index.html <output dir>`, to write
  the page with the component already rendered into `#app` by `Halogen.Static` (see `vanilla`, and
  `learn`, which writes a page per route under `HALOGEN_BASE_PATH`); `Main.hs` then mounts with
  `hydrateUI`, which adopts that HTML.
  For `npm run dev` (`toolchain/dev.sh`, browser GHCi), the executable needs the `interactive`
  flag every example has, which compiles the example's own modules into it and drops the reactor
  options, and its `main` starts with `HA.endPreviousRun` and passes `socket.dispose` (the
  tutorial also `Router.stop router`) to `HA.keepForNextRun` (`Halogen.IO.Util`), which end the
  last `:main`'s tree; the
  dev page is derived from `web/index.html` by replacing its `index.js` module script with GHCi's
  `../main.js`, so keep that script tag.
- A tutorial lesson is a directory of `examples/learn/lessons/` and a line in its `index.txt`;
  `examples/learn/README.md` says what goes in it (the header, the Markdown subset, the `Lesson`
  module's contract and the default wrapper, the packages a lesson can use), and
  `npm run test-learn` compiles every lesson's `Start.hs` and `Solution.hs` in the browser.
- Each example's component lives in a library module (`src/Example/<Name>.hs`); `Main.hs` only starts
  it. Pages deploys only the tutorial, so an example reaches readers through a lesson in
  `examples/learn`; see its README. On its own an example is served with `npm run serve-wasm`.
