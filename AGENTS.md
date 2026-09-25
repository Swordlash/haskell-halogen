# AGENTS.md

This file provides guidance to AI coding agents working with code in this repository.

## What this is

A port of purescript-halogen to GHC Haskell, as a multi-package cabal project: `core`
(`haskell-halogen-core`, the port itself), `hooks` (`purescript-halogen-hooks` port), `material`
(Material Components bindings), `pixi` (PixiJS v8 canvas backend), `hspec-halogen`
(an hspec harness that tests components in a real browser), and
`examples/*` (one browser app each, plus `examples/all`, the gallery that mounts the others and is what
Pages deploys). `hooks`, `material`, `pixi` and `hspec-halogen` depend only on `core`. Everything builds from the root
`cabal.project`, so a `core` change is type-checked against every dependent and example.

Every package is compiled for three targets: native GHC, the GHC JavaScript backend, and GHC
WebAssembly (the default browser and deployment target).

## Commands

```sh
npm run build-native            # all packages, host GHC, into dist-newstyle/native
npm run test-native             # native test suites
npm run test-ghcjs              # toolchain/test-ghcjs.sh — JS backend
npm run test-wasm               # toolchain/test-wasm.sh — wasm backend
npm run test                    # toolchain/run-tests.sh — all three in sequence
npm run serve-wasm -- <example> # build one example to wasm and serve it on :8080
npm run build-wasm-all          # every example, wasm-opt'd; Pages deploys only public/all
npm run test-gallery            # the built `all` gallery in headless Chromium (Playwright)
npm run build-js                # JS backend: build all, bundle the material example into dist/
npm run dev-test -- <package>   # a package's browser test suite in browser GHCi, rerun on save
npm run format                  # fourmolu over core, hooks, pixi, hspec-halogen, material/test, examples
```

`core` and `hooks` have test suites that run under Node (`Halogen-core-test`,
`Halogen-hooks-test`), and `material` and `hspec-halogen` have ones that run in a browser
(`Halogen-material-test`, `hspec-halogen-test`; on wasm and JS, see below). `hspec-halogen-test` is also
the harness's examples, and where core's browser behaviour (events, properties the page changes,
keyed moves, refs, forks, subscriptions) is tested for real. All are hspec, `main-is: Test.hs` which aggregates `Test.*` specs. Run one suite or one test with hspec's
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
  `Test.Hspec.Halogen.Internal.JS` (`interruptible` imports for the async calls). Both
  browser suites therefore also test core's JS-backend DOM code (`core/jsbits/monad_dom.js`).
  Natively `hspec-halogen` builds (its page functions panic, see
  `Test.Hspec.Halogen.Internal.Page`) so suites type-check for HLS; there `runBrowserTests`
  only reports a skip.
- `core/jsbits/polyfills.js`, one of core's `js-sources`, defines the `h$` functions other
  libraries import that GHC's JS runtime lacks (from `unix` and `splitmix`, which hspec pulls in),
  so every JS program linking core has them, test suites included. `POLYFILLS.md` lists each one
  with where its real fix belongs upstream; add a polyfill there, not in a package's own jsbits.
- Two wasm facts the harness depends on: an async (`safe`) JSFFI import returns a *thunk*, and the
  calling thread only waits for the promise when it is forced, so an `IO ()` import must be forced
  (`evaluate`) or the test runs on while the page still acts. And a JS→Haskell callback is not run
  on the spot: the glue schedules the RTS scheduler loop as a task (`scheduler.postTask` in
  Chromium), which is why every harness action ends with `settle`.
- `toolchain/test-gallery.mjs` tests the deployed artefact rather than a component: it serves the built
  `dist-newstyle/wasm/public/all` and checks that each route mounts its example and unmounts the
  previous one, through clicks, back, forward and a reload. It runs in CI and gates the Pages deploy;
  both install Chromium with
  `npx playwright install --with-deps --only-shell chromium`; Pixi needs network for its CDN.

## Architecture

### The component monad is the DOM monad

A component evaluates in the monad the DOM is spoken in. Each backend is a newtype over `IO`:
`BrowserDOM` (instances in `Halogen.VDom.DOM.Monad.JS` or `.WASM`, selected by architecture),
`MemDOM` (in-memory tree, instance in `.Native`, native only — this is what native tests render into),
and `PixiDOM` in `pixi`. `Halogen.VDom.DOM.Monad` picks the backend with CPP **and** the core `.cabal`
file picks the matching `exposed-modules` per `arch(...)` — keep the two in step.

The class hierarchy (`Halogen.VDom.DOM.Monad.Class`) is split by capability: `MonadDOM` (mutable tree
+ listeners; all the reconciler needs), `MonadAttributes` (HTML attributes/properties), and
`MonadBrowserDOM` (document splicing, window globals, storage). Methods have lifted defaults, so
`ReaderT`/`IdentityT` instances are small; `StateT` is deliberately unsupported because
`mkEventListener` has to *run* the transformer inside a DOM callback.

### Flow of a component

`HalogenM` (`Halogen.Query.HalogenM`) is a free monad over `HalogenF` (State, Subscribe, Fork, Kill,
Unlift, ChildQuery, Raise, …). `Halogen.IO.Driver.Eval.evalM` interprets it against a
`DriverState` IORef: a state change triggers a render; forks are tracked in a map so `kill`/`join`
and finalization can find them. Rendering produces `VDom`, reconciled by the machine-based
`Halogen.VDom.DOM.buildVDom` through whatever `MonadDOM` instance is in play.

The canvas layer reuses this: a scene (`Halogen.Canvas.Elements`/`Properties`, in core) is an
ordinary `VDom` whose nodes, in `pixi`, are Pixi display objects — so HTML and canvas share one
reconciler. Keyed groups (`keyedGroup`, `withKeys`) preserve identity across reorders.

### FFI per backend

JS-backend imports use arrow-function strings (`"((x) => x.child)"`) plus `js-sources` under each
package's `jsbits/`; wasm imports use `$1` body syntax (`"$1.child"`) and `ghc-experimental`.
Modules that need both switch on `javascript_HOST_ARCH` / `wasm32_HOST_ARCH` (see
`core/test/Test/GHCJS.hs`). On wasm, `foreign import javascript "wrapper"` callbacks run
synchronously until they block; on JS, `syncCallback1 ContinueAsync` gives the same behaviour.

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
- Warnings are errors: every package has `-Wall -Wextra` in its `.cabal` file, and `cabal.project`
  adds `-Werror` for the repository's own packages (not in the `.cabal` files, so a released package
  still builds under a GHC with new warnings). CPP gives each backend different code, so a change is
  warning-free only once native, JS and wasm all build. GHC doesn't recompile a module when only
  `-Werror` changes, so to see every warning, delete `<builddir>/build/*/*/<package>-*` first.
  `dev-test.sh` loads suites with `-Wwarn`.
- Each package has its own `CHANGELOG.md` with an `Unreleased` section written as prose for users
  upgrading; releases are tagged per package (`core-v0.10.0`, `material-v0.2.0`).
- Commit subjects are short imperative sentences in plain English ("Make a prop that goes away
  actually go away"), sometimes prefixed with the package (`core: …`, `hooks: …`); bodies explain why.
- Haddocks and comments explain reasoning in full sentences rather than restating the code.
- Don't hardcode a monad: components, specs and helpers are polymorphic in `m` with class
  constraints (`MonadMaterial m`, `MonadUUID m`, `MonadBrowserTest m`, …), and only an entry point
  picks one: `Main.hs`, or a test suite's `main`. A spec takes the monad as a required type argument
  (`spec :: forall m -> (MonadBrowserTest m) => Spec`, called as `spec BrowserDOM`), not through
  `AllowAmbiguousTypes`, a `Proxy` or a type application.
- Never hardcode a `RefLabel`: generate it with `H.newRefLabel "<name>"` in `initialState` and keep
  it in the state. Refs are looked up in the component that rendered them, which includes HTML a
  component renders for its parent (Tabs, List), so a fixed label can collide with a parent's.
- A new example needs only `examples/<name>/` with `halogen-example-<name>.cabal`, `Main.hs` and
  `web/` (`index.html` + `index.js` fetching `./app.wasm`); the `examples/*` glob and
  `build-wasm-all.sh` pick it up. An optional `bundle.sh` beside it is run with the output directory
  as its argument (the material example uses one to bundle its JS and CSS with esbuild and sass).
- Each example's component lives in a library module (`src/Example/<Name>.hs`); `Main.hs` only starts
  it. `examples/all/Gallery.hs` mounts those components by route, so a new example appears on the
  deployed site only once it is added there. `Gallery` is compiled for JS and wasm only, because
  `BrowserDOM` has no DOM instances on native.
