# haskell-halogen

[![CI Build](https://github.com/Swordlash/haskell-halogen/actions/workflows/build.yml/badge.svg)](https://github.com/Swordlash/haskell-halogen/actions/workflows/build.yml)

A port of [purescript-halogen](https://github.com/purescript-halogen/purescript-halogen/) to GHC
Haskell, plus the component and rendering libraries built on top of it.

[The tutorial](https://swordlash.github.io/haskell-halogen/) teaches it in the browser: each lesson
is a page with its text, an editor and a Compile button, and GHC itself, compiled to WebAssembly,
compiles your code in the page and mounts the component beside the text, with no server behind it.
The tutorial is a Halogen app too ([examples/learn](examples/learn/README.md)).

AI usage disclaimer: All code until tag `0.9.0` was hand-written. Any later commits might have used Codex or Claude.

![image](screenshot.png)

## Packages

| Directory | Package | What it is |
| --- | --- | --- |
| [core/](core/) | `haskell-halogen-core` | The Halogen port itself: components, VDom, events, SVG, layouts. |
| [hooks/](hooks/) | `haskell-halogen-hooks` | A port of `purescript-halogen-hooks`: a component as one function. |
| [store/](store/) | `haskell-halogen-store` | Global state: a port of `purescript-halogen-store`, one store changed by a pure reducer and components connected to the part they need. |
| [router/](router/) | `haskell-halogen-router` | Routing by the page's address: a port of `purescript-routing-duplex`, joined to the browser's history. |
| [net/](net/) | `haskell-halogen-net` | The browser's `fetch`: a request as a value, its response and body as values, aborted when the component waiting for it goes away. |
| [custom-element/](custom-element/) | `haskell-halogen-custom-element` | A component as a custom element (a Web Component) for any page, React's, Vue's or plain HTML: attributes and properties are its input, outputs are `CustomEvent`s, queries are methods. |
| [tauri/](tauri/) | `haskell-halogen-tauri` | Tauri v2's IPC for an app in a Tauri webview: Rust commands with aeson JSON, events as emitters, the window's own events; inert in a plain browser. |
| [material/](material/) | `haskell-halogen-material` | Google Material Components bindings. |
| [pixi/](pixi/) | `haskell-halogen-pixi` | A PixiJS v8 canvas rendering backend. |
| [jsbits/](jsbits/) | `haskell-halogen-jsbits` | Compile-time embedding of shared browser JavaScript into WASM FFI. |
| [sound/](sound/) | `haskell-halogen-sound` | Music and sound effects for a page: a theme to loop, a shuffled album fetched ahead, effects on top. |
| [hspec-halogen/](hspec-halogen/) | `hspec-halogen` | Test components in a real browser with hspec: mount, click, type, check. |
| [th/](th/) | `haskell-halogen-th` | HTML and CSS as written, with Haskell inside: the `hh` and `clay` quasi-quoters. |
| [th-plugin/](th-plugin/) | `haskell-halogen-th-plugin` | A GHC plugin that expands the quotes as a module is parsed, so every error, type errors included, is at its place in the file. |
| [examples/](examples/) | `halogen-example-*` | One runnable browser app per library, and `learn`, the tutorial, whose code compiles in the page. |

`hooks`, `store`, `router`, `material`, `pixi`, `hspec-halogen`, `th` and `custom-element` depend on
`core`, and `tauri` on `core` for its emitters; `net` and `sound` are independent of those runtime libraries. WASM backends also use the `jsbits`
compile-time helper. `th` builds natively and for WebAssembly, not with the JavaScript
backend, which cannot build `ghc-lib-parser`. Every package builds from the one `cabal.project` at the
repository root, so a change to
`core` is type-checked against every dependent and every example in the same build.

## The monad a component runs in

A component evaluates in the same monad the DOM is spoken in. Each backend is a newtype over `IO` —
`BrowserDOM`, `MemDOM`, and `PixiDOM` in `haskell-halogen-pixi` — so more than one can exist in a
single build and each can say, through associated type families, what its tree is made of.

An application with effects of its own stacks them on a backend and derives the classes through:

```haskell
newtype AppM a = AppM (ReaderT Config BrowserDOM a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, PrimMonad, MonadDOM, MonadAttributes, MonadBrowserDOM)
```

The class methods have lifted defaults, so a transformer instance is only as long as its associated
types plus `mkEventListener`. `ReaderT` and `IdentityT` come with the library; `StateT` and friends
are deliberately absent, because the DOM calls a listener back and `mkEventListener` has to *run*
the transformer rather than lift it — a state update made inside a callback has nowhere to go.

The interface is split by what a backend actually has. `MonadDOM` is the mutable tree and its
listeners, and is all the reconciler uses. `MonadAttributes` adds named attributes and properties,
which only an HTML backend has. `MonadBrowserDOM` adds document splicing and the window globals,
and carries the equalities back to the concrete `Node` and `Element` as superclasses.

## Building

The library itself compiles under any GHC from 9.6 to 9.14; CI builds against 9.14.1, the version
the GitHub runner image ships. The browser targets need a cross-compiler.

```sh
npm install                  # once, for esbuild, sass and Material Components
npm run build-native         # every package, host GHC
npm run test                 # test suites across native, JavaScript and wasm
```

The cross-backend test scripts use Node 24 or later. They give each run a
temporary Web Storage file so storage tests exercise the browser FFI as well
as the native backend, without sharing a store between runs.

### WebAssembly

The default browser and deployment target. It requires the
[ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta) toolchain to be bootstrapped
first — the build scripts source `~/.ghc-wasm/env` and will fail without it:

```sh
cd
git clone https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta.git
cd ghc-wasm-meta 
git checkout 358ea50b8496a69da6ce375c0c58bc049dbcb92d
SKIP_GHC=1 FLAVOUR=9.14 ./setup.sh
source ~/.ghc-wasm/env
ghcup -s "file://$HOME/ghc-wasm-meta/ghcup-wasm-0.0.9.yaml" install ghc "wasm32-wasi-9.14.1.20260731" --set -- $CONFIGURE_ARGS
```

That installs `wasm32-wasi-ghc` and friends under `~/.ghc-wasm`. The exact GHC version this
repository builds against is pinned in `cabal-wasm.project`, and `.github/workflows/build.yml`
pins the ghc-wasm-meta revision CI bootstraps from — keep the two in step when bumping either.

NOTE: use `cabal` version `3.16.1` on this repository.

With that in place, build and serve any example by name:

```sh
npm run serve-wasm -- pixi        # or: vanilla, hooks, material, th
npm run build-rootfs              # the tutorial's compiler: GHC and our libraries for the page
npm run build-wasm-all            # every example, size-optimised with wasm-opt, the tutorial with its compiler
npm run test-learn                # drive the built tutorial in headless Chromium, every lesson compiled
```

The tutorial, `examples/learn`, is what GitHub Pages serves: a page per lesson, switched by the
URL's path (`/02-counter/`) through `haskell-halogen-router`, with GHC in `rootfs/` beside it. The
build writes an `index.html` for every route, each with its lesson already rendered into it, so the
back button, reloads and deep links work on a static host with no 404 fallback.
[examples/learn/README.md](examples/learn/README.md) says how it is built, tested and deployed, and
how to write a lesson; [learn/README.md](learn/README.md) how GHC runs in the page.
`test-learn` needs a Chromium that Playwright can launch (`npx playwright install chromium`).

`serve-wasm` opens <http://127.0.0.1:8080> automatically. Set `PORT` to choose another port, or
`NO_OPEN=1` to suppress opening the browser (for example in CI).

To work on an example with GHCi rather than a build per change, install
[ghciwatch](https://mercurytechnologies.github.io/ghciwatch/) and run `npm run dev -- <example>`
(`vanilla`, `hooks`, `pixi`, `material`, `th` or `learn`). It loads the example into wasm browser
GHCi, opens its page at <http://127.0.0.1:8080/assets/index.html> (`PORT` changes the port), and
reloads the example and reruns `main` whenever you save one of its modules; a change to a library,
a cabal file or the example's `web/` restarts GHCi, which opens a new page. The page is the
example's own `web/index.html`, loading GHCi's script in place of `index.js`. GHCi's interpreter
runs in it, so reloading the page ends the session until GHCi is restarted. `npm run dev -- --repl
<example>` starts plain GHCi instead, for running `:main` yourself, and works without ghciwatch.

### JavaScript backend

Needs a `javascript-unknown-ghcjs-ghc` cross-compiler; the easiest way to get one is the `ghcup`
precompiled binaries described [here](https://www.haskell.org/ghcup/guide/#cross-support).

```sh
npm run serve-ghcjs -- vanilla    # cabal build + http-server
npm run build-js                  # material example, bundled into dist/ with esbuild
npm run build-js-dev              # the same, without minification or brotli
```

Build artifacts are kept in `dist-newstyle/native`, `dist-newstyle/javascript`,
`dist-newstyle/wasm` and `dist-newstyle/wasm-dev` respectively.

## Adding an example

Create `examples/<name>/` with a `halogen-example-<name>.cabal` (executable named
`halogen-example-<name>`), a `Main.hs`, and a `web/` directory holding `index.html` and an
`index.js` that fetches `./app.wasm`. `index.html` has an empty `<div id="app"></div>`, which
`Main.hs` finds with `selectElement` and mounts the component into. The `examples/*` glob in
`cabal.project` picks the package up, and `toolchain/build-wasm-all.sh` picks up the directory. If
the example needs bundling, add a `bundle.sh` beside it and the build script will run it, passing
the output directory as its argument.

For `npm run dev`, give the executable the `interactive` flag the other examples have, which
compiles the example's modules into it (GHCi reloads only those of the target it loaded) and drops
the wasm reactor options and the `hs_start` export, and have `main` start with
`Halogen.IO.Util.endPreviousRun` and pass how to end what it starts (its socket's `dispose`, and
`Halogen.Router.stop` for a router) to `keepForNextRun`: GHCi keeps the page between runs of
`:main`, and these end the last run's component before the next one mounts. `examples/vanilla` has
both, and `examples/learn` a router as well.

The page can arrive with the component already in it, before `app.wasm` has loaded. Add a native
executable `halogen-example-<name>-prerender` (not buildable on the browser backends) that renders
the component with `Halogen.Static.renderToHtml` and writes `index.html` with the HTML in `#app`:
`build-wasm.sh` builds it with the host GHC and runs it as `<exe> examples/<name>/web/index.html
<output dir>`. `examples/vanilla/Prerender.hs` is the smallest one; `examples/learn/Prerender.hs`
writes one page per route, under the base path in `HALOGEN_BASE_PATH` (`/` by default; the Pages
deploy sets it to the site's). Its `Main.hs` then mounts the component with `hydrateUI` rather than
`runUI`: the component adopts the prerendered HTML, attaching its listeners, instead of rendering
over it.

Put its component in a library module (`src/Example/<Name>.hs`, as the existing examples do, with
`Main.hs` only starting it). GitHub Pages serves only the tutorial, so an example reaches readers
through a lesson in `examples/learn`; [its README](examples/learn/README.md) says how to write one.

## Routing

`haskell-halogen-router` is a port of `purescript-routing-duplex`: a route codec both parses an
address and prints it, and the codec of a sum type is derived from one codec per constructor:

```haskell
data Route = Home | Post Int deriving stock (Generic)

route :: RouteDuplex' Route
route = root $ sum $ #Home .== noArgs .+ #Post .== path "post" (int segment)
```

`Halogen.Router` joins it to the browser's history: `newRouter base fallback route` (a base path
such as `/project/`, which `pageBase` reads from the page's `<base href>`), `current`, `navigate`,
`redirect`, a `changes` emitter fed by those and by `popstate`, and `link`/`follow` for links a
component follows without loading a page. `newHashRouter` routes by fragment instead. Natively the
address is kept in memory, so `Halogen.Static` can render a page for each route, as the tutorial's
prerender does.

## Global state

`haskell-halogen-store` is a port of `purescript-halogen-store`: one store for the application,
changed only by actions through a pure reducer, and components that follow the part of it they
need. A component asks for the store with a constraint, changes it with `updateStore`, and is
connected to a part of it by a selector, which renders it again only when that part changes:

```haskell
data Store = Store {count :: Int, name :: Text}
data Action = Increment | Rename Text

countView :: forall q o m. (Store.MonadStore Action Store m) => H.Component q () o m
countView = Store.connect (Store.selectEq (.count)) display   -- display gets a Store.Connected Int ()

main = runBrowserDOM $ do
  root <- Store.runStoreT (Store 0 "Ada") reduce App.component
  ...
```

The tree runs in `StoreT`, which `runStoreT` hoists away before `runUI`; `Store.subscribe` turns a
part's changes into a component's actions. `HalogenM` reaches the store through `H.liftEffect`, so
an update is made, and heard by the tree, inside the action that made it. See
[store/README.md](store/README.md), and lesson 15 of the tutorial.

## Fetching

`haskell-halogen-net`'s `Halogen.Net.Fetch` is the browser's `fetch`. A component waits for a
response in `lift`, and the request is aborted if the component goes away first:

```haskell
Load -> do
  outcome <- fetch (request GET "/api/notes") >>= either (pure . Left) text
  modify $ \st -> st {notes = outcome}
```

`start` begins a request inside an event's dispatch (`H.liftEffect`) and gives a handle to wait
for or abort later. A rejection (no response, or an abort) is a `Left JSError`; an HTTP error
status is a response whose `ok` is `False`.

## Custom elements

`haskell-halogen-custom-element` makes a component a custom element, which a page that is not a
Halogen app uses as it uses any element: plain HTML, React, Vue, Svelte, Astro, HTMX.

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

Every `<x-counter>` then mounts the component in its shadow root. A change to an observed
attribute or property reaches the component as `Receive`, as from a Halogen parent; what it
raises is dispatched as a `CustomEvent`; `counter.reset()` sends it a query. An element disposes
of its component when it is removed, not when it is moved, and defining the name again swaps the
component of every element of it. `CE.onEvent` reads the `detail` of another library's element's
events. `examples/custom-element` is a plain page driving two such elements from JavaScript.

## Tauri

`haskell-halogen-tauri` is for an app that runs in a [Tauri](https://tauri.app) v2 webview (with
`app.withGlobalTauri`): `invoke` calls a Rust command with aeson-encoded arguments and decodes
its result, `listen` is an emitter of an event's payloads for `H.subscribe`, and `emit` sends one.

```haskell
Initialize -> void (H.subscribe (Progress <$> listen "download-progress"))
Save -> do
  outcome <- invoke "save_note" SaveNote {noteId = 7, text = "…"}
  modify $ \st -> st {saved = outcome}
```

In a plain browser `isTauri` is `False`, a command gives `Left NotInTauri` and an emitter never
fires, so one program runs in both.

## Hooks

`haskell-halogen-hooks` writes a component as one function from its input to its HTML, asking for
state, effects, memoised values and a query handler as it goes, instead of spreading them across
`initialState`, `render` and `handleAction`:

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

`Hooks.do` is `QualifiedDo`, because a hook program is an indexed monad indexed by the list of
hooks it uses. That is what enforces the rules of hooks — the same hooks in the same order on
every render, since the interpreter walks a store of cells in step with the program — and it
enforces them at compile time: a `useState` inside an `if` does not type-check. A composite hook
is a parameterised type synonym over the same list:

```haskell
type UseCounter hooks = UseState Int : UseEffect Int : hooks
```

`Halogen.Hooks.Extra.Hooks` ports
[purescript-halogen-hooks-extra](https://github.com/JordanMartinez/purescript-halogen-hooks-extra)
into the same package rather than a second one: `useDebouncer`, `useThrottle`, `useGet`,
`useEvent`, the `useStateFn` family, and `preventDefault` and friends for handlers that have to
stop the browser handling the same event. `usePrevious` and the `useLocalStorage` family come
from that library's own examples. Storage is a `MonadBrowserDOM` operation, so the in-memory
backend has it too and what a page persists can be tested without a browser; a store holds one
prefixed entry per key, with a base64 value, and `Web.Storage.Serialize` says how a value becomes bytes — JSON by
default, for any type with aeson instances. None of them is primitive — each is written with the hooks
above and nothing else, and each is worth reading as an example of a composite hook.
[examples/hooks/](examples/hooks/) is a page that uses every one of them.

The PureScript original has to do several of these things at runtime, and GHC's type system means
this port does not. The hook list is a type-level list rather than a chain of newtypes; the cell
store is indexed by it, so a cell is read back at the type it was written rather than coerced out
of an array; a state handle is branded with the component that owns it, the way `ST` brands an
`STRef`, so it cannot be raised as an output or stashed somewhere that outlives its component;
effect and memo dependencies are an ordinary value compared with `==` (or with a comparison of
your own, through `useTickEffectBy` and `useMemoBy`), so
`Hooks.captures {x, y} Hooks.useTickEffect` becomes `Hooks.useTickEffect (x, y)`; and a
component's query algebra is part of its hook program's type, so there is no `componentWithQuery`
and no tokens to pass around.

## Canvas rendering

`Halogen.Canvas` (in `core`) is a component that owns a canvas DOM node and delegates to a
`Renderer` record, which a backend implements by supplying `mount`, `update` and `destroy`.

A scene is described in `Halogen.Canvas.Elements` and `Halogen.Canvas.Properties` (both in `core`),
which read like `Halogen.HTML.Elements` and `Halogen.HTML.Properties`: `group`, `line`, `rectangle`,
`circle`, `ellipse`, `arc`, the Bézier curves, `path`, `text` and `sprite`, each taking a list of
props, with a `_` variant for the styling-free case. `path` takes the same commands as an `<svg>`
`d` attribute, from `Halogen.Svg.Attributes`, so one drawing serves both. Props carry the transform, the cursor, the hit area and
pointer handlers — `onClick`, `onPointerDown`, `onPointerUp`, `onPointerOver`, `onPointerOut` and
`onPointerMove`. `outline` frames an element with a border the backend measures, which is the only
way to get one that is right: nothing writing a scene can know a label's extent, or a sprite's
before its texture has loaded. It is measured against the same bounds the backend hit-tests, so it
also shows exactly what is clickable. Higher-level drawings such as grids are ordinary Haskell composition rather than
renderer primitives. Handler actions are raised as typed component outputs; camera changes are
reported separately.

The vocabulary the scene is written in (`Halogen.Canvas.Types`) is backend-neutral, and a scene is an
ordinary `VDom`, so it reconciles through `Halogen.VDom.DOM.buildVDom` — the same machinery that
reconciles HTML. `haskell-halogen-pixi` supplies a PixiJS v8 interpretation of it: a `MonadDOM`
instance whose nodes are Pixi display objects, plus a prop applicator that paints them.

Give stable scene items keys with `keyedGroup` or `withKeys`. Subsequent `View` inputs reconcile
those keys, retain their display objects and listeners, and only repaint what changed; removed keys
are destroyed. Unkeyed siblings are matched by position, so explicit keys are only needed when
identity must survive insertion, removal or reordering. Camera transforms operate directly on the
retained scene. Pan changes are reported when the gesture ends, while wheel changes are coalesced
until the wheel burst has been idle for 120 ms.

Each mounted canvas owns its own Pixi `Application`, renderer, stage, event system and GPU canvas
context. Browsers cache evaluation of the dynamically imported Pixi module by URL, so multiple
canvases reuse the same module and Pixi asset cache rather than downloading and evaluating Pixi
repeatedly.

The default renderer loads its pinned PixiJS module from jsDelivr when mounted, so applications need
no Pixi JavaScript shim or global — but the example does need network access when opened. Use
`componentWith (Config { moduleUrl = ... })` to load a self-hosted or bundled PixiJS v8 module
instead.

## Releases

One repository, one tag namespace: releases are tagged with a package prefix, such as `core-v0.10.0`
or `material-v0.2.0`. Each package keeps its own `CHANGELOG.md` and uploads to Hackage separately.

### Shared browser JavaScript

Both browser backends use the same files under each package's `jsbits/`. The
JavaScript backend links them with `js-sources`; WASM embeds them at compile time
with [Halogen.JSBits](jsbits/README.md), leaving only callback and representation
adapters specific to the backend. Material keeps its existing bundled JS module.

WASM consumers must set `shared: True` in their Cabal project so Template Haskell
can load its dependencies. When using a Git source dependency, include `jsbits`
alongside the requested packages in `subdir`. No runtime asset loading is added.
