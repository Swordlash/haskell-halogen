# haskell-halogen-lazy

Load a [haskell-halogen](https://github.com/Swordlash/haskell-halogen) component on demand. The wrapper shows a placeholder while the loader runs, then mounts the component in its place. The loaded component behaves like any other child, with typed input, outputs and queries. This is similar to React's `lazy` and `Suspense`.

```haskell
import Halogen.Lazy qualified as Lazy

type Slots = ("editor" .== H.Slot Editor.Query Editor.Output ())

render state =
  HH.slot "editor" () (Lazy.lazy Lazy.defaultLoading loadEditor) state.document Edited
```

`loadEditor :: m (H.Component Editor.Query Document Editor.Output m)` can be any action in the component's monad that returns a component. It might fetch required data, load code on demand, or simply return `pure editor`.

## The wrapper

`Lazy.lazy loading loader` is a component of the same type as the one it waits for.

When mounted, the wrapper shows `loading.placeholder` and runs the loader through `H.async` on a separate thread. The rest of the component tree continues normally.

Once loading completes, the wrapper mounts the component with its initial input and delivers any subsequent inputs through `H.onReceive`. It forwards the child's outputs and queries. Queries received before loading finishes, or after a loading failure, return `Nothing`; `H.tell` has no effect. Queries are not queued for later.

If the loader throws, the wrapper shows `loading.failed` with the exception's `displayException` text. It stays in that state until remounted. Asynchronous exceptions, such as cancellation when the wrapper is removed, are not caught.

`Lazy.Loading` contains two `HH.PlainHTML` values without event handlers: the placeholder and the failure view. `Lazy.defaultLoading` uses an empty `<div>` while loading and a `<pre>` for errors.

## Loading once

Each mount runs the loader again. `Lazy.memo` shares one load between them:

```haskell
initialState _ = do
  editor <- Lazy.memo loadEditor
  pure State {editor, ...}

render state = ... HH.slot "editor" () (Lazy.lazy Lazy.defaultLoading state.editor) ...
```

`Lazy.memo` starts loading on its first use. Concurrent callers wait for the same load, and later callers receive the cached component immediately. Failed or cancelled loads are not cached, so a later mount can retry.

## Code loaded on demand: `Halogen.Lazy.Dyld`

On WebAssembly, GHC's dynamic linker can load a component from a shared library when it is first needed. This requires a program started with `dyld.mjs`, which ships with wasm GHC:

```haskell
import Halogen.Lazy.Dyld qualified as Dyld

feature :: Dyld.Library
feature = Dyld.library "lib/libfeature.so" "feature_entry"

loadFeature :: BrowserDOM (H.Component Feature.Query Text Feature.Output BrowserDOM)
loadFeature = liftIO (Dyld.load feature)
```

`Dyld.load` fetches the library and writes it into the program's file system under `/tmp/halogen-lazy/` using its file name. Set `path` to use another location. It then asks dyld to link the library and any missing dependencies, and calls the entry point to obtain a `Dynamic`.

The result is checked against the requested type. A mismatch throws `Dyld.WrongType`. Fetch, link or entry-point failures throw `Dyld.LinkFailed`; environments without dyld throw `Dyld.NotAvailable`. Each path is fetched and linked once. Subsequent loads call its entry point again.

Compile the library with `-shared -dynamic` against the same libraries as the main program. Its dependencies are resolved by file name. Export an entry point that returns the component:

```haskell
module Entry () where

foreign export javascript "feature_entry" entry :: IO (StablePtr Dynamic)

entry :: IO (StablePtr Dynamic)
entry = Dyld.offer (Feature.component :: H.Component Feature.Query Text Feature.Output BrowserDOM)
```

```sh
wasm32-wasi-ghc -shared -dynamic -O2 -this-unit-id my-feature \
  -package-db <the store's DB> -package-db <the project's in-place DB> \
  -package haskell-halogen-core -package haskell-halogen-lazy ... \
  Entry.hs Feature.hs -o libfeature.so
```

The program and shared library must agree on their types, symbol names and runtime setup:

- **Share the interface types.** Put query, input and output types in a library that both the program and the component library depend on. Types defined only inside the dynamically loaded library cannot be named by the program.
- **Use unique names.** dyld has one global export table and identifies libraries by file name. Export names and library file names must be unique. Give the library its own `-this-unit-id` to avoid symbol collisions with the main program.
- **Expose the linker to the page.** Set `globalThis.halogenDyld = { dyld, rootfs, shim }`. `dyld` is the result of `dyld.mjs`'s `main(...)`, `rootfs` is the `browser_wasi_shim` `PreopenDirectory` used by `DyLDBrowserHost`, and `shim` is the module providing `File` and `Directory`. `Dyld.available` checks for this configuration.

### How the library is linked and called

`Dyld.load` uses dyld's API through `lazy/jsbits/lazy.js`. The calling Haskell thread waits for its promise with `Halogen.JSBits.Promise.waitCatch`:

1. Fetch the library and write its bytes at `path` in dyld's file system, using `browser_wasi_shim`'s `File` and `Directory` types.
2. Await `dyld.loadDLLs(path)`. This instantiates the library in the program's memory and table, resolves imports, finds missing NEEDED libraries in the search directories, runs `_initialize` and adds exports to `dyld.exportFuncs`.
3. Call `dyld.exportFuncs[entry]()`. The exported Haskell function runs on the program's RTS and returns a numeric `StablePtr`. `Dyld.load` dereferences and frees it.

No additional runtime registration is needed. dyld started the RTS with `__ghc_wasm_jsffi_init` when loading the main program, and later libraries use that same runtime. Tests cover this through Node's `DyLDHost`, including repeated calls from an awaited program export, and in the browser through `npm run test-playground` and `npm run test-learn`.

A failed link cannot be retried until the page reloads. dyld marks the library name as loaded before linking and provides no way to clear it. Later loads of that path therefore return the same `LinkFailed`. A failed fetch can be retried.

### Where it works, and what it costs

Dynamic loading works only if GHC's dyld started the program. The tutorial uses this setup: GHC runs as a shared library, and lesson 18 loads `demo/feature` as `liblazy-demo.so`. `toolchain/build-learn-rootfs.sh` builds it, with shared interface types from `demo/`'s `haskell-halogen-lazy-demo`.

A standalone `.wasm` binary, a JavaScript-backend program or a native program has no dyld loader, so `Dyld.load` throws `NotAvailable`. `Lazy.lazy` still works with other loaders on all of these targets.

To use dynamic loading, build the application and all its dependencies, including boot libraries, as shared libraries. Start them with `dyld.mjs` from a file system populated by the page. See [`learn/README.md`](../learn/README.md) for the tutorial's setup.

This costs more than a standalone binary, where the linker removes unused code. The tutorial app is 2.5 MB as one `app.wasm`, or 0.8 MB compressed. As shared libraries, `haskell-halogen-core` and its dependencies beyond GHC's libraries occupy 84.5 MB, or 7.2 MB compressed. GHC's `ghc-internal` adds 25.7 MB and the C libraries add 6.8 MB. dyld also takes seconds to link them at startup.

Dynamic loading can make sense for a large application where each visit uses only a small subset of features. For a small application, one linked `.wasm` is smaller and starts faster.

## Backends

`Halogen.Lazy` is pure Halogen and works on every backend. `Halogen.Lazy.Dyld` builds on every backend, through `Halogen.JSBits.browserJS`, and links only on WebAssembly under dyld.
