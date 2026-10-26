# haskell-halogen-lazy

A [haskell-halogen](https://github.com/Swordlash/haskell-halogen) component the page has not got
yet. The tree shows a placeholder where the component goes, a loader brings the component, and
the component mounts in its place and is from then on an ordinary child: typed queries, outputs
and input, as if it had been there all along. It is what React's `lazy` and `Suspense` do.

```haskell
import Halogen.Lazy qualified as Lazy

type Slots = ("editor" .== H.Slot Editor.Query Editor.Output ())

render state =
  HH.slot "editor" () (Lazy.lazy Lazy.defaultLoading loadEditor) state.document Edited
```

`loadEditor :: m (H.Component Editor.Query Document Editor.Output m)` is anything of the
component's monad that ends with the component: a request for what the component needs before it
can show anything, code the page loads on demand (below), or `pure editor`.

## The wrapper

`Lazy.lazy loading loader` is a component of the same type as the one it waits for.

- When it is mounted it shows `loading.placeholder` and runs the loader, through `lift`: on a
  thread of its own, while the rest of the tree goes on.
- When the loader is done it shows the component in its place, with the input the wrapper was
  given: the first one and every one it has received since, which the component hears through
  its own `H.onReceive`.
- What the component raises, the wrapper raises; the queries sent to the wrapper are sent on to
  the component. A query that arrives before the component is there (or after the loader failed)
  is answered `Nothing`, as one sent to an empty slot: `H.request` gives `Nothing`, `H.tell` does
  nothing. The wrapper does not keep it for later.
- When the loader throws, it shows `loading.failed` with the exception's `displayException`, until
  it is mounted again. An asynchronous exception, the wrapper going away while the loader waits,
  is not caught.

`Lazy.Loading` is two pieces of plain HTML (`HH.PlainHTML`, no events), and `Lazy.defaultLoading`
is an empty `<div>` and the error in a `<pre>`.

## Loading once

Each mount runs the loader again. `Lazy.memo` shares one load between them:

```haskell
initialState _ = do
  editor <- Lazy.memo loadEditor
  pure State {editor, ...}

render state = ... HH.slot "editor" () (Lazy.lazy Lazy.defaultLoading state.editor) ...
```

The first use loads, and every later one gets what it loaded, at once; a use that comes while the
load is under way waits for it. A load that fails is not kept, so the next mount tries again, and
one cancelled (its wrapper removed) is left to the next.

## Code loaded on demand: `Halogen.Lazy.Dyld`

On WebAssembly, the code of a component can be a shared library that GHC's dynamic linker
(`dyld.mjs`, which ships with the wasm GHC) links into the running program when the component is
first shown:

```haskell
import Halogen.Lazy.Dyld qualified as Dyld

feature :: Dyld.Library
feature = Dyld.library "lib/libfeature.so" "feature_entry"

loadFeature :: BrowserDOM (H.Component Feature.Query Text Feature.Output BrowserDOM)
loadFeature = liftIO (Dyld.load feature)
```

`Dyld.load` has the page fetch the library, puts it in the program's file system (at
`/tmp/halogen-lazy/` and its file name; set `path` for another), has dyld link it, with whatever it
needs that is not linked yet, and calls its entry, which hands over a `Dynamic`. It checks that
the `Dynamic` holds the type asked for, and throws `Dyld.WrongType` when it does not,
`Dyld.LinkFailed` when the page could not fetch, link or call it, and `Dyld.NotAvailable` where
there is no dyld. A library is fetched and linked once per path; a later load of it only calls the
entry again.

The library is a module compiled with `-shared -dynamic` against the very libraries the program was
built with, which it links to by their file names, and it exports its entry:

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

What to keep in mind:

- **The types are shared.** The program and the library must agree on the component's type, every
  type in it: the query, input and output types belong in a library both are built against,
  never in the shared library alone, whose types the program cannot name.
- **Names are global.** dyld keeps the exports of every library in one table, and knows a
  library by its file name: both must be unique among everything the program links. Give the
  library a unit id of its own (`-this-unit-id`), so that its modules' symbols cannot meet the
  program's (both would be `main`'s otherwise).
- **The page hands dyld over.** The page that started the program with dyld sets
  `globalThis.halogenDyld = { dyld, rootfs, shim }`: `dyld` as `dyld.mjs`'s `main(...)` resolved
  it, `rootfs` the `browser_wasi_shim` `PreopenDirectory` its `DyLDBrowserHost` reads files from,
  and `shim` that `browser_wasi_shim` module (its `File` and `Directory`). `Dyld.available` says
  whether it did.

### How the library is linked and called

There is nothing behind `Dyld.load` but dyld's own API, called from `lazy/jsbits/lazy.js` while the
Haskell thread that asked waits for its promise (`Halogen.JSBits.Promise.await`):

1. `fetch` the library, and write its bytes at `path` into the file system dyld reads (the
   `browser_wasi_shim` directory, with that module's `File` and `Directory`).
2. `await dyld.loadDLLs(path)`, which instantiates the library in the program's one memory and
   table, resolves its imports against what is linked already (finding any NEEDED library not
   linked yet in its search directories, by file name), runs its initialisers (`_initialize`),
   and adds its exports to `dyld.exportFuncs`.
3. Call `dyld.exportFuncs[entry]()`. A `foreign export javascript` is an exported function of the
   library like any other; it runs the Haskell entry on the program's RTS and resolves with its
   result, a `StablePtr` (a number); `Dyld.load` dereferences and frees it.

This needs no registration of its own: the RTS was started once (`__ghc_wasm_jsffi_init`) when
dyld loaded the program, and a library linked later runs on it, its exports included. It was
checked in Node with dyld's `DyLDHost` (a library whose `foreign export javascript` is linked and
called from inside an awaited call of the program's, twice), and in the page by
`npm run test-playground` and the tutorial's `npm run test-learn`.

A link that fails cannot be tried again in the same page: dyld marks a library's name as loaded
before it links it, and has no way to forget it. So a failed link's `LinkFailed` is given again to
every later `load` of the path, until the page is reloaded; a failed fetch is tried again.

### Where it works, and what it costs

Only in a program that GHC's dyld started. The tutorial's page is one: it runs GHC itself as a
shared library, the reader's program runs inside it, and its lesson 18 loads a component from
`demo/feature` (built into `liblazy-demo.so` by `toolchain/build-learn-rootfs.sh`, with its types
in `demo/`'s `haskell-halogen-lazy-demo`). A program linked into one `.wasm`, as every example here
and a release build are, has no linker to link more into it; nor has a program of the JavaScript
backend or a native one. There `Dyld.load` throws `NotAvailable`, and `Lazy.lazy` works the same
with any other loader.

An application that wants it runs on dyld from the start, as the tutorial's rootfs does
([`learn/README.md`](../learn/README.md)): its own code and every library it uses are shared
libraries, the boot libraries included, loaded by `dyld.mjs` from a file system the page fills.
That is far more than one linked program, which the linker trims to what it uses: the tutorial's
app is 2.5 MB as one `app.wasm` (0.8 MB compressed), while `haskell-halogen-core` and the
libraries it needs beyond GHC's are 84.5 MB as shared libraries (7.2 MB compressed), GHC's
`ghc-internal` is another 25.7 MB and the C libraries 6.8 MB, and dyld takes seconds to link them
where a linked program starts at once. It pays for an application large enough that most of it is
rarely needed, so that what each visit downloads is the shared core and the parts it uses; for a
small application, one linked `.wasm` is smaller and faster.

## Backends

`Halogen.Lazy` is pure Halogen and works on every backend. `Halogen.Lazy.Dyld` builds on every
backend, through `Halogen.JSBits.browserJS`, and links only on WebAssembly under dyld.
