# learn/: GHC in the page

This is the feasibility proof for an in-browser tutorial: GHC itself, compiled
to WebAssembly, runs in a static page, compiles a Halogen component the reader
wrote and mounts it into the page, with no server behind it. It follows GHC's
own browser playground (the testsuite's `ghc-api-browser/playground001`) and
[try-miso](https://try.haskell-miso.org), which does the same for miso.

What is here:

- `Playground.hs`: the GHC API wrapper, built into `libplayground.so`. It
  exports `playground`, which starts one GHC session (bytecode backend,
  in-memory linking) and hands back the function a Run calls: it writes the
  source to `/tmp/Main.hs`, parses the arguments as GHC flags, loads the
  module and runs `Main.main` in the in-process interpreter.
- `web/index.html`, `web/index.js`: a minimal page (a text area, the GHC
  arguments, Run, stdout and stderr, `#app`, and the timings it measured).
  Plain modules, no bundler, no editor.
- `../toolchain/build-learn-rootfs.sh` (`npm run build-learn`): builds
  everything the page loads into `dist-newstyle/wasm/learn/`.
- `../toolchain/learn-inject.py`: registers a layer of packages in the
  rootfs; the script calls it.
- `../toolchain/test-learn.mjs` (`npm run test-learn`): drives the page in
  headless Chromium and prints what it measured.

## Build and run

```sh
npm run build-learn               # dist-newstyle/wasm/learn/, zstd -19 (a minute and a half on 4 cores)
ZSTD_CLEVEL=3 npm run build-learn # the same in seconds, 15 MB larger
LEARN_REPORT=1 npm run build-learn # also each part's compressed size (twice as long)
npm run test-learn                # headless Chromium, prints timings and memory
python3 -m http.server -d dist-newstyle/wasm/learn 8000   # or any static server
```

The script builds `haskell-halogen-jsbits`, `haskell-halogen-core` and
`haskell-halogen-hooks` for wasm first, as `toolchain/build-wasm.sh` builds
the examples (a no-op when they are built); extra arguments go to cabal
(`npm run build-learn -- -j2`). It needs the pinned wasm toolchain on the
PATH (or `~/.ghc-wasm/env`), `python3`, `zstd`, GNU `tar` and `curl` (once,
for `bsdtar.wasm`). The output is under `dist-newstyle/`, which `.gitignore`
already covers.

The page needs nothing but a static server: no COOP/COEP headers,
`crossOriginIsolated` is false and there is no `SharedArrayBuffer` (the test
checks this). Its one request off the site is browser_wasi_shim from esm.sh
(below).

`?rts=` on the page's address sets the RTS options (default `-c`), for trying
others: `?rts=-c+-H64m`. The test passes `LEARN_QUERY='rts=-c -H64m'` on.

## The rootfs

`rootfs.tar.zst` extracts to `/tmp` in the page's in-memory file system
(browser_wasi_shim's), which GHC sees as its file system:

| Path | What |
| --- | --- |
| `/tmp/libplayground.so` | `Playground.hs`, built `-package ghc -shared -dynamic -O2` |
| `/tmp/clib/` | the wasi-sysroot's `.so` (libc, libc++, ...), without `libsetjmp` and `libwasi-emulated-*` |
| `/tmp/hslib/lib/` | GHC's libdir: `settings`, `package.conf.d`, and in `wasm32-wasi-ghc-<version>-<abi>/` every `libHS*.so`, each unit's `.dyn_hi` tree in a directory of its own |
| `/tmp/hslib/layers/<name>/` | a further layer, from its own archive: `package.conf.d/`, `lib/*.so`, a `.dyn_hi` tree per unit |

The libdir is trimmed as the reference trims it: Cabal and Cabal-syntax are
unregistered and deleted, and so is everything a dynamic, non-profiled
compiler does not read (`.hi`, `.a`, profiling `.so` and interfaces, the debug
RTS, the documentation, the libdir's `.mjs`/`.js`/`.txt`).

The **base layer** is `haskell-halogen-jsbits`, `haskell-halogen-core` and the
closure of their dependencies that GHC does not ship (62 units: aeson, vector,
clay, protolude, unliftio, row-types, ...), found from the `depends:` of the
`.conf` files in the project's in-place DB and cabal's store (via
`dist-newstyle/wasm/cache/plan.json` for the store's directory), never from a
list. Each unit's `.so` goes into the boot libraries' directory (the one
directory dyld searches), its `.dyn_hi` tree into a directory named after the
unit beside them, and its `.conf` into the global DB with every path
`${pkgroot}`-relative, as the boot packages' are; `haddock-*`, `data-dir`,
`pkgroot`, `include-dirs` and `includes` are dropped. try-miso writes
`${topdir}`, which GHC reads the same, but `ghc-pkg check` does not expand it
and fails; with `${pkgroot}` the script runs `ghc-pkg recache` and `check` on
the result. A program needs only `-package haskell-halogen-core`.

The page also gets `rootfs.json` (the search directories, the libdir, the
layers and their package DBs, so the page holds no GHC version),
`dyld.mjs`/`post-link.mjs`/`prelude.mjs` from the libdir, and `bsdtar.wasm`.

### Layers

Further packages (hooks, router, net, material, pixi, th) are meant to arrive
as archives of their own, extracted on top of the base one, so that a chapter
downloads only what it uses. The script builds one by default, to prove the
mechanism: `layers/hooks.tar.zst` with `haskell-halogen-hooks`
(`LEARN_LAYERS='hooks=haskell-halogen-hooks router=haskell-halogen-router'`
for more, each on top of those before it; `LEARN_LAYERS=` for none). A layer is
the closure of its packages minus everything the global DB and the layers
below it have, in a **package DB of its own**,
`/tmp/hslib/layers/<name>/package.conf.d`, with its `package.cache` built at
build time and its paths `${pkgroot}`-relative, so it holds wherever it is
extracted. Its `.so` stay in its own `lib/` (its `dynamic-library-dirs`),
which works: GHC's loader finds them there, and their own dependencies resolve
through the base search directories. The page passes `-package-db <that DB>` to GHC (it adds it to the
arguments box), and the test compiles a hooks counter from it and clicks it.

That is the practical way, for two reasons found here:

- **GHC caches the package DBs it read first.** A session reads its unit
  databases on the first `setSessionDynFlags` and reuses them afterwards
  (`initUnits` is given the cached ones), so a `-package-db` that a later run
  adds is ignored: "cannot satisfy -package haskell-halogen-hooks" although the
  DB was there. `Playground.hs` forgets the cached DBs before it sets each
  run's flags, so a layer extracted after the first run is seen by the next.
  Rereading the DBs costs nothing measurable.
- **A `.conf` dropped into the global DB is not seen without a recache**, as
  GHC reads a DB's `package.cache` rather than its `.conf` files (checked with
  the host compiler on the extracted rootfs: "cannot satisfy -package" until
  `ghc-pkg recache`), and there is no `ghc-pkg` in the page. A layer could carry a whole replacement global
  `package.cache`, but then layers would have to be extracted in one fixed
  order, and every combination would need its own cache. A DB per layer has
  neither problem.

## Measurements

Measured here: a 4-core cloud VM, headless Chromium (Playwright's 1243
build), the page served from localhost by `test-learn.mjs`, the release build
(`zstd -19`, dyld patched as below). Times are three runs of
`npm run test-learn` (four with the final clean build).

### Sizes

Compressed sizes are each part compressed alone at `zstd -19`; they add up to
the archive's within half a megabyte.

| Part | Uncompressed | zstd -19 |
| --- | ---: | ---: |
| `libplayground.so` | 0.06 MB | 0.01 MB |
| `/tmp/clib` (.so) | 6.8 MB | 0.8 MB |
| boot libraries (.so) | 243.3 MB | 35.8 MB |
| &nbsp;&nbsp;of which `libHSghc` | 169.5 MB | 25.6 MB |
| boot libraries (.dyn_hi) | 56.1 MB | 14.3 MB |
| &nbsp;&nbsp;of which `ghc`'s own | 32.2 MB | 8.4 MB |
| base layer (.so, 62 units) | 84.5 MB | 7.2 MB |
| base layer (.dyn_hi) | 18.8 MB | 4.1 MB |
| rest of the libdir (settings, package DB) | 1.4 MB | 0.2 MB |
| **`rootfs.tar.zst`** | **411.1 MB** | **62.9 MB** |
| `layers/hooks.tar.zst` | 0.8 MB | 0.13 MB |
| `bsdtar.wasm` | | 1.2 MB |
| `dyld.mjs` + `post-link.mjs` + `prelude.mjs`, page | | 0.07 MB |

At `zstd -3` the archive is 77.8 MB. The largest file served is
`rootfs.tar.zst`, 62.9 MB.

### Time

| Step | Time |
| --- | ---: |
| fetch `rootfs.tar.zst` (localhost) | 0.2–0.6 s |
| extract (bsdtar.wasm into memory) | 2.6–2.8 s |
| dyld load (GHC and the boot libraries it links, until `playground` returned) | 4.3–4.4 s |
| run 1: the counter, the first program to use `haskell-halogen-core` (loads the base layer's 62 libraries, compiles, mounts) | 3.8–4.1 s |
| run 2: the same source again | 1.2–1.4 s |
| run 3: the source edited | 1.3–1.4 s |
| run 4: a hooks counter, the first use of the hooks layer | 1.1–1.2 s |
| for comparison: `main = putStrLn "hello"` before anything else | 0.4 s |

So the page is ready about 7.5 s after the archive has arrived, and the
counter is on screen 4 s after the first Run; after that an edit runs in
about 1.3 s. Over the network the download dominates: 63 MB is about 20 s at
25 Mbit/s, 5 s at 100 Mbit/s.

**dyld patch.** GHC's `dyld.mjs`, as it ships, builds each library's import
object with `{ ...this.exportFuncs }`: a copy of every function every library
loaded before it exports, some hundred thousand once GHC is loaded, again for
each of the base layer's 62 libraries. A CPU profile of run 1 put 8.5 s of its
11 s in that one function. The build script patches the copy it ships to look
up only the functions a library imports (in dyld's existing loop over the
imports, which is equivalent), and says so; if a later GHC's dyld has changed
there, it ships it unpatched. Unpatched: dyld load 4.7–5.1 s, run 1
11.0–11.9 s; the other runs are the same. This belongs upstream in GHC's
`utils/jsffi/dyld.mjs`.

### Memory

| After | JS heap (DevTools, after a GC) | wasm memory | renderer RSS |
| --- | ---: | ---: | ---: |
| load | 168 MB | 33 MB | 1.12 GB |
| run 1 | 225 MB | 67 MB | 1.20 GB |
| run 4 | 226 MB | 85 MB | 1.19 GB |

The renderer's peak resident memory (VmHWM) was 1.20–1.32 GB across runs.
`performance.memory` (which the page shows) is rounded by Chrome and lags;
the test reads the heap through the DevTools protocol and the renderer's
memory from `/proc`.

Where it goes, roughly: dyld gives every library one shared wasm memory
(GHC's heap, 33–85 MB, which only grows: 4–9 MB a run), the JS heap holds
dyld's symbol tables (a function and a `WebAssembly.Global` per symbol), and
the rest is V8's: the wire bytes of some 330 MB of loaded wasm, which it keeps
for lazy compilation, the code compiled so far, and the files still in the
in-memory file system (the `.dyn_hi`, the libraries not loaded). V8's tiering
flags barely move it (`--no-wasm-tier-up` and `--liftoff-only` the same,
eager compilation 2 GB at load); lazy compilation is already the default.

### RTS flags

None are needed. The reference's `+RTS -c` (compacting collection of the old
generation) keeps the wasm memory smaller after a few runs (69 and 77 MB
after runs 2 and 3, against 82 and 95 MB without it), at no cost in time, so
the page keeps it. `-H64m` only raised the memory at load to 87 MB.

### wasm-opt

GHC's own build can run every `.so` through `wasm-opt` with `WASM_SO_OPT`
(`--debuginfo --low-memory-unused --strip-dwarf -Oz`); the script does this
for the layers' libraries when `WASM_SO_OPT` is set. Results:

- It works after the fact on a dynamic-linking `.so`: dyld loads the result
  and the test passes.
- It takes the base layer's `.so` from 84.5 to 58.0 MB (core's from 6.9 to
  5.2 MB), but compressed they are the same size (core's 700 KB before and
  after, the archive 0.5 MB smaller at `-3`), and run 1 took the same time.
  Renderer peak memory was about 100 MB lower in one run. It adds two minutes
  to the build. Not worth it by default.
- On `libHSghc` (170 MB) wasm-opt ran out of this machine's 15 GB after
  four minutes. Only a GHC built with `WASM_SO_OPT` would have that one
  optimised.

## Pages

- **Per file**: the largest file is 62.9 MB, under the 100 MB GitHub limit. If
  it grows past it (more boot libraries, a thicker base layer), the archive
  splits naturally into several extracted in sequence, which the page already
  does for layers: for example `libHSghc` alone, the other boot libraries with
  the libdir, and the base layer. Splitting a single tarball into byte ranges
  and concatenating them in the page before extracting works as well.
- **Site size**: the playground is 64 MB beside the gallery, far from the 1 GB
  limit; each further layer is small (hooks 0.13 MB).
- **Bandwidth**: Pages has a soft limit of 100 GB a month, about 1,600 cold
  loads of the playground. Its responses are cacheable (10 minutes, then
  revalidated with an ETag), so a returning reader does not download it again,
  and the page could keep the archive in the Cache API across visits.
- **Headers**: none needed (no COOP/COEP), and whatever type Pages serves
  `.zst` with, the page reads it as bytes.

## What this proves, and what it does not

It proves:

- GHC 9.14's API runs in a page from a static server, and compiles and runs a
  module that uses `haskell-halogen-core`, whose libraries were built by this
  project's own wasm build and injected into the global package DB.
- Interpreted code calls into our compiled libraries, whose JSFFI bindings
  (`Halogen.JSBits.browserJS`) work as they do in a linked program: `runUI`
  from the bytecode mounts the counter, its click listeners call back into the
  RTS, and the counter counts.
- Run twice, the second tree replaces the first: the program's `main` calls
  `Halogen.IO.Util.endPreviousRun` and `keepForNextRun socket.dispose`, as the
  examples' interactive mode does, and since core is loaded once per session
  the reference it keeps survives from one run to the next.
- A further layer, in an archive and a package DB of its own, works, including
  after runs without it.
- Template Haskell splices run in the page (`$(litE ...)`).

It does not show:

- A program cannot declare a `foreign import javascript` of its own: GHC
  compiles such a module to an object file, which needs clang
  (`createPipe: unsupported operation`). A program uses JavaScript through
  library bindings only; a tutorial chapter about FFI would need its bindings
  in a layer.
- Anything but Chromium on a fast desktop. Firefox and Safari are untested
  here (GHC's own playground is tested on all three, and dyld works around a
  WebKit `table.grow` bug), and so are phones.
- The other layers: `th` depends on `ghc-lib-parser`, which is about the size
  of `libHSghc` again; material and pixi need their JavaScript on the page as
  well.
- The editor, error markers, persistence, or loading layers on demand from the
  tutorial's chapters (the mechanism is there; the page loads every layer at
  start).

## Risks for the tutorial

- **Memory**: 1.2–1.3 GB of renderer memory. Desktop browsers are fine; many
  phones kill a tab well before that, and iOS Safari's limits are tighter
  still. A mobile reader may need a "read-only" fallback (prerendered results)
  rather than a live compiler.
- **Download**: 63 MB before the first compile, and Pages' bandwidth limit
  above. Keeping the archive in the Cache API, and trimming what no chapter
  needs (`ghc`'s own interfaces are 8.4 MB compressed and only a program that
  imports the GHC API reads them; haddock-api, haskeline and the like) would
  help.
- **Slow machines**: the numbers above are a fast VM. Extracting and loading
  are single-threaded JavaScript and wasm instantiation; on a slow laptop or a
  phone expect several times as long. A progress display matters.
- **Safari**: untested here; WebKit is the browser most likely to fall short
  on memory.
- **Keeping it in step**: the rootfs must be built with exactly the GHC that
  `cabal-wasm.project` pins and from the same build of our packages (the base
  layer's `.so` and interfaces are ABI-bound to both). The script takes both
  from the project's wasm build, so a GHC bump or a package change needs only a
  rebuild, but the tutorial's text must be checked against the packages it was
  built with. browser_wasi_shim is imported from
  `https://esm.sh/gh/haskell-wasm/browser_wasi_shim`, unpinned, because
  `dyld.mjs` imports it from there and the page must share its copy; vendoring
  it means downloading a pinned build (esm.sh serves
  `gh/haskell-wasm/browser_wasi_shim@<commit>`) and rewriting that import in the
  shipped `dyld.mjs`, as the dyld patch rewrites its import object.
- **dyld patch**: it is ours until GHC takes it; the script checks the code it
  replaces and leaves dyld alone if it changed.

## Provenance

- `bsdtar.wasm`: [haskell-wasm/bsdtar-wasm](https://github.com/haskell-wasm/bsdtar-wasm),
  libarchive's bsdtar built for wasm32-wasi with zstd, BSD-2-Clause
  (Copyright (c) 2025 Cheng Shao); downloaded once from
  `https://haskell-wasm.github.io/bsdtar-wasm/bsdtar.wasm`, the build GHC's
  playground loads, and pinned by its SHA-256 in the script.
- browser_wasi_shim: [haskell-wasm/browser_wasi_shim](https://github.com/haskell-wasm/browser_wasi_shim),
  MIT OR Apache-2.0, through esm.sh.
- `dyld.mjs`, `post-link.mjs`, `prelude.mjs`: GHC's, from the toolchain's
  libdir (BSD-3-Clause), `dyld.mjs` patched as above.
