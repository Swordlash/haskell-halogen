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
  module and runs `Main.main` in the in-process interpreter. stdout and
  stderr, which the program shares, are line-buffered and flushed when a run
  ends, so the page gets what a program writes as it writes it.
- `web/index.html`, `web/index.js`: a minimal page (a text area, the GHC
  arguments, a button per layer, Run, stdout and stderr, `#app`, and the
  timings it measured). Plain modules, no bundler, no editor.
- `../toolchain/build-learn-rootfs.sh` (`npm run build-rootfs`): builds
  everything the page loads into `dist-newstyle/wasm/learn/`.
- `../toolchain/learn-inject.py`: registers a layer of packages in the
  rootfs; the script calls it. `../toolchain/learn-rootfs-key.py`: what the
  script's `--key` prints, for CI's cache.
- `../toolchain/test-playground.mjs` (`npm run test-playground`): drives the
  page in headless Chromium, with a program for every layer, and prints what
  it measured.

The tutorial, [`examples/learn`](../examples/learn/README.md), runs on the
same rootfs: its build copies this directory, but for the page, to
`rootfs/` beside the app, and that is what GitHub Pages deploys. This page
stays a local tool, for trying the compiler with nothing around it.

## Build and run

```sh
npm run build-rootfs                # dist-newstyle/wasm/learn/, zstd -19 (two and a half minutes on 4 cores)
ZSTD_CLEVEL=3 npm run build-rootfs  # the same in 20 seconds, 18 MB larger
LEARN_REPORT=1 npm run build-rootfs # also each part's compressed size (five minutes)
npm run test-playground             # headless Chromium, prints timings and memory
LEARN_ONLY=th npm run test-playground  # the base runs and only the layers named
python3 -m http.server -d dist-newstyle/wasm/learn 8000   # or any static server
sh toolchain/build-learn-rootfs.sh --key   # the hash CI caches the output under (below)
```

The script builds `haskell-halogen-jsbits`, `haskell-halogen-core` and every
layer's packages for wasm first, as `toolchain/build-wasm.sh` builds the
examples (a no-op when they are built); extra arguments go to cabal
(`npm run build-rootfs -- -j2`). It needs the pinned wasm toolchain on the
PATH (or `~/.ghc-wasm/env`), `python3`, `zstd`, GNU `tar` and `split`, `curl`
(once, for `bsdtar.wasm`), and `npm ci`'s esbuild and sass (for the material
layer's assets). The output is under `dist-newstyle/`, which `.gitignore`
already covers.

The page needs nothing but a static server: no COOP/COEP headers,
`crossOriginIsolated` is false and there is no `SharedArrayBuffer` (the test
checks this). It requests nothing off the site but the layers' external
dependencies (PixiJS, a font), when they are used: browser_wasi_shim is
`vendor/`'s copy, shipped beside `dyld.mjs` (below).

Packing is reproducible: GNU tar packs the members sorted by name, with
every time set to the epoch and no owner, so packing the same built
libraries again gives the same bytes and the manifest the same `version`
(two builds here were byte for byte the same; zstd with several threads
gives the same bytes for the same level whatever the number of threads).
Compiling a library again need not give the same bytes, as GHC's output is
not deterministic by default: after a rebuild of `haskell-halogen-core`
every archive but `th`'s changed, and so did the version. That is why CI
does not rebuild the rootfs for nothing: it caches the output directory under
`build-learn-rootfs.sh --key`, which plans the wasm build and prints a hash
of everything the output is made from: the compiler (its id and ABI hash),
every unit of the plan in the closure of the packages packed (one from the
store or GHC by its id, which hashes its own inputs; one of ours by its
flags and its package directory's files), the wasi-sysroot's libraries, the
files the script reads (itself, `learn/`, material's stylesheet and bundle
script, `package-lock.json`) and its settings. A hit restores the
directory and the build is skipped.

`?rts=` on the page's address sets the RTS options (default `-c`), for trying
others: `?rts=-c+-H64m`. The test passes `LEARN_QUERY='rts=-c -H64m'` on.
`?layers=router,net` (or `?layers=all`) loads those layers as soon as the
compiler has; otherwise a layer loads when its button is pressed.

## The rootfs

`rootfs.tar.zst` extracts to `/tmp` in the page's in-memory file system
(browser_wasi_shim's), which GHC sees as its file system:

| Path | What |
| --- | --- |
| `/tmp/libplayground.so` | `Playground.hs`, built `-package ghc -shared -dynamic -O2` |
| `/tmp/clib/` | the wasi-sysroot's `.so` (libc, libc++, ...), without `libsetjmp` and `libwasi-emulated-*` |
| `/tmp/hslib/lib/` | GHC's libdir: `settings`, `package.conf.d`, and in `wasm32-wasi-ghc-<version>-<abi>/` every `libHS*.so`, each unit's `.dyn_hi` tree in a directory of its own |
| `/tmp/hslib/layers/<name>/` | a further layer, from its own archive: `package.conf.d/`, `lib/*.so`, a `.dyn_hi` tree per unit |

The libdir is trimmed as the reference trims it: everything a dynamic,
non-profiled compiler does not read is deleted (`.hi`, `.a`, profiling `.so`
and interfaces, the debug RTS, the documentation, and the libdir's
`.mjs`/`.js`/`.txt`), and the boot packages' `.conf` no longer name the
documentation. Beyond that it is trimmed to what GHC and a reader's program
can use (next section).

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

The page also gets `rootfs.json` (the manifest, below), `dyld.mjs`,
`post-link.mjs` and `prelude.mjs` from the libdir, `browser_wasi_shim.js` from
`vendor/browser_wasi_shim/`, and `bsdtar.wasm`.

### Trimming the boot libraries

GHC ships 46 boot packages. The ones that neither GHC (the closure of `ghc`,
`ghci`, `ghc-boot`, `template-haskell`, `hpc` and `ghc-heap`, which are kept)
nor any package of the base layer or a layer depends on are unregistered and
their `.so` and interfaces deleted (`BOOT_TRIM` in the script): `Cabal` and
`Cabal-syntax` (the first to go), `haddock-api`, `haddock-library`, `xhtml`,
`haskeline`, `ghc-toolchain`, `ghc-compact`, `integer-gmp` and `ghc-bignum`
(the last two are compatibility shims over `ghc-internal`). `ghc-pkg
unregister` refuses one that a boot package left depends on, and `ghc-pkg
check` runs on the result. That holds for boot packages only: the trimming
runs before the base layer and the layers are injected, so ghc-pkg never
sees their packages' dependencies on a trimmed one. For those,
`learn-inject.py` is told the trimmed units (`--trimmed`) and stops the
build on a dependency on one, naming it and `BOOT_TRIM`; the list is chosen
by hand, from what the base layer and the layers depend on today.
`ghc-platform` cannot go: `ghc-boot` depends on it.
`parsec` and `text` are not in GHC's closure either, but the base layer
needs them.

The `ghc` package itself is **hidden**, and its interfaces (32.2 MB) are left
out: only a program that imports the GHC API reads them, and no lesson does.
Its `.conf` keeps its library directory but lists no modules, so `ghc-pkg
check` holds and `-package ghc` with `import GHC.Data.FastString` says
"Could not find module" rather than failing on a missing interface. Its
library stays, as `libplayground.so` is linked with it and dyld loads it by
file name. `LEARN_GHC_API=1` keeps `ghc` exposed with its interfaces (the
archive is then 8.4 MB larger); a program using the GHC API was checked to
compile and run in the page with it, and to fail as above without.

What it saved, at `zstd -19`:

| | Uncompressed | zstd -19 |
| --- | ---: | ---: |
| the eight packages beyond Cabal (`.so` and `.dyn_hi`) | 14.1 MB | 2.4 MB |
| `ghc`'s interfaces | 32.2 MB | 8.4 MB |
| **`rootfs.tar.zst`** | 411.1 → **364.7 MB** | 62.9 → **52.0 MB** |

Every test run passes on the trimmed rootfs, Template Haskell and the `th`
layer's quasi-quoter included.

### Layers

Every other package of this project a lesson can use is a **layer**: an
archive of its own, extracted on top of the base one when a lesson asks for
it, so that a lesson downloads only what it uses. A layer is the closure of
its packages minus everything the global DB has, in a **package DB of its
own**, `/tmp/hslib/layers/<name>/package.conf.d`, with its `package.cache`
built at build time and its paths `${pkgroot}`-relative, so it holds wherever
it is extracted. Its `.so` stay in its own `lib/` (its
`dynamic-library-dirs`): GHC's loader finds them there, and their own
dependencies resolve through the base search directories. The page passes
`-package-db <that DB>` and `-package <package>` for each of its packages to
GHC (it adds them to the arguments box when the layer has loaded).

**A layer stands on the base layer alone, never on another layer**, so that a
lesson can load any set of layers, in any order, and each archive is valid on
its own. A library two layers both need belongs in the base layer:
`learn-inject.py` is given the other layers' DBs (`--taken`) and stops the
build if a layer's closure reaches a unit another layer already carries, as
two copies of one library in a session would be two libraries to dyld. Today
no two layers share anything: hooks, router, net, elements, devtools, pixi,
store and worker need only themselves beyond the base layer, lazy its two packages
(`haskell-halogen-lazy` and `haskell-halogen-lazy-demo`), material also `clock` and `extra`, th its
parsers, hspec hspec's own closure (`hspec`, `hspec-core`, `HUnit`,
`ansi-terminal` and the rest; `QuickCheck` and its generators are in the
base layer already), none of it a boot package the script trims.

The layers (`DEFAULT_LAYERS` in the script; `LEARN_LAYERS='name=pkg,pkg
...'` for others, `LEARN_LAYERS=` for none). Sizes are the archive's at
`zstd -19`; load time is the page's, from asking for the layer to its files
extracted and its assets loaded; first run is the first program using it,
which loads its libraries into dyld and compiles. Three runs of
`npm run test-playground`:

| Layer | Packages | Units | Uncompressed | zstd -19 | Assets on the page | Load | First run |
| --- | --- | ---: | ---: | ---: | --- | ---: | ---: |
| `hooks` | `haskell-halogen-hooks` | 1 | 0.81 MB | 0.13 MB | none | 28–33 ms | 1.2–1.3 s |
| `router` | `haskell-halogen-router` | 1 | 0.88 MB | 0.14 MB | none | 22–23 ms | 2.8–3.5 s |
| `net` | `haskell-halogen-net` | 1 | 0.68 MB | 0.11 MB | none | 21–29 ms | 0.6–0.8 s |
| `elements` | `haskell-halogen-custom-element` | 1 | 0.43 MB | 0.08 MB | none | 22–25 ms | 0.5–0.7 s |
| `devtools` | `haskell-halogen-devtools` | 1 | 0.71 MB | 0.11 MB | none | 23–24 ms | 1.2–1.5 s |
| `material` | `haskell-halogen-material` (+ `clock`, `extra`) | 3 | 2.58 MB | 0.36 MB | `material.js` 127 KB, `material.css` 206 KB; the Material Icons font from fonts.gstatic.com | 56–65 ms | 0.7–1.3 s |
| `pixi` | `haskell-halogen-pixi` | 1 | 0.97 MB | 0.15 MB | none; PixiJS 8.20.1 from jsDelivr (0.8 MB), which the library imports itself | 27–31 ms | 1.4–1.6 s |
| `th` | `haskell-halogen-th` (+ `ghc-lib-parser`, `megaparsec`, `parser-combinators`, `tagsoup`, `css-syntax`, `case-insensitive`) | 7 | 194.7 MB | 21.4 MB | none | 2.1–3.9 s | 8.0–12.0 s |
| `hspec` | `hspec-halogen` (+ `hspec`, `hspec-core`, `hspec-expectations`, `hspec-discover`, `HUnit`, `quickcheck-io`, `ansi-terminal`, `ansi-terminal-types`, `colour`, `call-stack`, `haskell-lexer`) | 12 | 12.6 MB | 1.34 MB | none | 98–100 ms | 3.6–4.1 s |
| `store` | `haskell-halogen-store` | 1 | 0.49 MB | 0.07 MB | none | 23–25 ms | 0.9–1.7 s |
| `worker` | `haskell-halogen-worker` | 1 | 0.20 MB | 0.04 MB | none | 20–21 ms | 7.4–9.5 s |
| `lazy` | `haskell-halogen-lazy`, `haskell-halogen-lazy-demo` | 2 | 0.27 MB | 0.06 MB | none; `liblazy-demo.so` (64 KB), which its program fetches itself (below) | 19–27 ms | 6.2–7.2 s |
| **all twelve** | | 32 | 215.4 MB | 24.5 MB | 0.33 MB | 1.9–2.3 s | |
| all but `th` | | 25 | 20.7 MB | 2.61 MB | 0.33 MB | 0.37–0.39 s | |

(The `hspec` row is from three later runs, on a machine other builds were
sharing; `th` loaded in 1.6–1.8 s in them. The `elements` row is from
three runs later again, of a rootfs in which `th`'s archive was 21.9 MB and
`material`'s 2.61 MB uncompressed, and `th` loaded in 1.5–1.8 s. The
`store` and `devtools` rows are each from three runs of a rootfs with that
layer added to the eight before it, in which `th` loaded in 1.6–2.0 s; the
two totals were those runs' totals with the other row's sizes added. The
`worker` row and the two totals are from three runs of a rootfs with all
eleven layers, whose sizes the totals add up (`hooks` 0.82 MB, `material`
2.62 MB and `th`'s archive 21.9 MB in it); `th` loaded in 1.6–2.0 s. Every
first run after `th`'s was slower in those runs than in the earlier ones,
`store`'s 6.1–7.0 s and `hspec`'s 6.4–7.5 s, so the worker program's
7.4–9.5 s says as much about its place last in the sequence as about the
layer. The `lazy` row and the two totals are from three runs of a rootfs
with all twelve layers, whose sizes the totals add up (`th`'s archive 21.9
MB in it); `th` loaded in 1.6–1.9 s. The same goes for `lazy`'s first run,
last of all: 6.2–7.2 s, where `store`'s was 3.6–6.2 s and `hspec`'s 6.4–7.2 s
in the same runs, and 1.1 s in a run with only that layer
(`LEARN_ONLY=lazy`). It compiles the program and loads the layer's two
libraries, then the program fetches `liblazy-demo.so` and dyld links it.)
`hspec`'s archive is small enough that the harness is a layer like any
other, with `hspec` itself rather than `hspec-core` alone: `hspec-core` is
4.1 MB of its 10.9 MB of `.so`, `haskell-lexer` 3.0 MB and `colour` 1.4 MB,
and `hspec` adds 0.1 MB.

Of `th`'s, `ghc-lib-parser` is 158.8 MB of `.so` and 22.0 MB of interfaces:
it is about as big as `libHSghc`, and its archive (21.4 MB) is 43% of the
base one, so it stays a layer of its own that only a lesson about the
quasi-quoters loads. It is well under the per-file limit, so it is one file.

What each test run does, so that the layer's `.so` loads and its JavaScript
runs: hooks, a `Hooks.useState` counter, clicked; router, a
`newHashRouter` with a `Router.link`, whose click `Router.follow`s it and
`Router.current` then reads (`#/about`); net, a `Fetch.fetch` of a file the
test's server serves, its text shown; elements, a
`Halogen.CustomElement.define` over HTML already in `#app`, the element's
shadow root then showing its component, which hears the attribute the page
sets afterwards, and the HTML taken out again when the next run starts;
devtools, `Halogen.DevTools.open`'s panel over the page, then a counter in
`#app`, which the panel lists under its name and whose click it counts;
material, `Halogen.Material.Button`
mounted, its ripple attached by `material.js` (`mdc-ripple-upgraded`), its
colour from `material.css`, clicked; pixi, a rectangle on a Pixi canvas
(PixiJS from the CDN), tapped until the tap reaches the scene (Pixi
initialises asynchronously); th, an `[hh| ... |]` with a `{{ }}` in it, whose
quasi-quoter runs ghc-lib-parser in the page's interpreter; hspec, an
`hspec-halogen` spec of two tests, a click and a `typeText`, run with
`runBrowserTestsIn` in `#app`, where with no runner in the page the harness
acts with synthetic events, and hspec's report (`2 examples, 0 failures`)
is the program's stdout, with `#app` empty afterwards; store, a tree
in `StoreT` run with `runStoreT`, whose button's `updateStore` a component
`connect`ed to the store shows; worker, `Halogen.Worker.program` serving a
table of two jobs, `Worker.available` saying no worker can be made (the
interpreted program has no binary for one to load), one job run through
`Worker.run` on the page and the other's exception given back as a
`WorkerError`; lazy, `Halogen.Lazy`'s placeholder while
`Halogen.Lazy.Dyld` fetches `layers/lazy/liblazy-demo.so` and has dyld link it
into the program, then the component its entry handed over, clicked and told
to wave by its parent, which hears it each time. All pass, with no
console errors. If the CDN cannot be reached from the browser, the pixi run
is skipped and says why (it was reachable here, through the proxy).

A note for lessons: on one line, whitespace inside an `hh` quote is text
(JSX's rule), so `[hh| <p>hello</p> |]` is three nodes and `hh` refuses it
("one element, please"); write `[hh|<p>hello</p>|]`, or the element on lines
of its own.

**Assets.** `material`'s components call functions that
`material/jsbits/material.js` puts on `globalThis`; that file imports the
`@material` npm packages, so unlike every other package's jsbits it is not
embedded in its library with `browserJS`. The build runs
`examples/material/bundle.sh` into `layers/material/`, as the example does:
`material.js` (esbuild, an IIFE) and `material.css` (sass), and the page
adds a `<script>` and a `<link>` for them when the layer loads, before any
run can call them. The CSS names the Material Icons font on fonts.gstatic.com,
which only a component showing an icon fetches. `pixi` imports PixiJS from
jsDelivr when a canvas mounts (`Halogen.Canvas.Pixi.defaultConfig`), so it
needs no asset, only the network; the manifest lists both addresses under
`external`. `lazy` carries a library its programs fetch while they run:
`liblazy-demo.so`, the demo library lesson 18 loads with `Halogen.Lazy.Dyld`
(`lazy/demo/feature`, compiled `-shared -dynamic` against the base layer's
libraries and the layer's own, as `libplayground.so` is against GHC's), is
served at `layers/lazy/liblazy-demo.so` beside the archive rather than in it,
since fetching and linking code the program did not start with is what it
shows; the manifest lists it under the layer's `libraries`. For that the page
hands its dyld over as `globalThis.halogenDyld = { dyld, rootfs, shim }` (this
page and the tutorial's both do). Nothing else needs anything: every other
package's JavaScript is in its `.so` (checked: every run above passes with
only these).

That a layer has its own DB is the practical way, for two reasons found here:

- **GHC caches the package DBs it read first.** A session reads its unit
  databases on the first `setSessionDynFlags` and reuses them afterwards
  (`initUnits` is given the cached ones), so a `-package-db` that a later run
  adds is ignored: "cannot satisfy -package haskell-halogen-hooks" although the
  DB was there. `Playground.hs` forgets the cached DBs before it sets each
  run's flags, so a layer extracted after the first run is seen by the next
  (the test loads every layer after three runs). Rereading the DBs costs
  nothing measurable.
- **A `.conf` dropped into the global DB is not seen without a recache**, as
  GHC reads a DB's `package.cache` rather than its `.conf` files (checked with
  the host compiler on the extracted rootfs: "cannot satisfy -package" until
  `ghc-pkg recache`), and there is no `ghc-pkg` in the page. A layer could carry a whole replacement global
  `package.cache`, but then layers would have to be extracted in one fixed
  order, and every combination would need its own cache. A DB per layer has
  neither problem.

**Parts.** An archive larger than `LEARN_PART_MB` (default 95, in units of
10^6 bytes, under the 100 MB GitHub allows a file) is split with `split -b`
into `<archive>.00`, `.01`, ..., and the manifest lists the parts in order;
the page fetches them one after the other and joins the bytes before
extracting. Nothing is that large today (the largest file is
`rootfs.tar.zst`, 50.0 MB); `LEARN_PART_MB=20` was used to check it, with
the base archive in four parts and `th`'s in two, and the test passed.

### The manifest: `rootfs.json`

Everything a page needs to know is in `rootfs.json`, so a page holds no GHC
version, path or size. Paths without a leading `/` are relative to the
manifest (the site); paths with one are in the page's file system. Sizes are
in bytes. This is the one description of it: the tutorial's
`examples/learn/js/playground.js` and this page's `web/index.js` read it.

```json
{
  "ghc": "ghc-9.14.1.20260731",
  "version": "3f9a0c1d2e4b5a67",
  "size": 364664105,
  "compressedSize": 52044047,
  "libdir": "/tmp/hslib/lib",
  "searchDirs": ["/tmp/clib", "/tmp/hslib/lib/wasm32-wasi-ghc-9.14.1.20260731-a678"],
  "mainSoPath": "/tmp/libplayground.so",
  "rootfs": {
    "archive": "rootfs.tar.zst",
    "parts": [{ "path": "rootfs.tar.zst", "size": 52044047 }],
    "compressedSize": 52044047,
    "size": 364664105,
    "packages": ["haskell-halogen-jsbits", "haskell-halogen-core"],
    "units": ["haskell-halogen-jsbits-0.1.0-inplace", "OneTuple-0.4.3-…", "…"]
  },
  "layers": [
    {
      "name": "material",
      "archive": "layers/material.tar.zst",
      "packageDb": "/tmp/hslib/layers/material/package.conf.d",
      "packages": ["haskell-halogen-material"],
      "units": ["clock-0.8.4-…", "extra-1.8.1-…", "haskell-halogen-material-0.2.0.0-inplace"],
      "parts": [{ "path": "layers/material.tar.zst", "size": 360548 }],
      "size": 2575164,
      "compressedSize": 360548,
      "assets": [
        { "kind": "script", "path": "layers/material/material.js", "size": 126809 },
        { "kind": "stylesheet", "path": "layers/material/material.css", "size": 206336 }
      ],
      "external": [
        { "url": "https://fonts.gstatic.com/s/materialicons/v142/….woff2", "what": "the Material Icons font, which material.css names" }
      ],
      "libraries": []
    }
  ]
}
```

| Field | What |
| --- | --- |
| `ghc` | the compiler the rootfs was built with (the page shows it) |
| `version` | the first 16 hex digits of a SHA-256 of every part of every archive (each with its path), in the order of the manifest: it changes whenever any archive's content does, and since packing is reproducible, only then (a recompiled library may change it, above). A page that keeps the archives (the tutorial, in the Cache API) keeps them under it. (The first manifests had none; a page then asks the server for each part's `ETag`.) |
| `size`, `compressedSize` | the base archive's, the same as `rootfs.size` and `rootfs.compressedSize` (below), for a page that reads only the top level |
| `libdir` | GHC's libdir in the page's file system: `playground`'s argument |
| `searchDirs` | where dyld looks for a `.so` by name: dyld's `searchDirs` |
| `mainSoPath` | the library dyld loads first: dyld's `mainSoPath` |
| `rootfs` | the base archive, in the same terms as a layer's (below), extracted at `/` before dyld starts; its `packages` are the base layer's roots and `units` the 62 units of the base layer. (The first manifest had no `rootfs`; for one without it the page fetches `rootfs.tar.zst`.) |
| `layers[]` | one entry per layer, in the order built |
| `.name` | the layer's name: what `?layers=` and `learn.loadLayer` take |
| `.archive` | the archive's name. When it is not split, the file served (all the first page read); when it is, the name its parts join to, which is not served |
| `.packageDb` | the layer's package DB in the page's file system, for `-package-db` once extracted |
| `.packages` | the layer's packages (its roots), each for a `-package` |
| `.units` | every unit in it, its packages and their dependencies beyond the base layer |
| `.parts[]` | the files to fetch, in order, and join: `{ path, size }`; one part, the archive itself, unless it was split |
| `.size` | the extracted files' total size, uncompressed: what it adds to the in-memory file system |
| `.compressedSize` | the parts' total: what a reader downloads, for a progress line and a total |
| `.assets[]` | files to add to the page before the layer's first run: `{ kind, path, size }`, `kind` `"script"` (a classic `<script src>`) or `"stylesheet"` (a `<link rel=stylesheet>`) |
| `.external[]` | what the layer's code loads from elsewhere at run time, which a page cannot provide but should know of (an offline note, a CSP): `{ url, what }` |
| `.libraries[]` | shared libraries served beside the archive, which the layer's programs fetch and have dyld link while they run (`Halogen.Lazy.Dyld`), and which a page need do nothing about: `{ path, size, what }`. Only `lazy` has one, `liblazy-demo.so` |

To load a layer, a page fetches `parts` in order, joins them and extracts them
with `bsdtar -x` into the root directory it extracted the base archive into,
adds `assets` and waits for them to load, and adds `-package-db <packageDb>`
and a `-package <p>` for each of `packages` to the arguments of the next run.
`learn/web/index.js` does exactly this (`loadLayer`), one thing at a time:
a layer does not load while a program compiles.

## Measurements

Measured here: a 4-core cloud VM, headless Chromium (Playwright's 1243
build), the page served from localhost by `test-playground.mjs`, the release build
(`zstd -19`, dyld patched as below). Times are three runs of
`npm run test-playground` on a clean build.

### Sizes

Compressed sizes are each part compressed alone at `zstd -19`, as
`LEARN_REPORT=1` measures them, here of the deployed rootfs; they add up to
51.6 MB, and the archive is 1.6 MB smaller. A local build packed the same
uncompressed sizes into 52.0 MB, so the archive's size moves by a few percent
from one build to the next.

| Part | Uncompressed | zstd -19 |
| --- | ---: | ---: |
| `libplayground.so` | 0.06 MB | 0.01 MB |
| `/tmp/clib` (.so) | 6.8 MB | 0.8 MB |
| boot libraries (.so) | 231.4 MB | 34.0 MB |
| &nbsp;&nbsp;of which `libHSghc` | 169.5 MB | 25.6 MB |
| boot libraries (.dyn_hi), without `ghc`'s | 21.7 MB | 5.3 MB |
| base layer (.so, 62 units) | 84.5 MB | 7.2 MB |
| base layer (.dyn_hi) | 18.8 MB | 4.1 MB |
| rest of the libdir (settings, package DB) | 1.3 MB | 0.2 MB |
| **`rootfs.tar.zst`** | **364.7 MB** | **50.0 MB** |
| the layers but `th` (six archives) | 18.5 MB | 2.2 MB |
| the `th` layer | 194.7 MB | 21.4 MB |
| material's assets (`material.js`, `material.css`) | 0.33 MB | |
| `bsdtar.wasm` | | 1.2 MB |
| `dyld.mjs` + `post-link.mjs` + `prelude.mjs`, `browser_wasi_shim.js`, page, manifest | | 0.16 MB |

At `zstd -3` the base archive is 64.3 MB and `th`'s 27.1 MB. The largest file
served is `rootfs.tar.zst`, 50.0 MB; the whole output directory is 75.3 MB.

### Time

| Step | Time |
| --- | ---: |
| fetch `rootfs.tar.zst` (localhost) | 0.2–0.5 s |
| extract (bsdtar.wasm into memory) | 2.3–2.4 s |
| dyld load (GHC and the boot libraries it links, until `playground` returned) | 4.4–4.7 s |
| run 1: the counter, the first program to use `haskell-halogen-core` (loads the base layer's 62 libraries, compiles, mounts) | 3.8–4.2 s |
| run 2: the same source again | 1.2–1.4 s |
| run 3: the source edited | 1.1–1.2 s |
| a layer but `th`: load, then its first run | 20–65 ms, then 0.6–3.5 s (table above) |
| the `th` layer: load, then its first run | 2.1–3.9 s, then 8.0–12.0 s |
| for comparison: `main = putStrLn "hello"` before anything else | 0.4 s |

So the page is ready about 7 s after the archive has arrived, and the counter
is on screen 4 s after the first Run; after that an edit runs in about 1.2 s.
The small layers cost nothing to load; their first run is mostly compiling
(the router's codec, `Duplex.sum` over a row, takes the longest). The `th`
layer takes 2–4 s to fetch and extract its 195 MB, and its first run another
8–12 s, which is loading its libraries, `ghc-lib-parser`'s 159 MB above
all, into dyld: run again it takes 0.6 s, and 1.1 s with the quote edited
(measured once, in a page that loaded only `th`). Over the network the download dominates:
50 MB is about 16 s at 25 Mbit/s, 4 s at 100 Mbit/s; `th` adds 7 s or 2 s.

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
| run 1 | 225 MB | 66 MB | 1.15 GB |
| run 8 (every layer but `th` loaded and run) | 229 MB | 122 MB | 1.22 GB |
| run 9 (`th`) | 321 MB | 165 MB | 1.72 GB |

The renderer's peak resident memory (VmHWM) was 1.29–1.42 GB until the `th`
run and 1.81 GB after it. `performance.memory` (which the page shows) is
rounded by Chrome and lags; the test reads the heap through the DevTools
protocol and the renderer's memory from `/proc`.

Where it goes, roughly: dyld gives every library one shared wasm memory
(GHC's heap, 33–165 MB, which only grows: 4–9 MB a run, 43 MB for `th`'s), the
JS heap holds dyld's symbol tables (a function and a `WebAssembly.Global` per
symbol) and the extracted files, and the rest is V8's: the wire bytes of the
loaded wasm (some 330 MB, and `ghc-lib-parser`'s 159 MB more), which it
keeps for lazy compilation, and the code compiled so far. V8's tiering flags
barely move it (`--no-wasm-tier-up` and `--liftoff-only` the same, eager
compilation 2 GB at load); lazy compilation is already the default.

### RTS flags

None are needed. The reference's `+RTS -c` (compacting collection of the old
generation) keeps the wasm memory smaller after a few runs (69 and 77 MB
after runs 2 and 3, against 82 and 95 MB without it), at no cost in time, so
the page keeps it. `-H64m` only raised the memory at load to 87 MB.

### wasm-opt

GHC's own build can run every `.so` through `wasm-opt` with `WASM_SO_OPT`
(`--debuginfo --low-memory-unused --strip-dwarf -Oz`); the script does this
for the layers' libraries when `WASM_SO_OPT` is set. Results (measured on the base
layer):

- It works after the fact on a dynamic-linking `.so`: dyld loads the result
  and the test passes.
- It takes the base layer's `.so` from 84.5 to 58.0 MB (core's from 6.9 to
  5.2 MB), but compressed they are the same size (core's 700 KB before and
  after, the archive 0.5 MB smaller at `-3`), and run 1 took the same time.
  Renderer peak memory was about 100 MB lower in one run. It adds two minutes
  to the build. Not worth it by default.
- On `libHSghc` (170 MB) wasm-opt ran out of this machine's 15 GB after
  four minutes. Only a GHC built with `WASM_SO_OPT` would have that one
  optimised; `ghc-lib-parser`'s (159 MB) would likely go the same way, and
  is not tried.

## Pages

- **Per file**: the largest file is `rootfs.tar.zst`, 50.0 MB, and `th`'s
  archive is 21.4 MB, both under the 100 MB GitHub allows a file. Should an
  archive grow past `LEARN_PART_MB` (95 MB), the build splits it into parts
  and the page joins them (above), so the limit cannot be hit unnoticed.
- **What is deployed**: the tutorial (`examples/learn`), whose build copies
  this directory, but for `index.html` and `index.js`, to `rootfs/` beside
  the app; this page is not deployed.
- **Site size**: 78 MB, the rootfs 75 MB of it (54 MB without the `th`
  layer), far from the 1 GB limit.
- **Bandwidth per cold load**: the page, `bsdtar.wasm`, the manifest and
  `rootfs.tar.zst` are 51.4 MB; a lesson with any of the small layers adds
  at most 2.8 MB (all eight and material's assets; `hspec`'s is the largest,
  1.3 MB; lesson 18 also fetches `liblazy-demo.so`, 64 KB), and the `th` lesson 21.4 MB more, 75.6 MB in all. Pages has a
  soft limit of 100 GB a month: about 1,850 cold loads a month without
  `th`, about 1,330 if every reader also loaded `th`. Its responses are cacheable (10 minutes, then revalidated
  with an ETag), and the tutorial keeps the archives in the Cache API under
  the manifest's `version`, so a returning reader downloads them again only
  when a deploy has changed them. Not counted:
  PixiJS (0.8 MB) and the icon font, which come from their CDNs.
- **Headers**: none needed (no COOP/COEP), and whatever type Pages serves
  `.zst` or a `.00` part with, the page reads it as bytes.

## What this proves, and what it does not

It proves:

- GHC 9.14's API runs in a page from a static server, and compiles and runs a
  module that uses `haskell-halogen-core`, whose libraries were built by this
  project's own wasm build and injected into the global package DB.
- Interpreted code calls into our compiled libraries, whose JSFFI bindings
  (`Halogen.JSBits.browserJS`) work as they do in a linked program: `runUI`
  from the bytecode mounts the counter, its click listeners call back into the
  RTS, and the counter counts. So do the asynchronous ones: `fetch` waits for
  its promise in the interpreted program.
- Run twice, the second tree replaces the first: the program's `main` calls
  `Halogen.IO.Util.endPreviousRun` and `keepForNextRun socket.dispose`, as the
  examples' interactive mode does, and since core is loaded once per session
  the reference it keeps survives from one run to the next.
- Every package of the project a lesson would use works from a layer loaded
  on demand, after runs without it: hooks, router, net, elements (a custom
  element whose class's callbacks call into the interpreted program's
  component), devtools (a panel listening to the interpreted program's
  trace), material (with its
  CSS and JavaScript on the page), pixi (PixiJS from its CDN), th, whose
  quasi-quoter runs ghc-lib-parser inside the page's GHC, store (a tree run
  through `runStoreT`, whose interpreted components share one store), worker
  (jobs run on the page, there being no binary for a Web Worker to start),
  lazy (a shared library the running program fetches and has dyld link into
  itself, whose `foreign export javascript` hands over the component it
  mounts), and hspec: an
  `hspec-halogen` suite, hspec's runner included, runs in the interpreter
  as it does in a linked test program, with no `getArgs`, terminal or
  configuration file in its way, and prints its report to stdout. Its
  clicks and keystrokes are synthetic events rather than Playwright's
  (`hspec-halogen/README.md`, "Without the runner").
- Template Haskell splices run in the page (`$(litE ...)`, and `hh`).
- The boot libraries can be trimmed by 46 MB (11 MB compressed) without
  breaking anything a program here does.

It does not show:

- A program cannot declare a `foreign import javascript` of its own: GHC
  compiles such a module to an object file, which needs clang
  (`createPipe: unsupported operation`). A program uses JavaScript through
  library bindings only; a tutorial chapter about FFI would need its bindings
  in a layer.
- Anything but Chromium and WebKit on a fast desktop. Firefox is untested
  here (GHC's own playground is tested on all three, and dyld works around a
  WebKit `table.grow` bug). Playwright's WebKit (Safari 26's engine) loads the
  compiler in about 20 s on the deployed tutorial and compiles and runs the
  testing lesson's suite, with a desktop window and with an iPhone's; so does
  Chromium with a phone's screen and touch (a Pixel 7's), once the reader has
  asked for the compiler, which a small screen does not load by itself. What
  a real phone does is still untested: the emulation gives it the desktop's
  memory and speed.
- The editor, error markers, persistence, or a lesson naming its layers:
  those are the tutorial's ([`examples/learn`](../examples/learn/README.md)),
  on this rootfs; this page only has `?layers=` and `learn.loadLayer`.
- `sound` and `tauri` have no layer: a lesson has no sound backend to drive
  in a test, nor a Tauri to talk to. Each would be a line in
  `DEFAULT_LAYERS`.

## Risks for the tutorial

- **Memory**: 1.2–1.4 GB of renderer memory, 1.8 GB once `th` has run.
  Desktop browsers are fine; many phones kill a tab well before that, and
  iOS Safari's limits are tighter still. A mobile reader may need a
  "read-only" fallback (prerendered results) rather than a live compiler, and
  the `th` lesson is the one most likely to fail on a small machine.
- **Download**: 50 MB before the first compile (21 MB more for `th`), and
  Pages' bandwidth limit above. The tutorial keeps the archives in the Cache
  API, so a returning reader downloads them again only after a deploy that
  changed them.
- **Slow machines**: the numbers above are a fast VM. Extracting and loading
  are single-threaded JavaScript and wasm instantiation; on a slow laptop or a
  phone expect several times as long. A progress display matters (the
  manifest's sizes are for that; the page shows bytes received against them).
- **Safari**: WebKit loads the compiler and compiles on a desktop (above);
  an iPhone's memory limit, tighter than the desktop's, is untested, and
  WebKit is the browser most likely to fall short on it.
- **Keeping it in step**: the rootfs must be built with exactly the GHC that
  `cabal-wasm.project` pins and from the same build of our packages (the
  layers' `.so` and interfaces are ABI-bound to both, and to the base layer:
  a layer must be rebuilt with the base layer it stands on). The script takes
  all of them from the project's wasm build in one go, so a GHC bump or a
  package change needs only a rebuild, but the tutorial's text must be checked
  against the packages it was built with. GHC's `dyld.mjs` imports
  browser_wasi_shim from `https://esm.sh/gh/haskell-wasm/browser_wasi_shim`,
  unpinned, and the page must share dyld's copy (the WASI instance dyld makes
  checks the file system the page hands it against its own copy's classes).
  So the script ships `vendor/browser_wasi_shim/browser_wasi_shim.js` beside
  `dyld.mjs` and rewrites that import to it, and the pages import the same
  file: an outage of esm.sh does not take the compiler down, and the shim
  changes only when that file does. The script stops if dyld no longer
  imports it as it expects, since a dyld left with a copy of its own would
  not recognise the page's file system. The file's header says which commit it was built
  from and how to build a later one; it is one of the inputs `--key` hashes
  (all of `learn/`), so a new copy rebuilds the rootfs in CI. It is not in the
  manifest's `version`, which only keys the archives the tutorial caches: like
  `dyld.mjs`, the shim is fetched as an ordinary module on every visit.
- **External dependencies**: the pixi layer needs jsDelivr at run time and
  material's icons need Google Fonts; a reader behind a filter that blocks
  them gets a canvas that never draws, or icons as words. The manifest lists
  both, so a page can say so.
- **dyld patch**: it is ours until GHC takes it; the script checks the code it
  replaces and leaves dyld alone if it changed.

## Provenance

- `bsdtar.wasm`: [haskell-wasm/bsdtar-wasm](https://github.com/haskell-wasm/bsdtar-wasm),
  libarchive's bsdtar built for wasm32-wasi with zstd, BSD-2-Clause
  (Copyright (c) 2025 Cheng Shao); downloaded once from
  `https://haskell-wasm.github.io/bsdtar-wasm/bsdtar.wasm`, the build GHC's
  playground loads, and pinned by its SHA-256 in the script.
- `browser_wasi_shim.js`: [haskell-wasm/browser_wasi_shim](https://github.com/haskell-wasm/browser_wasi_shim),
  MIT OR Apache-2.0 (`vendor/browser_wasi_shim/LICENSE-*`), bundled from its
  source at commit `2f86b49` (the one esm.sh served) with esbuild, as its
  header says. That fork rather than `@bjorn3/browser_wasi_shim` from npm:
  the fork is upstream's unreleased main with GHC's changes, the one GHC's
  playground is tested on, and the last release, 0.4.2, copies a whole file
  on every write that grows it, which extracting the rootfs' large libraries
  would pay for quadratically.
- `dyld.mjs`, `post-link.mjs`, `prelude.mjs`: GHC's, from the toolchain's
  libdir (BSD-3-Clause), `dyld.mjs` patched as above.
- `layers/material/material.js`, `material.css`: our `material/jsbits` and
  `examples/material/style.scss`, bundled with the
  [Material Components for the web](https://github.com/material-components/material-components-web)
  packages they import (MIT), as the material example is.
- PixiJS ([pixijs/pixijs](https://github.com/pixijs/pixijs), MIT) and the
  Material Icons font (Apache-2.0) are not served from here: their CDNs serve
  them.
