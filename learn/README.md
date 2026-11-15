# learn/: GHC in the page

This directory demonstrates GHC running entirely in a static web page. GHC is compiled to WebAssembly, compiles a Halogen component supplied by the reader, and mounts it in the same page. No server-side compiler is involved. The implementation follows GHC's `ghc-api-browser/playground001` test and [try-miso](https://try.haskell-miso.org), which uses the same approach for miso.

What is here:

- `Playground.hs` builds into `libplayground.so` and exposes the compiler session. It writes source to `/tmp/Main.hs`, parses GHC flags, loads the module with the bytecode backend and runs `Main.main` in-process. Stdout and stderr are line-buffered and flushed after each run.
- `web/index.html` and `web/index.js` provide a minimal editor, argument field, layer buttons, output panes and timings. They use plain modules without a bundler. `web/compiler.js` starts GHC; `web/worker.js` reuses it to compile the same program in a worker configured through `halogenWorker.script`.
- `../toolchain/build-learn-rootfs.sh`, invoked by `npm run build-rootfs`, builds the files into `dist-newstyle/wasm/learn/`. `learn-inject.py` registers package layers, and `learn-rootfs-key.py` computes the CI cache key.
- `../examples/learn/playground-test/` is the native Chromium suite, run with `npm run test-playground` through [`haskell-halogen-playwright`](../playwright/README.md). It compiles programs for each layer and reports timings and memory. Configure it with `LEARN_ROOTFS_DIR`, comma-separated `LEARN_ONLY`, `LEARN_TIMEOUT` in seconds (default 300), and `LEARN_QUERY`, for example `rts=-c -H64m`.

`--learn-check` selects type checking with `-fno-code` and skips execution. Successful compiles and checks report top-level `Lesson` binding types as tab-separated `learn-type`, line, column, name and type fields. A typechecker plugin records these in the module's own scope, like GHCi's `:type`; cached results are reused only for the same source when GHC skips rechecking. This includes bindings omitted from a module's exported interface.

`--learn-spans` adds source positions to supported HTML and SVG element expressions. A parser plugin wraps fully applied constructors with `Halogen.HTML.Core.withProps`, adds `data-learn-src="<line>:<column>"` before user properties, and inserts the required import. It handles qualified and unqualified names and `$` applications. GHC keeps diagnostics at the original source positions; the tutorial strips generated wrappers from quoted diagnostic text.

For partial applications with a literal property list, the plugin inserts the mark into that list. Other partial applications and quasi-quote output are unmarked. The tutorial enables this flag for compiles and checks to support Source mode; see `examples/learn/README.md`.

The tutorial in [`examples/learn`](../examples/learn/README.md) uses this same rootfs. Its build copies the compiler files into `rootfs/` beside the app for deployment to GitHub Pages. The minimal page here remains a local tool for testing the compiler on its own.

## Build and run

```sh
npm run build-rootfs                # dist-newstyle/wasm/learn/, zstd -12 (under a minute on 4 cores, our libraries built)
ZSTD_CLEVEL=3 npm run build-rootfs  # the same in half a minute, 12 MB larger
ZSTD_CLEVEL=19 npm run build-rootfs # in two and a half minutes, 6 MB smaller
LEARN_REPORT=1 npm run build-rootfs # also each part's compressed size (ten seconds more)
npm run test-playground             # headless Chromium, prints timings and memory
LEARN_ONLY=th npm run test-playground  # the base runs and only the layers named
python3 -m http.server -d dist-newstyle/wasm/learn 8000   # or any static server
sh toolchain/build-learn-rootfs.sh --key   # the hash CI caches the output under (below)
```

The build first compiles `haskell-halogen-jsbits`, `haskell-halogen-core` and layer packages with the wasm toolchain, reusing existing outputs when possible. Additional arguments go to Cabal, for example `npm run build-rootfs -- -j2`. Requirements are the pinned wasm toolchain on PATH or in `~/.ghc-wasm/env`, Python 3, zstd, GNU tar and split, curl for the initial `bsdtar.wasm` download, and npm dependencies installed with `npm ci`. Material assets use esbuild and sass. Output goes under the ignored `dist-newstyle/` directory.

Any static server can host the page. It needs no COOP/COEP headers or `SharedArrayBuffer`; tests verify that `crossOriginIsolated` is false. The compiler uses the bundled `vendor/` copy of browser_wasi_shim. Only optional layer dependencies, such as PixiJS, Three.js and the icon font, require external requests.

Archive packing is reproducible. GNU tar sorts entries, normalises timestamps to the epoch and removes ownership information. Repacking identical libraries at the same zstd level produces identical bytes and the same manifest `version`, regardless of compression thread count.

Recompiling can change the archives because GHC output is not deterministic by default. In one rebuild of core, every archive except `th` changed. CI therefore caches the complete output using `build-learn-rootfs.sh --key` rather than rebuilding unnecessarily.

The key hashes the compiler ID and ABI, the packed packages' dependency closure, local package flags and files, wasi-sysroot libraries, build scripts, `learn/`, Material assets, `package-lock.json` and settings. Installed units are identified by their input-derived IDs. A cache hit restores the directory and skips the build.

Use `?rts=` to experiment with RTS options; the default is `-c`. For example, open `?rts=-c+-H64m` or set `LEARN_QUERY='rts=-c -H64m'` for tests. `?layers=router,net` or `?layers=all` loads layers immediately after compiler startup. Otherwise, layers load when their buttons are pressed.

## The rootfs

`rootfs.tar.zst` extracts to `/tmp` in the page's in-memory file system (browser_wasi_shim's), which GHC sees as its file system:

| Path | What |
| --- | --- |
| `/tmp/libplayground.so` | `Playground.hs`, built `-package ghc -shared -dynamic -O2` |
| `/tmp/clib/` | the wasi-sysroot's `.so` (libc, libc++, ...), without `libsetjmp` and `libwasi-emulated-*` |
| `/tmp/hslib/lib/` | GHC's libdir: `settings`, `package.conf.d`, and in `wasm32-wasi-ghc-<version>-<abi>/` every `libHS*.so`, each unit's `.dyn_hi` tree in a directory of its own |
| `/tmp/hslib/layers/<name>/` | a further layer, from its own archive: `package.conf.d/`, `lib/*.so`, a `.dyn_hi` tree per unit |

The build removes files that a dynamic, non-profiled compiler does not use: static and profiling interfaces and libraries, the debug RTS, documentation and libdir JavaScript/text assets. Boot package configurations no longer reference documentation. Additional trimming removes unused packages as described below.

The base layer contains `haskell-halogen-jsbits`, `haskell-halogen-core` and their dependencies not shipped with GHC: 62 units including aeson, vector, clay, protolude, unliftio and row-types. The build discovers them from package configuration dependencies, using the project and Cabal store databases; it does not maintain a manual dependency list.

Shared libraries go in dyld's boot-library search directory. Each unit gets its own `.dyn_hi` directory, and its configuration is added to the global database with `${pkgroot}`-relative paths. Documentation, data and include paths are removed. Unlike `${topdir}`, `${pkgroot}` also works with `ghc-pkg check`, so the build can recache and validate the result. Programs need only `-package haskell-halogen-core`.

The page also gets `rootfs.json` (the manifest, below), `dyld.mjs`, `post-link.mjs` and `prelude.mjs` from the libdir, `browser_wasi_shim.js` from `vendor/browser_wasi_shim/`, and `bsdtar.wasm`.

### Trimming the boot libraries

The toolchain ships 46 boot packages. `BOOT_TRIM` removes packages unused by the compiler or any supported layer: `Cabal`, `Cabal-syntax`, `haddock-api`, `haddock-library`, `xhtml`, `haskeline`, `ghc-toolchain`, `ghc-compact`, `integer-gmp` and `ghc-bignum`. The last two are compatibility shims over `ghc-internal`.

The retained compiler closure includes `ghc`, `ghci`, `ghc-boot`, `template-haskell`, `hpc` and `ghc-heap`. `ghc-platform` is required by `ghc-boot`; the base layer also needs `parsec` and `text`.

`ghc-pkg unregister` and `ghc-pkg check` protect boot-package dependencies. Because layers are injected afterward, `learn-inject.py --trimmed` separately checks their dependencies and fails with the affected unit and `BOOT_TRIM` entry if a layer needs a removed package. The trim list must be reviewed when dependencies change.

The `ghc` package is hidden, and its 32.2 MB of interfaces live in the `th` layer rather than the base archive. Ordinary lessons do not import the GHC API. Quasi-quoters need some of those interfaces, so `ghc`'s `import-dirs` points to `/tmp/hslib/layers/th/ghc`.

Its base configuration keeps the library directory but lists no modules. This passes `ghc-pkg check` and makes direct imports fail with "Could not find module" instead of a missing-interface error. `libHSghc` itself remains in the base archive because `libplayground.so` and the quoter use it.

Set `LEARN_GHC_API=1` to expose GHC and include its interfaces in the base archive, adding about 9 MB compressed. A browser test confirmed that a GHC API program runs with this option and fails as expected without it.

What it saved, at `zstd -12` (the parts each compressed alone, as `LEARN_REPORT=1` does; the archive before is one built with only `Cabal` and `Cabal-syntax` trimmed and `LEARN_GHC_API=1`):

| | Uncompressed | zstd -12 |
| --- | ---: | ---: |
| the eight packages beyond Cabal (`.so` and `.dyn_hi`) | 14.1 MB | 2.7 MB |
| `ghc`'s interfaces | 32.2 MB | 9.1 MB |
| **`rootfs.tar.zst`** | 411.8 → **365.4 MB** | 68.1 → **56.3 MB** |

Every test run passes on the trimmed rootfs, Template Haskell and the `th` layer's quasi-quoter included.

### Layers

Additional tutorial packages are delivered as separate layers, downloaded only when needed. Each archive contains its packages and dependencies beyond the base layer. It has its own database at `/tmp/hslib/layers/<name>/package.conf.d`, with a prebuilt `package.cache` and relocatable `${pkgroot}` paths.

Layer libraries stay in their own `lib/` directory. GHC finds them through `dynamic-library-dirs`, and dependencies resolve through the base search paths. After loading a layer, the page adds `-package-db <database>` and `-package <package>` arguments for subsequent compiles.

Each layer depends only on the base layer, never on another optional layer. This allows arbitrary combinations and load order. Dependencies shared by optional layers must move into the base layer. `learn-inject.py --taken` checks other layer databases and rejects duplicate units, which would otherwise become separate libraries in dyld.

Most layers contain only their own package. `lazy` also includes its demo interface package; `material` adds `clock` and `extra`; `th` adds parsers and GHC interfaces; and `hspec` adds the hspec dependency closure. QuickCheck is already in the base layer. None requires a trimmed boot package.

The default layers are listed in `DEFAULT_LAYERS`. Override them with `LEARN_LAYERS='name=pkg,pkg ...'`, or set `LEARN_LAYERS=` for none. The table records archive sizes, extraction and asset-loading time, and the first program run, which also links the layer and compiles the program. Timing provenance and differences between builds are documented below the table.

| Layer | Packages | Units | Uncompressed | zstd -12 | Assets on the page | Load | First run |
| --- | --- | ---: | ---: | ---: | --- | ---: | ---: |
| `hooks` | `haskell-halogen-hooks` | 1 | 0.82 MB | 0.14 MB | none | 28–33 ms | 1.2–1.3 s |
| `router` | `haskell-halogen-router` | 1 | 0.88 MB | 0.15 MB | none | 22–23 ms | 2.8–3.5 s |
| `net` | `haskell-halogen-net` | 1 | 0.68 MB | 0.12 MB | none | 21–29 ms | 0.6–0.8 s |
| `elements` | `haskell-halogen-custom-element` | 1 | 0.43 MB | 0.09 MB | none | 22–25 ms | 0.5–0.7 s |
| `devtools` | `haskell-halogen-devtools` | 1 | 0.77 MB | 0.13 MB | none | 23–24 ms | 1.2–1.5 s |
| `material` | `haskell-halogen-material` (+ `clock`, `extra`) | 3 | 2.62 MB | 0.40 MB | `material.js` 127 KB, `material.css` 206 KB; the Material Icons font from fonts.gstatic.com | 56–65 ms | 0.7–1.3 s |
| `pixi` | `haskell-halogen-pixi` | 1 | 0.97 MB | 0.17 MB | none; PixiJS 8.20.1 from jsDelivr (0.8 MB), which the library imports itself | 27–31 ms | 1.4–1.6 s |
| `three` | `haskell-halogen-three` | 1 | 1.17 MB | 0.20 MB | none; Three.js 0.186.1 from jsDelivr (0.75 MB, 0.19 MB with brotli), which the library imports itself | 26 ms | 2.0–2.1 s |
| `th` | `haskell-halogen-th` (+ `megaparsec`, `parser-combinators`, `tagsoup`, `css-syntax`, `case-insensitive`, and `ghc`'s interfaces) | 6 | 46.0 MB | 11.0 MB | none | 0.74–0.99 s | 3.1–4.2 s |
| `hspec` | `hspec-halogen` (+ `hspec`, `hspec-core`, `hspec-expectations`, `hspec-discover`, `HUnit`, `quickcheck-io`, `ansi-terminal`, `ansi-terminal-types`, `colour`, `call-stack`, `haskell-lexer`) | 12 | 12.7 MB | 1.48 MB | none | 98–100 ms | 3.6–4.1 s |
| `store` | `haskell-halogen-store` | 1 | 0.52 MB | 0.09 MB | none | 23–25 ms | 0.9–1.7 s |
| `worker` | `haskell-halogen-worker` | 1 | 0.20 MB | 0.04 MB | none | 20–21 ms | 7.4–9.5 s |
| `lazy` | `haskell-halogen-lazy`, `haskell-halogen-lazy-demo` | 2 | 0.28 MB | 0.06 MB | none; `liblazy-demo.so` (64 KB), which its program fetches itself (below) | 19–27 ms | 6.2–7.2 s |
| `live` | `haskell-halogen-remote` | 1 | 1.25 MB | 0.21 MB | none | 24–31 ms | 9.4–10.2 s |
| **all fourteen** | | 33 | 70.0 MB | 14.4 MB | 0.33 MB | 1.2 s | |
| all but `th` | | 27 | 23.2 MB | 3.29 MB | 0.33 MB | 0.41–0.47 s | |

The table combines measurements from several builds, so its timings are not a controlled comparison between layers. Early runs used `zstd -19`; later sizes use `-12`. The hspec measurements were taken while other builds shared the machine, with `th` loading in 1.6–1.8 seconds. Later elements measurements used a 21.9 MB uncompressed `th` archive and a 2.61 MB Material archive; `th` loaded in 1.5–1.8 seconds. Store and devtools each came from three later runs, with `th` loading in 1.6–2.0 seconds. Their totals combined those runs with the other layer's sizes.

The worker row came from three runs with eleven layers. In that build, hooks was 0.82 MB, Material 2.62 MB and the `th` archive 21.9 MB. Runs after `th` were slower: store took 6.1–7.0 seconds and hspec 6.4–7.5 seconds. Worker's 7.4–9.5 seconds therefore reflects its position at the end of the run as well as its own cost.

The lazy row came from three twelve-layer runs. It took 6.2–7.2 seconds at the end of the sequence but only 1.1 seconds with `LEARN_ONLY=lazy`. In those runs, store took 3.6–6.2 seconds, hspec 6.4–7.2 seconds and `th` loaded in 1.6–1.9 seconds. Lazy's run includes compiling the program, loading both layer libraries, fetching `liblazy-demo.so` and linking it.

The Three.js row came from three thirteen-layer runs. Its 2.0–2.1 seconds includes fetching Three.js from jsDelivr and waiting for a raycast click to reach the box; that CDN download is not part of the layer size. `th` loaded in 1.8–2.0 seconds. Later runs were again slower: store took 6.7–8.3 seconds, worker 7.1–9.6 seconds and lazy 6.2–7.7 seconds.

The live row came from three fourteen-layer runs and took 9.4–10.2 seconds last in the sequence, compared with 1.4 seconds under `LEARN_ONLY=live`. Store took 6.2–6.5 seconds, lazy 7.3–7.7 seconds and `th` loaded in 1.8–2.1 seconds. Live's run includes compiling, linking and clicking a counter built by the remote client.

Those lazy, Three.js and live builds still had the 21.9 MB `th` archive. The current `th` row and first total instead use GHC's existing parser at `zstd -12`. Its timings come from three isolated runs and one full run, which also supplied the total load time. Later layers were fast again: store 0.8 seconds, lazy 0.8 seconds and live 2.3 seconds. The earlier slowdown came from `ghc-lib-parser`, which added about 159 MB of shared libraries. That version of the layer had 7 units, occupied 194.7 MB or 23.4 MB compressed, loaded in 2.1–3.9 seconds and took 8.0–12.0 seconds for its first run.

The hspec archive is small enough to include the full `hspec` package. Of its 10.9 MB of shared libraries, `hspec-core` accounts for 4.1 MB, `haskell-lexer` 3.0 MB and `colour` 1.4 MB. `hspec` itself adds only 0.1 MB.

`th` uses the already loaded `libHSghc` parser. Its layer contains 11.6 MB of shared libraries, 2.2 MB of their interfaces and GHC's 32.2 MB of interfaces. Previously it bundled `ghc-lib-parser`, adding 158.8 MB of libraries and 22.0 MB of interfaces; that layer totalled 194.7 MB, or 23.4 MB compressed. It remains optional because ordinary lessons do not need the GHC interfaces.

The playground suite compiles and exercises a program for every layer so both its shared libraries and JavaScript bindings run. Coverage includes a hooks counter, hash routing, fetch, custom-element input updates, devtools counters, Material styling and ripple, Pixi pointer input, Three.js raycasting, an `hh` quote, an hspec suite, store subscriptions, worker jobs, dynamic loading and remote loopback.

Worker checks verify availability, worker identity, `run`, concurrent `start`, `runPar` and exceptions returned as `WorkerError`. The page supplies `worker.js` to start a second compiler. Lazy-loading checks observe the placeholder, fetch and link `liblazy-demo.so`, then exercise child clicks, queries and outputs. The hspec program reports `2 examples, 0 failures` and cleans up its result container.

Successful runs produce no console errors. Pixi and Three.js checks are skipped with an explanation if their CDN cannot be reached; it was reachable through the proxy in the recorded runs.

Within a single-line `hh` quote, surrounding whitespace is text. `[hh| <p>hello</p> |]` therefore contains three nodes and is rejected by the single-element quoter. Write `[hh|<p>hello</p>|]` or put the element on separate lines.

**Assets.** Material requires separate JavaScript and CSS because its jsbits imports npm packages. The build runs `examples/material/bundle.sh` into `layers/material/`, producing `material.js` as an IIFE and `material.css`. The page loads both before running layer code. Icons fetch the font from fonts.gstatic.com only when needed.

Pixi and Three.js load their modules from jsDelivr when a canvas or scene mounts. Their default configurations specify the URLs, and the manifest lists these and the icon font under `external`.

The lazy layer serves `liblazy-demo.so` beside its archive at `layers/lazy/liblazy-demo.so`. The demo fetches it during execution to demonstrate linking new code. It is built from `lazy/demo/feature` against the same base and layer libraries and listed under `libraries`. Both playground pages expose `globalThis.halogenDyld = { dyld, rootfs, shim }` for this.

Other packages embed their JavaScript in their shared libraries and need no separate assets.

Separate package databases solve two problems:

- **GHC caches package databases.** The first `setSessionDynFlags` caches unit databases, so a later `-package-db` is otherwise ignored. `Playground.hs` clears that cache before setting each run's flags. Layers loaded after earlier runs then become visible, with no measurable reread cost.
- **GHC reads `package.cache`, not loose `.conf` files.** Adding a configuration to the global database would require `ghc-pkg recache`, but the browser has no `ghc-pkg`. Shipping replacement global caches would also impose a fixed layer order or require one cache per combination. Separate prebuilt databases avoid both problems.

**Archive parts.** Archives exceeding `LEARN_PART_MB` are split with `split -b` into `.00`, `.01` and subsequent parts. The default is 95 million bytes, below GitHub's 100 MB file limit. The manifest lists parts in order; the page downloads and joins them before extraction. Current archives fit in one part. A test with `LEARN_PART_MB=20` passed with a four-part base archive and a two-part `th` archive.

### The manifest: `rootfs.json`

`rootfs.json` supplies compiler versions, paths and sizes so pages do not hardcode them. Relative paths refer to files beside the manifest; absolute paths refer to the in-memory file system. Sizes are bytes. Both the tutorial's `examples/learn/js/playground.js` and the standalone `web/index.js` consume this format:

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

To load a layer, download its parts in order, concatenate them and extract with `bsdtar -x` into the existing root file system. Load its assets, then add its package database and package flags to future runs. `learn/web/index.js` implements this in `loadLayer`. Loading and compilation are serialised.

## Measurements

Measured here: a 4-core cloud VM, headless Chromium (Playwright's 1243 build), the page served from localhost by the playground's test, dyld patched as below. Times are three runs of `npm run test-playground` on a clean build, made when the archives were packed at `zstd -19`; sizes are of a build at `zstd -12`, the default since.

### Sizes

Compressed sizes are each part compressed alone at `zstd -12`, as `LEARN_REPORT=1` measures them, of a local build; they add up to 56.1 MB, and the archive is 56.3 MB. (At `zstd -19` the parts of the deployed rootfs added up to 51.6 MB and its archive was 50.0 MB, and a local build packed the same uncompressed sizes into 52.0 MB: the archive's size moves by a few percent from one build to the next.)

| Part | Uncompressed | zstd -12 |
| --- | ---: | ---: |
| `libplayground.so` | 0.41 MB | 0.06 MB |
| `/tmp/clib` (.so) | 6.8 MB | 0.9 MB |
| boot libraries (.so) | 231.4 MB | 36.6 MB |
| &nbsp;&nbsp;of which `libHSghc` | 169.5 MB | 27.2 MB |
| boot libraries (.dyn_hi), without `ghc`'s | 21.7 MB | 5.8 MB |
| base layer (.so, 62 units) | 84.8 MB | 8.1 MB |
| base layer (.dyn_hi) | 18.9 MB | 4.4 MB |
| rest of the libdir (settings, package DB) | 1.3 MB | 0.2 MB |
| **`rootfs.tar.zst`** | **365.4 MB** | **56.3 MB** |
| the layers but `th` (thirteen archives) | 23.2 MB | 3.3 MB |
| the `th` layer | 46.0 MB | 11.0 MB |
| material's assets (`material.js`, `material.css`) | 0.33 MB | |
| `bsdtar.wasm` | | 1.2 MB |
| `dyld.mjs` + `post-link.mjs` + `prelude.mjs`, `browser_wasi_shim.js`, page, manifest | | 0.16 MB |

Packing all the archives takes 8 s on this machine at `zstd -12`, 129 s at `-19` and 1.5 s at `-3`. At `zstd -19` the base archive is 52.0 MB and `th`'s 21.9 MB, 6.1 MB less in all; at `-3` 64.4 MB and 27.1 MB, 12.2 MB more. CI and the deploy use 12, a sixteenth of the time 19 takes for 8% more bytes. The largest file served is `rootfs.tar.zst`, 56.3 MB; the whole output directory is 84.8 MB.

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
| the `th` layer: load, then its first run | 0.85–0.99 s, then 3.1–4.2 s |
| for comparison: `main = putStrLn "hello"` before anything else | 0.4 s |
| a program's Web Worker (`haskell-halogen-worker`), this page: from the run's end to the answers of its first jobs, the worker started | 9.0–11.2 s |
| the same, every layer loaded first (the worker extracts them all, `th`'s too) | 11.0 s |
| the same in the tutorial, lesson 17's first count after a Compile, the worker started | 11.5–11.9 s |
| lesson 17's second count, the worker already up | 1.0–1.1 s |

In these measurements, the compiler is ready about seven seconds after the archive arrives. The first counter run takes another four seconds; subsequent edits run in about 1.2 seconds. Small layers load quickly, with compilation dominating their first run. The router's row-based codec is one of the slower examples.

The `th` layer takes about one second to extract and 3–4 seconds for its first run, compared with 8–12 seconds when it bundled `ghc-lib-parser`. Network transfer can dominate startup: 56 MB takes roughly 18 seconds at 25 Mbit/s or 4.5 seconds at 100 Mbit/s. The `th` download adds about 3.5 or 0.9 seconds respectively.

A program's worker runs another compiler through `web/worker.js` or the tutorial's `lesson-worker.js`. It fetches the archives again, using the tutorial's Cache API when available, extracts them, starts dyld and recompiles the program before handling its first job. Later jobs reuse that worker.

The timing rows include three `LEARN_ONLY=worker` playground runs and three tutorial lesson-17 runs. The second count takes about one second of computation while the page draws roughly 55 frames per second. A full-layer run extracts every loaded layer in the worker too. This took 23.8 seconds with the old 195 MB `th` layer. During `test-learn`, concurrent compilation on the second page increased lesson 17's first count to 20.5 seconds.

**dyld patch.** The shipped linker builds each library's imports by copying all prior exports with `{ ...this.exportFuncs }`. After GHC loads, this copies roughly a hundred thousand entries for each of the base layer's 62 libraries. Profiling attributed 8.5 seconds of an 11-second first run to this operation.

The build patches its copy to look up only the library's actual imports in dyld's existing import loop. It reports the patch and leaves unfamiliar future versions unchanged. Without the patch, linker startup took 4.7–5.1 seconds and the first run 11.0–11.9 seconds; subsequent runs were unaffected. The upstream location is GHC's `utils/jsffi/dyld.mjs`.

### Memory

| After | JS heap (DevTools, after a GC) | wasm memory | renderer RSS |
| --- | ---: | ---: | ---: |
| load | 168 MB | 33 MB | 1.12 GB |
| run 1 | 225 MB | 66 MB | 1.15 GB |
| run 8 (every layer but `th` loaded and run) | 229 MB | 122 MB | 1.22 GB |
| `th`'s run (every layer before it in the test loaded and run) | 242 MB | 149 MB | 1.35 GB |
| a program's worker up (the `worker` layer alone loaded) | | | 1.83 GB |
| a program's worker up, every layer loaded first | | | 2.19 GB |
| lesson 17's worker up, in the tutorial | | | 1.77 GB |

Across the full test run, renderer peak resident memory (`VmHWM`) reached 1.29–1.42 GB before `th` and 1.42 GB afterward. With the older `ghc-lib-parser`, the `th` row was 321 MB of JavaScript heap, 165 MB of wasm memory and 1.72 GB RSS, with a 1.81 GB peak. A worker loading every layer then reached 2.70 GB.

Workers run in the page's renderer process but have their own compiler, file system and dyld. They added about 0.7 GB in isolated playground runs, 1.0 GB with all layers, and 0.5 GB in the tutorial. Corresponding pre-worker RSS values were 1.13 GB in the playground and 1.28 GB in the tutorial. Startup peaks reached 1.95–2.4 GB, or 3.0 GB with every layer. The next run terminates the pool and releases the worker.

Chrome's `performance.memory` display is rounded and delayed. The test instead reads the JavaScript heap through DevTools and process memory from `/proc`.

Memory is split among the shared wasm heap, the JavaScript heap and V8's compiled-module storage. The wasm heap only grows: recorded usage was 33–165 MB, increasing 4–9 MB per run and 43 MB for `th`. JavaScript holds extracted files and dyld symbol tables, with a function and `WebAssembly.Global` per symbol. V8 retains roughly 330 MB of wasm bytes for lazy compilation plus generated code. Disabling tier-up or using Liftoff alone made little difference; eager compilation raised startup memory to 2 GB.

### RTS flags

No RTS flags are required. The page defaults to `-c`, which compacts the old generation and reduced wasm memory after runs 2 and 3 to 69 and 77 MB, compared with 82 and 95 MB without it, with no measured timing cost. Adding `-H64m` only increased startup memory to 87 MB.

### wasm-opt

GHC's own build can run every `.so` through `wasm-opt` with `WASM_SO_OPT` (`--debuginfo --low-memory-unused --strip-dwarf -Oz`); the script does this for the layers' libraries when `WASM_SO_OPT` is set. Results (measured on the base layer):

- It works after the fact on a dynamic-linking `.so`: dyld loads the result and the test passes.
- It takes the base layer's `.so` from 84.5 to 58.0 MB (core's from 6.9 to 5.2 MB), but compressed they are the same size (core's 700 KB before and after, the archive 0.5 MB smaller at `-3`), and run 1 took the same time. Renderer peak memory was about 100 MB lower in one run. It adds two minutes to the build. Not worth it by default.
- On `libHSghc` (170 MB) wasm-opt ran out of this machine's 15 GB after four minutes. Only a GHC built with `WASM_SO_OPT` would have that one optimised.

## Pages

- **Per file**: the largest file is `rootfs.tar.zst`, 56.3 MB, and `th`'s archive is 11.0 MB, both under the 100 MB GitHub allows a file. Should an archive grow past `LEARN_PART_MB` (95 MB), the build splits it into parts and the page joins them (above), so the limit cannot be hit unnoticed.
- **What is deployed**: the tutorial (`examples/learn`), whose build copies this directory, but for the page's own files (`index.html`, `index.js`, `compiler.js`, `worker.js`), to `rootfs/` beside the app; this page is not deployed.
- **Site size**: 75 MB, the rootfs 72 MB of it (61 MB without the `th` layer), far from the 1 GB limit.
- **Bandwidth per cold load**: the page, `bsdtar.wasm`, the manifest and `rootfs.tar.zst` are 57.7 MB; a lesson with any of the small layers adds at most 3.6 MB (all thirteen and material's assets; `hspec`'s is the largest, 1.5 MB; lesson 18 also fetches `liblazy-demo.so`, 64 KB), and the `th` lesson 11.0 MB more, 72.3 MB in all. Pages has a soft limit of 100 GB a month: about 1,630 cold loads a month without `th`, about 1,380 if every reader also loaded `th`. Its responses are cacheable (10 minutes, then revalidated with an ETag), and the tutorial keeps the archives in the Cache API under the manifest's `version`, so a returning reader downloads them again only when a deploy has changed them. Not counted: PixiJS (0.8 MB), Three.js (0.75 MB) and the icon font, which come from their CDNs.
- **Headers**: none needed (no COOP/COEP), and whatever type Pages serves `.zst` or a `.00` part with, the page reads it as bytes.

## What this proves, and what it does not

It proves:

- GHC 9.14 can compile and run Halogen modules in a page served by a static server, using libraries from this repository's wasm build.
- Interpreted code can call compiled libraries and their JSFFI bindings. Tests cover rendering, event callbacks and asynchronous fetch operations.
- Repeated runs can dispose the previous tree with `endPreviousRun` and register cleanup with `keepForNextRun`. Core's session-wide cleanup reference survives interpreter runs.
- Optional layers can load after earlier runs and work without restarting GHC. Coverage includes hooks, routing, networking, custom elements, devtools, Material, Pixi, Three.js, templates, stores, workers, lazy loading, remote loopback and hspec. Workers use a second compiler through a custom loader. The in-page hspec harness uses synthetic input and reports to stdout; see `hspec-halogen/README.md`, "Without the runner".
- Template Haskell, including `$(litE ...)` and `hh`, runs in the page.
- Removing unused boot libraries saves 46 MB, or 12 MB compressed, without breaking the tested programs.

It does not show:

- Programs cannot define new `foreign import javascript` bindings in the interpreter. GHC would need clang to produce object files and fails with `createPipe: unsupported operation`. Supply bindings through compiled layers instead.
- Browser testing here covers Chromium and WebKit on desktop hardware, including mobile viewport and touch emulation. Firefox is untested here, although GHC's playground tests all three browsers. WebKit based on Safari 26 loaded the deployed compiler in about 20 seconds and ran the testing lesson. Mobile emulation still uses desktop memory and CPU, so it does not establish real-phone feasibility.
- Editor features, diagnostics, persistence and lesson-selected layers belong to the tutorial in [`examples/learn`](../examples/learn/README.md). This minimal page exposes only `?layers=` and `learn.loadLayer`.
- `sound` and `tauri` have no layers because tests here provide neither audio playback coverage nor a Tauri host. Adding either would require an entry in `DEFAULT_LAYERS`.

## Risks for the tutorial

The main constraint is memory: the renderer uses roughly 1.2–1.4 GB, and lesson 17's worker adds another half gigabyte. The older `ghc-lib-parser` setup used about 1.8 GB after loading `th`. Real phones, particularly iOS devices, may terminate such tabs. A read-only mobile mode and explicit compiler loading remain useful safeguards.

The initial compiler download is about 56 MB, with another 11 MB for `th`. The Cache API avoids repeat archive downloads until a changed deployment. Slower machines may also take several times longer to extract and link than the VM used for these measurements. The manifest's byte counts support progress reporting. Desktop WebKit works, but real iPhone memory limits remain untested.

Build the rootfs with the exact GHC pinned in `cabal-wasm.project` and matching builds of every package. Shared libraries and interfaces depend on those ABIs, including the base layer. The build script collects them together; compiler or package changes require rebuilding and checking the tutorial against that build.

The page and dyld must use the same browser_wasi_shim module because WASI validates file-system objects against its classes. The build replaces dyld's unpinned `https://esm.sh/gh/haskell-wasm/browser_wasi_shim` import with the bundled `vendor/browser_wasi_shim/browser_wasi_shim.js`, which pages also import. It fails if the expected import no longer matches. The vendor file documents its source commit and rebuild instructions. Changes affect the rootfs build key, but not the archive-only manifest version; the shim and dyld are fetched as ordinary modules on each visit.

Pixi, Three.js and Material icons still need their external CDNs. Blocked dependencies can leave canvases blank or show icon names as text. Their URLs are listed in the manifest. The local dyld optimisation also needs maintenance: the build applies it only while the expected source still matches.

## Provenance

- `bsdtar.wasm`: [haskell-wasm/bsdtar-wasm](https://github.com/haskell-wasm/bsdtar-wasm), libarchive's bsdtar built for wasm32-wasi with zstd, BSD-2-Clause (Copyright (c) 2025 Cheng Shao); downloaded once from `https://haskell-wasm.github.io/bsdtar-wasm/bsdtar.wasm`, the build GHC's playground loads, and pinned by its SHA-256 in the script.
- `browser_wasi_shim.js`: [haskell-wasm/browser_wasi_shim](https://github.com/haskell-wasm/browser_wasi_shim), MIT OR Apache-2.0 (`vendor/browser_wasi_shim/LICENSE-*`), bundled from its source at commit `2f86b49` (the one esm.sh served) with esbuild, as its header says. That fork rather than `@bjorn3/browser_wasi_shim` from npm: the fork is upstream's unreleased main with GHC's changes, the one GHC's playground is tested on, and the last release, 0.4.2, copies a whole file on every write that grows it, which extracting the rootfs' large libraries would pay for quadratically.
- `dyld.mjs`, `post-link.mjs`, `prelude.mjs`: GHC's, from the toolchain's libdir (BSD-3-Clause), `dyld.mjs` patched as above.
- `layers/material/material.js`, `material.css`: our `material/jsbits` and `examples/material/style.scss`, bundled with the [Material Components for the web](https://github.com/material-components/material-components-web) packages they import (MIT), as the material example is.
- PixiJS ([pixijs/pixijs](https://github.com/pixijs/pixijs), MIT), Three.js ([mrdoob/three.js](https://github.com/mrdoob/three.js), MIT) and the Material Icons font (Apache-2.0) are not served from here: their CDNs serve them.
