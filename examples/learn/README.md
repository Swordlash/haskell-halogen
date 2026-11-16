# examples/learn: the tutorial

Learn haskell-halogen in the browser. Each lesson has an explanation, a code editor and a Compile button. GHC runs as WebAssembly in the page, compiles your module and mounts its component beside the lesson. No compilation server is needed. The compiler comes from [`learn/`](../../learn/README.md). Use `/playground/` to write code without a lesson.

## How it works

The tutorial is a Halogen app in `src/Learn/App.hs`, with one route per lesson and one for the playground. `Prerender.hs` renders each route natively under the site's base path. Readers can therefore see the lesson before WebAssembly loads. `app.wasm` adopts the HTML with `hydrateUI` and starts the compiler in the background.

`js/playground.js` downloads the compiler rootfs from `rootfs/`, caching archives with the Cache API. It extracts them into an in-memory file system with `bsdtar.wasm` and starts `libplayground.so` through GHC's `dyld.mjs`. `js/editor.js` provides the CodeMirror 6 editor. `bundle.sh` bundles both into `learn.js` for the bindings in `jsbits/learn.js` and `Learn.Browser`.

Compilation writes the reader's code to `/tmp/Lesson.hs` and the lesson wrapper to `/tmp/Main.hs`, then runs `Main.main` in GHC's interpreter. The wrapper mounts `Lesson.component` in `#learn-result`. The app creates this container once per lesson and never patches its contents. `Learn.Diagnostics` parses GHC's stderr to show errors below the editor and mark their locations.

**Live checking.** After 1.2 seconds without an edit, the page asks GHC to typecheck the code. `checkDelay` in `Learn.App` controls this delay. Checking starts only when the compiler and layers are ready and no compile is running. It uses the same modules and arguments as compilation, with `--learn-check` selecting `-fno-code` and skipping execution.

Check results update diagnostics and display timing, such as "Checked in 0.4 s: it compiles." They do not replace the mounted result. The first check runs automatically when the compiler becomes ready. Checks and compiles share one queue. Edits restart the debounce timer, and stale check results are discarded if the code changed or a compile started afterward.

**Type information.** Compiles and checks report top-level `Lesson` bindings on stdout before program output. Each tab-separated line contains `learn-type`, source line, column, name and a type formatted like GHCi's `:type`, using the module's imported names. `Learn.Types` removes these lines from displayed stdout through `Learn.Browser`.

The editor shows types on hover, and "Top-level types" lists clickable entries below it. If checking fails, the previous types remain marked as coming from the last successful compile. Only top-level bindings are included: local `where` and `let` bindings, imports, constructors, fields and methods are not. Names following a dot have no tooltip. A local name shadowing a top-level name may show the top-level type.

**Source navigation.** Every compile and check enables `--learn-spans`. The parser plugin marks supported elements with `data-learn-src="<line>:<column>"`, identifying their constructor's position in `Lesson.hs`. Source mode, implemented in `js/source.js`, outlines the element under the pointer and marks its source line. Clicking moves the editor cursor there and stops the click in capture phase, so the lesson does not handle it.

Cursor changes also select elements: the app matches the exact source position or the latest element constructor before the cursor on that line. Source mode stays enabled across compiles. The editor retains the compiled text and later edits so source positions track inserted and deleted lines. Elements whose source was deleted show that status without linking to a line. `Learn.Diagnostics.withoutMarks` removes generated wrappers from code quoted in diagnostics.

The plugin marks fully applied functions from `Halogen.HTML.Elements` and `Halogen.Svg.Elements`, qualified or unqualified. This includes ordinary elements, `_` variants, leaves, `HH.element`, `HH.withKeys` and applications using `$`, including chains. It wraps the result with `Halogen.HTML.Core.withProps`, placing the source attribute before user properties so a user-provided attribute wins.

Partially applied constructors cannot be wrapped. If their first argument is a literal property list, as in `map (HH.li [HP.class_ c]) groups`, the mark is inserted there. Expressions such as `map HH.li_ items` and generated `hh` markup receive no mark; Source mode uses the nearest marked ancestor. Write out the element in a lambda when each result needs a mark.

These are ordinary DOM attributes, present even when Source mode is off. Attribute-inspection tests see them, lesson 12 adds them during hydration, and lesson 21 sends them as remote `attr` operations.

**The playground.** `/playground/` replaces lesson text with layer checkboxes, extra GHC arguments and a link back to the lessons. Lesson slugs start with digits, so they cannot conflict with this route. Each checkbox shows the layer's packages and download size or loaded status. Selecting it loads the layer and adds its packages to subsequent compiles. Extra arguments follow those package flags.

Code is a `Lesson` module mounted with the default wrapper. Reset restores `Learn.Playground.template`, a counter. Code, selected layers and arguments are saved in `localStorage` as best effort. "Open in playground" transfers the current lesson code through app state, so it also works when storage writes fail. It selects the lesson's layers and copies non-package arguments, such as lesson 20's `-fdefer-typed-holes`.

Custom lesson wrappers are not transferred. Lessons 6, 12–17 and 21 rely on their own `Main.hs` for routers, stores, pools, tests or surrounding UI; their code may need adaptation to the playground's default wrapper. Links to the playground appear in the introduction and lesson pager.

Below 900 pixels wide, the compiler does not load automatically. Readers can view lessons and code and explicitly request the compiler. This avoids an automatic download of roughly 63 MB and runtime memory use of about 1.2 GB.

## Build, run, test

```sh
npm run build-rootfs                        # the compiler with every layer, into dist-newstyle/wasm/learn (once; see learn/README.md)
sh toolchain/build-wasm.sh learn            # the app, its pages, learn.js and a copy of the rootfs, into dist-newstyle/wasm/public/learn
python3 -m http.server -d dist-newstyle/wasm/public/learn 8080
npm run test-learn                          # headless Chromium: the checks below
```

`bundle.sh` copies the compiler rootfs from `LEARN_ROOTFS_DIR`, defaulting to `dist-newstyle/wasm/learn`, into the output's `rootfs/`. It excludes the standalone playground page files. The resulting `dist-newstyle/wasm/public/learn` contains the complete site. If the rootfs is missing, bundling continues with a warning and the page reports a compiler-load failure. Rebuild it with `npm run build-rootfs` after changing a library used by lessons; rebuilding the app alone does not update those copies.

**Deployment.** GitHub Pages serves `dist-newstyle/wasm/public/learn` at <https://swordlash.github.io/haskell-halogen/>. `.github/workflows/build-deploy.yml` builds or restores the rootfs using the key from `toolchain/build-learn-rootfs.sh --key`, then runs `npm run build-wasm-all` with `HALOGEN_BASE_PATH=/haskell-halogen/`. That base is written into each page's `<base href>`.

Deployment runs `npm run test-learn` against the same output and base path before uploading. The build workflow's `learn-wasm` job performs the same checks on pushes and pull requests, plus `npm run test-playground` and `npm run test-worker`. To reproduce the deployed layout locally:

```sh
HALOGEN_BASE_PATH=/haskell-halogen/ npm run build-wasm-all
HALOGEN_BASE_PATH=/haskell-halogen/ npm run test-learn
```

`npm run test-learn` runs the native suite in `browser-test/` through `haskell-halogen-playwright` and `toolchain/test-example.sh learn`. It builds the tests, but not the app or rootfs. It serves `dist-newstyle/wasm/public/learn` under `HALOGEN_BASE_PATH`, which must match the build.

The suite checks prerendered headings, clean hydration on narrow screens, compiler startup without metadata `HEAD` requests, routing, caching, Reset, load retries and older manifest formats. It checks initial lesson behaviour, selector-specific updates in lesson 15, and no large downloads on narrow screens until requested.

Source tests cover pointer outlines, editor marks, click navigation without triggering the component, cursor selection, inserted and removed lines, deleted source, `$` chains and returning to normal interaction. Playground tests cover prerendering, navigation from lessons even when storage writes fail, template compilation, Source mode, layer loading and persistence of code, layers and arguments.

Every lesson's `Start.hs` must compile. Every `Solution.hs` must compile without diagnostics and produce visible content or output. Two lessons run concurrently in separate browser contexts. Additional checks cover lesson 13's expected failing/passing test reports; lesson 14's custom elements; lesson 16's devtools counts; lesson 17's worker results and frame responsiveness; lesson 18's dynamic component, queries, outputs and remounts; lesson 19's Three.js interaction; lesson 20's count and rename actions; and lesson 21's remote input, add and clear operations.

Live-check tests verify typed-hole warnings, top-level types, hover information, source navigation, edited errors and stale-result suppression when Compile is pressed. A compilation error must leave the previous result mounted. Browser console errors fail the checks.

`LEARN_ROOTFS_DIR` overrides the served rootfs. `LEARN_DEBUG=1` streams page logs, and `LEARN_TIMEOUT` sets the load, compile and check deadline in seconds, defaulting to 300. Tests wait for program activity rather than arbitrary delays: the app counts compiler work in `H.async`, and compiled lessons expose their own activity. `browser-test/Tutorial/Site.hs` documents conditions that require separate waits. Repeating lesson timers use `H.delay` or `Hooks.delay`, as lesson 8 does, so they do not prevent quiet waits from completing.

`npm run dev -- learn` runs the app in browser GHCi, as for every example; `bundle.sh` copies the rootfs beside its page too, so the compiler loads there as well.

## Writing a lesson

Create a lesson directory under `lessons/` named `NN-slug`, with a two-digit number and a lowercase slug containing letters, digits or hyphens. The name determines its URL, such as `/03-components/`, and its browser-storage key. Number lessons from 01 without gaps and list every directory in `lessons/index.txt` in order.

The build embeds these files through `Learn.Lessons` and fails on invalid lessons or unlisted directories. Cabal notices edits to existing files but may miss newly added files, including an optional `Main.hs`. Edit the index whenever adding one; a blank line or `#` comment is enough to trigger rebuilding.

| File | What |
| --- | --- |
| `lesson.md` | The header, then the text. |
| `Start.hs` | What the editor opens with, and what Reset goes back to. It must compile. |
| `Solution.hs` | The answer, which the Solution button loads. It must compile with no warning and show something (a component, or what the program prints): the test checks both. |
| `Main.hs` | Optional: the program around the lesson's module, in place of the default one below. |

Run `sh toolchain/build-wasm.sh learn` and `npm run test-learn` to check a lesson: the test compiles every lesson's `Start.hs` and `Solution.hs` in the browser.

### The header

The first lines of `lesson.md`, up to the first blank line, are `key: value`:

```
title: State and actions
layers: hooks
args: -package haskell-halogen-hooks
```

- `title` (required): the lesson's name, its page's heading (as "2. State and actions") and its title in the list of lessons.
- `layers` (optional): the rootfs layers its code needs beside the base one, separated by spaces or commas. The page extracts a layer the first time a lesson needs it and passes its package database to GHC (`-package-db`). The rootfs has `hooks`, `router`, `net`, `material`, `pixi`, `three`, `th` and `store`, one per package of the same name (`haskell-halogen-hooks`, ...), `elements`, for `haskell-halogen-custom-element`, `devtools`, for `haskell-halogen-devtools`, `worker`, for `haskell-halogen-worker`, `hspec`, for `hspec-halogen`, `lazy`, for `haskell-halogen-lazy` and `haskell-halogen-lazy-demo`, and `live`, for `haskell-halogen-remote` (see `learn/README.md`, "Layers"). `lazy` also has a library of its own beside the archive, `rootfs/layers/lazy/liblazy-demo.so`, which lesson 18's program fetches and links into itself while it runs, with `Halogen.Lazy.Dyld`: the page hands its dyld over for that (`js/playground.js` sets `globalThis.halogenDyld`).
- `args` (optional): further GHC arguments, separated by spaces, after the ones every compile has (`-package haskell-halogen-core -Wno-missed-extra-shared-lib -i/tmp`). A layer's package still has to be named here with `-package`, as above. The live check passes them too. Lesson 20's are `-fdefer-typed-holes`, which makes its `Start.hs`'s holes warnings, so that it compiles as a start must.

### The text

The text is Markdown, of which the page reads this much (`Learn.Markdown`):

- headings `##` and `###` (the title is the page's only `#`, so a `#` heading in the text is an error);
- paragraphs: lines that follow one another, up to a blank line;
- fenced code blocks between lines of three backticks; ` ```haskell ` marks one as Haskell (the `<code>` gets `class="language-haskell"`);
- unordered lists (`- ` or `* ` at the start of a line) and ordered lists (`1. `, which start at their first item's number); an item can go on over lines indented under it, but cannot hold blocks of its own; blank lines between items do not end a list; lists do not nest, and an indented item is a build error;
- blockquotes: lines starting with `>`, whose content is read as blocks;
- inline, anywhere: `` `code` ``, `**bold**`, `*italic*` or `_italic_` (an underscore inside a word, as in `handle_action`, is just an underscore, and so is a `*` before a space or another `*`), `[text](address)` (the text ends at the first `]`, and the address at the `)` that closes its `(`, so it may hold parentheses that balance), and `\` before a character to write it as it is.

Everything else is text as written. Something opened and not closed (a code block, a backtick, a `**`, a link) is a build error naming the line.

Keep a lesson's text to what its code shows: every identifier it names should appear in `Start.hs` or `Solution.hs`, so the reader can find it.

### The code: the `Lesson` module

`Start.hs` and `Solution.hs` are the module `Lesson`, which exports `component`. With the default wrapper its type is

```haskell
component :: forall q o m. (MonadIO m) => H.Component q () o m
```

A component specialised to `BrowserDOM` also works. The default wrapper supplies `()` input and accepts any query and output types. Use `H.component initialState render [...]` with lifecycle and action handlers as taught in the lessons. Files use GHC2021; declare additional extensions such as `OverloadedStrings` or `QualifiedDo` with normal `LANGUAGE` pragmas.

The default wrapper (`Learn.Lesson.defaultMain`) is

```haskell
main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  result <- HA.selectElement (QuerySelector "#learn-result") >>= maybe (error "the page has no #learn-result") pure
  socket <- runUI component () result
  HA.keepForNextRun socket.dispose
```

The wrapper disposes the previous run, mounts the new component and registers its cleanup. Core stays loaded across interpreter runs, so the cleanup reference survives between compiles.

Provide `Main.hs` when a lesson needs custom input, output logging, multiple components, a router or a store. It must import `Lesson`, call `HA.endPreviousRun`, mount into `#learn-result`, and register all cleanup with `HA.keepForNextRun`. Stdout is shown while a compile runs, including previous-run finalizers; output from later click handlers is not included in that report.

Existing custom wrappers provide useful examples. Lesson 15 runs `Store.runStoreT` before mounting. Lesson 14 installs a custom element into plain HTML and cleans up both registration and markup. Lesson 16 creates containers for the component and devtools, registers `DevTools.showing @State`, and disposes both sockets and containers.

Lesson 17 wraps startup in `Worker.program`, creates a pool, mounts `component pool`, and registers pool termination. Before compiling, the page configures `lesson-worker.js` as `halogenWorker.script` with the compile request. Each worker starts another compiler from the same rootfs, compiles identical modules and flags to preserve static keys, and runs the same `main`. Its first job waits for this startup.

Lesson 21 mounts a page component that runs the lesson through `Halogen.Remote.Loopback` and shows its JSON traffic in a `<pre>`, newest first. Its finalizer disposes the loopback.

Lesson 13 runs `runBrowserTestsIn result (spec BrowserDOM)` instead of mounting a result component. The harness uses synthetic input because there is no Playwright runner in the page. Each test cleans up its container, and the stdout report appears under the editor. See `hspec-halogen/README.md`, "Without the runner".

When leaving a lesson, the app compiles a small `Main` that calls `HA.endPreviousRun`, ensuring the old program stops before its result container disappears.

Lessons can use GHC boot libraries except trimmed packages such as Cabal, `haskell-halogen-core`, `haskell-halogen-jsbits`, their dependencies and packages from selected layers. The `hspec` layer includes hspec and HUnit; `lazy` includes the shared demo types. Template Haskell works. Lesson-defined `foreign import javascript` declarations do not, because compiling them would require clang in the browser. Use library-provided bindings instead; see `learn/README.md`.

## The rootfs, and what a deploy needs

The page resolves `rootfs/rootfs.json` against its `<base href>`, supplied by prerendering from `HALOGEN_BASE_PATH`. The manifest identifies archives, layers, `bsdtar.wasm`, `dyld.mjs` and its imports. See [`learn/README.md`, "The manifest"](../../learn/README.md#the-manifest-rootfsjson) for the format. `js/playground.js` also uses these fields for caching and UI:

- `version`: the archives are kept in the Cache API under it, each by its `archive` name, so a new rootfs is never taken for the old one; once a new version is downloaded, whatever the cache holds under another is deleted.
- each archive's `parts`, fetched one after the other and joined, and its `compressedSize`, what the introduction and a lesson's heading say it downloads and what the progress bar counts towards.
- a layer's `assets`, added to the page once each when the layer has been extracted and before it is reported ready: a `kind: "script"` as a `<script src>`, a `kind: "stylesheet"` as a `<link rel=stylesheet>`, their paths relative to `rootfs/`. Only `material` has any (`material.js`, which its components call, and `material.css`, without which they are unstyled).
- a layer's `external` entries are what its code loads from elsewhere at run time, which the page does not provide and the reader's browser must reach: PixiJS from jsDelivr for `pixi` (when a canvas mounts), Three.js from jsDelivr for `three` (when a scene mounts) and the Material Icons font from fonts.gstatic.com for `material` (named by `material.css`, fetched only when an icon shows). The page reads nothing from them; a lesson using those layers should say it needs the network for them.

For a manifest without `version`, `parts` or `compressedSize` (an older build's, or one written by hand) the page asks the server with a `HEAD` request per part and uses its `ETag` (or `Last-Modified`) as the version and its `Content-Length` as the size, which works on GitHub Pages and any ordinary static server, at the cost of a request per part on every visit.

## Notes

- The compiler's `browser_wasi_shim` is the rootfs' `browser_wasi_shim.js`, a copy of haskell-wasm's fork kept in `learn/vendor/`, which the rootfs build also points its `dyld.mjs` at, since the page must share dyld's copy (`learn/README.md`). The app's own `index.js` uses `@bjorn3/browser_wasi_shim` 0.3.0, bundled in from `node_modules` by `bundle.sh` (every other example's imports it from jsDelivr); the two do not meet. So the page loads nothing from another site but a layer's `external` dependencies, which `npm run test-learn` checks.
- The app and the compiled program are two Haskell programs in one page, each with its own runtime. Both load `haskell-halogen-core`'s JavaScript, which `Halogen.JSBits.wasmJS` keeps on `globalThis` under a key made of the package, the module and the splice's position, so the second one loaded may replace the first one's functions with the same functions. That is harmless as long as those files keep no state of their own (only `core/jsbits/indexeddb.js` does, and the app does not use IndexedDB).
