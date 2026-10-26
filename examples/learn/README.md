# examples/learn: the tutorial

A tutorial for haskell-halogen whose code compiles in the browser. Each lesson
is a page with its text, a code editor and a Compile button; GHC itself,
compiled to WebAssembly (the playground of [`learn/`](../../learn/README.md)),
compiles the reader's module in the page and mounts the component it defines
beside the text, with no server behind it.

## How it works

The page is a Halogen application (`src/Learn/App.hs`) with a route per
lesson, through `haskell-halogen-router`. The build renders every route's page
natively (`Prerender.hs`, under the site's base path), so a lesson's text is on
the page before any WebAssembly has arrived; `app.wasm` then adopts that page
(`hydrateUI`) and starts the compiler in the background. The compiler is the
rootfs that `toolchain/build-learn-rootfs.sh` builds, which the page looks for
in `rootfs/` under its base: `js/playground.js` downloads it (through the
Cache API, so a second visit does not), extracts it with `bsdtar.wasm` into an
in-memory file system and starts `libplayground.so` with GHC's `dyld.mjs`. The
editor is CodeMirror 6 (`js/editor.js`). Both are bundled by `bundle.sh` into
`learn.js`, which publishes them for the app's bindings
(`jsbits/learn.js`, `Learn.Browser`). A compile writes the reader's code to
`/tmp/Lesson.hs` and the lesson's wrapper to `/tmp/Main.hs` and runs
`Main.main` in GHC's interpreter; the wrapper mounts `Lesson.component` into
`#learn-result`, an element the app renders once per lesson and never patches
inside. GHC's errors come back on stderr; `Learn.Diagnostics` reads their
locations, which the page lists under the editor and marks in it.

**The live check.** As the reader types, the page has GHC typecheck the code
without running it: 1.2 s after the last change (`checkDelay` in
`Learn.App`), once the compiler and the lesson's layers are there and no
compile is under way, it hands the same two modules and arguments to
`js/playground.js`'s `check`, which runs the playground with
`--learn-check` (`learn/Playground.hs` then compiles with `-fno-code` and
runs nothing). Its diagnostics replace the ones in the editor and under it,
and a line under the editor says how long it took ("Checked in 0.4 s: it
compiles."); the result's panel is a compile's alone. The first check runs
when the compiler is ready, so a lesson opened fresh is checked with no key
pressed. One check runs at a time, in the page's one queue of runs with the
compiles (a check waits for a compile under way, and a compile pressed
meanwhile for the check), and a check's result is dropped when the code has
changed since it started or a compile has started since: the compile,
later, says it all again. A change while a check waits out its delay starts
the delay again.

**The types.** Every run that typechecks the program, a compile or a check,
writes a line to stdout for each top-level binding of `Lesson` before what
the program writes there: `learn-type`, the line and column it is bound at,
its name and its type as GHCi's `:type` prints it, with the names the module
imports as it writes them (`H.Component`), separated by tabs
(`learn/README.md` says how the playground finds them). `Learn.Types` takes
those lines out of stdout (`Learn.Browser` does, as it reads a run's result)
and the page shows them twice: in the editor, a tooltip over a name the
module binds at the top level gives its type (`js/editor.js`'s
`showTypes`), and under the editor, "Top-level types" lists them, each going
to its line when clicked. When the code stops typechecking, the last ones
stay, said to be from the last time it compiled. What they know is the
module's top-level bindings and nothing else: not a binding under a `where`
or a `let` (so a lesson that wants its pieces listed binds them at the top
level, as lesson 20 does), not a name the module imports, not a
constructor, a record field, a class's or an instance's methods. A name
right after a dot, qualified or a record's field, shows no tooltip, and a
local name that is also a top-level one shows the top-level one's type.

On a screen under 900 pixels wide the compiler is not loaded by itself (it is
a 63 MB download and takes 1.2 GB of memory once running): the lesson and its
code are there to read, and a button loads the compiler anyway.

## Build, run, test

```sh
npm run build-rootfs                        # the compiler with every layer, into dist-newstyle/wasm/learn (once; see learn/README.md)
sh toolchain/build-wasm.sh learn            # the app, its pages, learn.js and a copy of the rootfs, into dist-newstyle/wasm/public/learn
python3 -m http.server -d dist-newstyle/wasm/public/learn 8080
npm run test-learn                          # headless Chromium: the checks below
```

`bundle.sh` copies the rootfs directory (`LEARN_ROOTFS_DIR`, default
`dist-newstyle/wasm/learn`, but for `learn/`'s own `index.html` and
`index.js`) to `rootfs/` in the output, where the page looks for it, so
`dist-newstyle/wasm/public/learn` is the whole site. Without a rootfs it says
so and goes on, and the page then says the compiler did not load. Rebuild the
rootfs (`npm run build-rootfs`) after changing a library a lesson uses: the
rootfs has its own copies of them, and the app's build does not look at it.

**Deploy.** GitHub Pages serves `dist-newstyle/wasm/public/learn` and nothing
else, at <https://swordlash.github.io/haskell-halogen/>. CI's deploy
(`.github/workflows/build-deploy.yml`) builds the rootfs (or restores it from
its cache, keyed by everything it is made from:
`toolchain/build-learn-rootfs.sh --key`), runs `npm run build-wasm-all` with
`HALOGEN_BASE_PATH=/haskell-halogen/` (the prerender writes it into every
page's `<base href>`, and the page finds `rootfs/` under it), runs
`npm run test-learn` on that tree under the same base path, and uploads it.
The build workflow does the same on every push and pull request, and runs
`npm run test-playground` on the rootfs as well. To try the deployed form
locally:

```sh
HALOGEN_BASE_PATH=/haskell-halogen/ npm run build-wasm-all
HALOGEN_BASE_PATH=/haskell-halogen/ npm run test-learn
```

`npm run test-learn` (`toolchain/test-learn.mjs`) builds nothing: it serves
`dist-newstyle/wasm/public/learn` as Pages would, the rootfs in it, under
`HALOGEN_BASE_PATH` (default `/`; it must be the one the pages were built
for), and checks that every lesson's page arrives with its heading in it,
that the page is adopted and the compiler loads (printing how long each step
took) with no `HEAD` request, the manifest giving the version and the sizes,
that the first two lessons do what their text says (and lesson 15's
connected views render again only for their own part of the store), that a type error is
listed at its line with the last result still mounted, that **every lesson's
`Start.hs` compiles and its `Solution.hs` compiles with no diagnostic and
shows something** (one line per lesson; for lesson 13, whose program runs a
spec, hspec's report under the editor: one failure for `Start.hs`, none for
`Solution.hs`, and nothing left in the result; for lesson 14, whose program
puts a page of its own around the lesson's custom element, that the element
and the page's buttons do what the text says, before the exercise and after
it; for lesson 16, whose program opens `Halogen.DevTools`' panel under the
component, that the panel lists the component and counts a click, and that
its count of renders climbs every second for `Start.hs` and not for
`Solution.hs`; for lesson 17, whose program runs its jobs through
`Halogen.Worker`, that the component says no worker can be made here, and
that each counts the primes right, `Start.hs` drawing no frame meanwhile and
`Solution.hs`, through `Worker.run` in a `liftIO`, at least one; for lesson 18, whose component comes from a shared library
the program links while it runs, that the library's component greets the
name and waves, and for `Solution.hs` that the parent hears it, tells it to
wave and asks it how often, and that hidden and shown again it starts over; for lesson 19,
whose scene is drawn with Three.js, that its canvas is there and that a click at its centre, on the
sphere, changes the colour the text under it names; for lesson 20, that the component counts and
renames), that the page checks the code as the reader types (on lesson 20, whose
`Start.hs` has three holes: with no Compile, its holes' warnings and the types of its
bindings are listed, a tooltip over `render` gives its type, a click in the list goes
to the binding's line, an error typed is marked within the check's delay and goes when
undone, and a check under way when Compile is pressed is not reported over the
compile), that back, forward, a second visit
(from the cache) and Reset work, that a narrow screen downloads nothing large
until asked, that a failed load can be retried and loads from a manifest
without a version or sizes too, and that nothing is logged as an error.
`LEARN_ROOTFS_DIR` serves that rootfs directory at `rootfs/` in place of the
tree's own; `LEARN_DEBUG=1` prints everything the page logs and requests.

`npm run dev -- learn` runs the app in browser GHCi, as for every example;
`bundle.sh` copies the rootfs beside its page too, so the compiler loads
there as well.

## Writing a lesson

A lesson is a directory of `lessons/`, named `NN-slug`: two digits, its
number, and a slug of lower-case letters, digits and hyphens, which is its
address (`/03-components/`) and the key the reader's code is kept under in
the browser. Lessons are numbered from 01 with no gaps, and **listed in
`lessons/index.txt`**, one directory per line in order: the build reads only
what the index lists (and fails for a lesson directory it leaves out),
because a new file is something cabal does not notice even when it matches
the package's `extra-source-files`, while an edited index is; an edited
lesson file it does notice. That holds for a lesson's optional `Main.hs`
too: one added to a lesson that had none goes unseen until the index is
edited (a blank line will do, or a comment, a line starting with `#`). The files are embedded into the app at compile time
(`Learn.Lessons`); a lesson that cannot be read fails the build with what is
wrong with it.

| File | What |
| --- | --- |
| `lesson.md` | The header, then the text. |
| `Start.hs` | What the editor opens with, and what Reset goes back to. It must compile. |
| `Solution.hs` | The answer, which the Solution button loads. It must compile with no warning and show something (a component, or what the program prints): the test checks both. |
| `Main.hs` | Optional: the program around the lesson's module, in place of the default one below. |

Run `sh toolchain/build-wasm.sh learn` and `npm run test-learn` to check a
lesson: the test compiles every lesson's `Start.hs` and `Solution.hs` in the
browser.

### The header

The first lines of `lesson.md`, up to the first blank line, are `key: value`:

```
title: State and actions
layers: hooks
args: -package haskell-halogen-hooks
```

- `title` (required): the lesson's name, its page's heading (as
  "2. State and actions") and its title in the list of lessons.
- `layers` (optional): the rootfs layers its code needs beside the base one,
  separated by spaces or commas. The page extracts a layer the first time a
  lesson needs it and passes its package database to GHC (`-package-db`).
  The rootfs has `hooks`, `router`, `net`, `material`, `pixi`, `three`, `th`
  and `store`, one per package of the same name (`haskell-halogen-hooks`, ...),
  `elements`, for `haskell-halogen-custom-element`, `devtools`, for
  `haskell-halogen-devtools`, `worker`, for `haskell-halogen-worker`,
  `hspec`, for `hspec-halogen`, and `lazy`, for `haskell-halogen-lazy` and
  `haskell-halogen-lazy-demo` (see `learn/README.md`, "Layers").
  `lazy` also has a library of its own beside the archive,
  `rootfs/layers/lazy/liblazy-demo.so`, which lesson 18's program fetches
  and links into itself while it runs, with `Halogen.Lazy.Dyld`: the page
  hands its dyld over for that (`js/playground.js` sets
  `globalThis.halogenDyld`).
- `args` (optional): further GHC arguments, separated by spaces, after the
  ones every compile has (`-package haskell-halogen-core
  -Wno-missed-extra-shared-lib -i/tmp`). A layer's package still has to be
  named here with `-package`, as above. The live check passes them too.
  Lesson 20's are `-fdefer-typed-holes`, which makes its `Start.hs`'s holes
  warnings, so that it compiles as a start must.

### The text

The text is Markdown, of which the page reads this much (`Learn.Markdown`):

- headings `##` and `###` (the title is the page's only `#`, so a `#` heading
  in the text is an error);
- paragraphs: lines that follow one another, up to a blank line;
- fenced code blocks between lines of three backticks; ` ```haskell ` marks
  one as Haskell (the `<code>` gets `class="language-haskell"`);
- unordered lists (`- ` or `* ` at the start of a line) and ordered lists
  (`1. `, which start at their first item's number); an item can go on over
  lines indented under it, but cannot hold blocks of its own; blank lines
  between items do not end a list; lists do not nest, and an indented item
  is a build error;
- blockquotes: lines starting with `>`, whose content is read as blocks;
- inline, anywhere: `` `code` ``, `**bold**`, `*italic*` or `_italic_` (an
  underscore inside a word, as in `handle_action`, is just an underscore, and
  so is a `*` before a space or another `*`), `[text](address)` (the text
  ends at the first `]`, and the address at the `)` that closes its `(`, so
  it may hold parentheses that balance), and `\` before a character to
  write it as it is.

Everything else is text as written. Something opened and not closed (a code
block, a backtick, a `**`, a link) is a build error naming the line.

Keep a lesson's text to what its code shows: every identifier it names
should appear in `Start.hs` or `Solution.hs`, so the reader can find it.

### The code: the `Lesson` module

`Start.hs` and `Solution.hs` are the module `Lesson`, which exports
`component`. With the default wrapper its type is

```haskell
component :: forall q o m. (MonadIO m) => H.Component q () o m
```

or anything that can be used at that type with `m` the browser's own monad,
`BrowserDOM` (`H.Component q () o BrowserDOM` itself, for a hooks component,
say): the wrapper mounts it with no input, whatever its queries and outputs.
A lesson builds it as lesson 1 teaches, `H.component initialState render
[...]`, and the later lessons say what it does in that list of handlers
(`H.onAction`, `H.onQuery`, `H.onInitialize`, `H.onFinalize`). The language
is GHC2021; anything else (`OverloadedStrings` for `HH.text "..."`,
`QualifiedDo` for hooks) is a `LANGUAGE` pragma in the file, as it would be
in a project of the reader's own.

The default wrapper (`Learn.Lesson.defaultMain`) is

```haskell
main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  result <- HA.selectElement (QuerySelector "#learn-result") >>= maybe (error "the page has no #learn-result") pure
  socket <- runUI component () result
  HA.keepForNextRun socket.dispose
```

It ends what the last compile mounted (`Halogen.IO.Util`: the compiler's
session keeps `haskell-halogen-core` loaded, so the reference that
`keepForNextRun` keeps survives from one run to the next), mounts the
component and keeps how to end it. Write a `Main.hs` of your own when the
lesson needs something else around its component: an input, the component's
output messages printed with `putStrLn` (the page shows what the program
writes to stdout while the compile runs, a line at a time, as the playground
makes stdout line-buffered and flushes it when a run ends: so what the previous
run's `H.onFinalize` prints, which the next compile's `HA.endPreviousRun`
runs, shows under the editor, and a click handler's output does not), two
components, a router, a store (lesson 15's runs the tree through
`Store.runStoreT` before `runUI`). It is the module `Main`, imports
`Lesson`, and must do the same three things: `HA.endPreviousRun` first, mount
into `#learn-result`, and `HA.keepForNextRun` with whatever ends what it
started. Lesson 14's puts plain HTML and a script into `#learn-result`
rather than mount there, defines a custom element over it from the
`definition` its `Lesson` exports beside `component`, and keeps the
registration's `dispose` and the HTML's removal. Lesson 16's puts two
elements into `#learn-result`, opens `Halogen.DevTools`' panel in the second
(after `DevTools.showing @State`, which is why its `Lesson` exports `State`
beside `component`) and then mounts the component in the first, and keeps
both sockets' `dispose` and the elements' removal. Lesson 17's is the
program an application with Web Workers ships: `Worker.program jobs` (the
table its `Lesson` exports beside `component`) around the rest, which makes
a pool and mounts `component pool`, and keeps the socket's `dispose` and
`Worker.terminate pool`; in the tutorial the pool starts no worker (the
interpreted program has no binary for one to load), and the jobs run on the
page. A `Main.hs` may also run a
spec of the lesson's instead, as lesson 13's does: `Lesson` exports `spec` beside `component`, and `main` is
`HA.endPreviousRun` then `runBrowserTestsIn result (spec BrowserDOM)`
(`hspec-halogen`'s `Test.Hspec.Halogen`), which runs each test in a page of
its own inside `#learn-result` and prints hspec's report to stdout, which
the page shows. There is no Playwright in the page, so the harness clicks
and types with synthetic events of its own (`hspec-halogen/README.md`,
"Without the runner"). Each test takes down what it mounted, so the run
leaves `#learn-result` empty and keeps nothing for the next. (The page ends the last run itself when the reader leaves a lesson,
by compiling a `Main` of its own whose `main` is `HA.endPreviousRun`, so that
nothing a lesson started goes on against an element that is gone.)

What the code can use: GHC's boot libraries (`base`, `text`, `containers`,
`mtl`, `transformers`, `bytestring`, `template-haskell` and the rest, but not
`Cabal`), `haskell-halogen-core`, `haskell-halogen-jsbits` and everything they
depend on (`aeson`, `vector`, `clay`, `row-types`, `unliftio`, `protolude`,
...), and the packages of the layers the header names (the `hspec` layer's
`hspec-halogen` brings `hspec`, `hspec-core` and `HUnit` with it, and the
`lazy` layer's `haskell-halogen-lazy-demo` the types lesson 18's library
shares with the program). Template Haskell
works. A `foreign import javascript` of the lesson's own does not (GHC would
need clang in the page, `learn/README.md`): JavaScript is reached through the
libraries' bindings.

## The rootfs, and what a deploy needs

The page loads `rootfs/rootfs.json` relative to its `<base href>` (the
prerender writes `HALOGEN_BASE_PATH` into it), and from it the base archive's
parts, a layer's when a lesson names it, `bsdtar.wasm` and `dyld.mjs` with the
two modules it imports. The manifest is described in
[`learn/README.md`, "The manifest"](../../learn/README.md#the-manifest-rootfsjson);
what `js/playground.js` reads of it beyond what dyld and GHC are given:

- `version`: the archives are kept in the Cache API under it, each by its
  `archive` name, so a new rootfs is never taken for the old one; once a new
  version is downloaded, whatever the cache holds under another is deleted.
- each archive's `parts`, fetched one after the other and joined, and its
  `compressedSize`, what the introduction and a lesson's heading say it
  downloads and what the progress bar counts towards.
- a layer's `assets`, added to the page once each when the layer has been
  extracted and before it is reported ready: a `kind: "script"` as a
  `<script src>`, a `kind: "stylesheet"` as a `<link rel=stylesheet>`, their
  paths relative to `rootfs/`. Only `material` has any (`material.js`, which
  its components call, and `material.css`, without which they are
  unstyled).
- a layer's `external` entries are what its code loads from elsewhere at run
  time, which the page does not provide and the reader's browser must reach:
  PixiJS from jsDelivr for `pixi` (when a canvas mounts), Three.js from
  jsDelivr for `three` (when a scene mounts) and the Material
  Icons font from fonts.gstatic.com for `material` (named by `material.css`,
  fetched only when an icon shows). The page reads nothing from them; a
  lesson using those layers should say it needs the network for them.

For a manifest without `version`, `parts` or `compressedSize` (an older
build's, or one written by hand) the page asks the server with a `HEAD`
request per part and uses its `ETag` (or `Last-Modified`) as the version and
its `Content-Length` as the size, which works on GitHub Pages and any
ordinary static server, at the cost of a request per part on every visit.

## Notes

- The compiler's `browser_wasi_shim` is the rootfs' `browser_wasi_shim.js`, a
  copy of haskell-wasm's fork kept in `learn/vendor/`, which the rootfs build
  also points its `dyld.mjs` at, since the page must share dyld's copy
  (`learn/README.md`). The app's own `index.js` uses
  `@bjorn3/browser_wasi_shim` 0.3.0, bundled in from `node_modules` by
  `bundle.sh` (every other example's imports it from jsDelivr); the two do not
  meet. So the page loads nothing from another site but a layer's `external`
  dependencies, which `npm run test-learn` checks.
- The app and the compiled program are two Haskell programs in one page, each
  with its own runtime. Both load `haskell-halogen-core`'s JavaScript, which
  `Halogen.JSBits.wasmJS` keeps on `globalThis` under a key made of the
  package, the module and the splice's position, so the second one loaded may
  replace the first one's functions with the same functions. That is harmless as long as those files keep no state of their
  own (only `core/jsbits/indexeddb.js` does, and the app does not use
  IndexedDB).
