# examples/learn: the tutorial

A tutorial for haskell-halogen whose code compiles in the browser. Each lesson
is a page with its text, a code editor and a Compile button; GHC itself,
compiled to WebAssembly (the playground of [`learn/`](../../learn/README.md)),
compiles the reader's module in the page and mounts the component it defines
beside the text, with no server behind it.

## How it works

The page is a Halogen application (`src/Learn/App.hs`) with a route per
lesson, through `haskell-halogen-router`. The build renders every route's page
natively (`Prerender.hs`, as `examples/all` does), so a lesson's text is on
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

On a screen under 900 pixels wide the compiler is not loaded by itself (it is
a 63 MB download and takes 1.2 GB of memory once running): the lesson and its
code are there to read, and a button loads the compiler anyway.

## Build, run, test

```sh
npm run build-learn                         # the rootfs, into dist-newstyle/wasm/learn (once; see learn/README.md)
sh toolchain/build-wasm.sh learn            # the app, prerendered pages and learn.js, into dist-newstyle/wasm/public/learn
ln -s ../../learn dist-newstyle/wasm/public/learn/rootfs   # the rootfs where the page looks for it
python3 -m http.server -d dist-newstyle/wasm/public/learn 8080
npm run test-learn-app                      # headless Chromium: the checks below
```

`npm run test-learn-app` (`toolchain/test-learn-app.mjs`) builds nothing: it
serves `dist-newstyle/wasm/public/learn` with the rootfs directory
(`LEARN_ROOTFS_DIR`, default `dist-newstyle/wasm/learn`) at `rootfs/`, and
checks that every lesson's page arrives with its heading in it, that the page
is adopted and the compiler loads (printing how long each step took), that the
first two lessons do what their text says, that a type error is listed at its
line with the last result still mounted, that **every lesson's `Start.hs`
compiles and its `Solution.hs` compiles with no diagnostic and shows
something** (one line per lesson), that back, forward, a second visit (from
the cache) and Reset work, that a narrow screen downloads nothing large until
asked, that a failed load can be retried, and that nothing is logged as an
error. `LEARN_DEBUG=1` prints everything the page logs and requests.

`npm run dev -- learn` runs the app in browser GHCi, as for every example; the
compiler then needs the rootfs at `dist-newstyle/wasm-dev/examples/learn/rootfs`
(a symbolic link to `dist-newstyle/wasm/learn`, made after the dev loop has
started, as it writes that directory afresh each time). Without it the page
says the compiler did not load, and everything else works.

## Writing a lesson

A lesson is a directory of `lessons/`, named `NN-slug`: two digits, its
number, and a slug of lower-case letters, digits and hyphens, which is its
address (`/03-components/`) and the key the reader's code is kept under in
the browser. Lessons are numbered from 01 with no gaps, and **listed in
`lessons/index.txt`**, one directory per line in order: the build reads only
what the index lists (and fails for a lesson directory it leaves out),
because a new file is something cabal does not notice even when it matches
the package's `extra-source-files`, while an edited index is; an edited
lesson file it does notice. The files are embedded into the app at compile time
(`Learn.Lessons`); a lesson that cannot be read fails the build with what is
wrong with it.

| File | What |
| --- | --- |
| `lesson.md` | The header, then the text. |
| `Start.hs` | What the editor opens with, and what Reset goes back to. It must compile. |
| `Solution.hs` | The answer, which the Solution button loads. It must compile with no warning and show something: the test checks both. |
| `Main.hs` | Optional: the program around the lesson's module, in place of the default one below. |

Run `sh toolchain/build-wasm.sh learn` and `npm run test-learn-app` to check a
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
  The rootfs has `hooks` (`haskell-halogen-hooks`) now; more are coming (see
  `learn/README.md` and `toolchain/build-learn-rootfs.sh`'s `LEARN_LAYERS`).
- `args` (optional): further GHC arguments, separated by spaces, after the
  ones every compile has (`-package haskell-halogen-core
  -Wno-missed-extra-shared-lib -i/tmp`). A layer's package still has to be
  named here with `-package`, as above.

### The text

The text is Markdown, of which the page reads this much (`Learn.Markdown`):

- headings `##` and `###` (the title is the page's only `#`, so a `#` heading
  in the text is an error);
- paragraphs: lines that follow one another, up to a blank line;
- fenced code blocks between lines of three backticks; ` ```haskell ` marks
  one as Haskell (the `<code>` gets `class="language-haskell"`);
- unordered lists (`- ` or `* ` at the start of a line) and ordered lists
  (`1. `); an item can go on over lines indented under it, but cannot hold
  blocks of its own, and lists do not nest;
- blockquotes: lines starting with `>`, whose content is read as blocks;
- inline, anywhere: `` `code` ``, `**bold**`, `*italic*` or `_italic_` (an
  underscore inside a word, as in `handle_action`, is just an underscore, and
  so is a `*` before a space), `[text](address)`, and `\` before a character
  to write it as it is.

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
The language is GHC2021; anything else (`OverloadedStrings` for `HH.text
"..."`, `QualifiedDo` for hooks) is a `LANGUAGE` pragma in the file, as it
would be in a project of the reader's own.

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
writes to stdout while the compile runs, so a `Finalize` of the previous run
shows and a click handler's output does not), two components, a router. It is the module `Main`, imports
`Lesson`, and must do the same three things: `HA.endPreviousRun` first, mount
into `#learn-result`, and `HA.keepForNextRun` with whatever ends what it
started. (The page ends the last run itself when the reader leaves a lesson,
by compiling a `Main` of its own whose `main` is `HA.endPreviousRun`, so that
nothing a lesson started goes on against an element that is gone.)

What the code can use: GHC's boot libraries (`base`, `text`, `containers`,
`mtl`, `transformers`, `bytestring`, `template-haskell` and the rest, but not
`Cabal`), `haskell-halogen-core`, `haskell-halogen-jsbits` and everything they
depend on (`aeson`, `vector`, `clay`, `row-types`, `unliftio`, `protolude`,
...), and the packages of the layers the header names. Template Haskell
works. A `foreign import javascript` of the lesson's own does not (GHC would
need clang in the page, `learn/README.md`): JavaScript is reached through the
libraries' bindings.

## The rootfs, and what a deploy needs

The page loads `rootfs/rootfs.json` relative to its `<base href>` (the
prerender writes `HALOGEN_BASE_PATH` into it), and from it `rootfs.tar.zst`,
`layers/<name>.tar.zst`, `bsdtar.wasm` and `dyld.mjs` with the two modules it
imports. A deploy copies the directory `npm run build-learn` writes (without
its own `index.html` and `index.js`, which are `learn/`'s test page) to
`rootfs/` beside the app.

The archives are kept in the Cache API, under a version: a new rootfs must
not be taken for the old one. The page reads these optional fields of
`rootfs.json`, which `build-learn-rootfs.sh` does not write yet:

- `version` (top level): any string that changes whenever an archive does,
  such as a hash of the archives. Every archive is cached under it.
- `size` (top level, for `rootfs.tar.zst`, and on each layer): the archive's
  size in bytes, which the introduction shows.
- `archive` (top level): the base archive's path, default `rootfs.tar.zst`.

Without `version` or `size` the page asks the server with a `HEAD` request per
archive and uses its `ETag` (or `Last-Modified`) as the version and its
`Content-Length` as the size, which works on GitHub Pages and any ordinary
static server, at the cost of a request per archive on every visit.

## Notes

- `browser_wasi_shim` comes from esm.sh, unpinned, because the rootfs'
  `dyld.mjs` imports it from there and the page must share that copy
  (`learn/README.md`). The app's own `index.js` uses the pinned
  `@bjorn3/browser_wasi_shim` from jsDelivr, as every example's does; the two
  do not meet.
- The app and the compiled program are two Haskell programs in one page, each
  with its own runtime. Both load `haskell-halogen-core`'s JavaScript, which
  `Halogen.JSBits.wasmJS` keeps on `globalThis` under a key made of the
  package, the module and the splice's position, so the second one loaded may
  replace the first one's functions with the same functions. That is harmless as long as those files keep no state of their
  own (only `core/jsbits/indexeddb.js` does, and the app does not use
  IndexedDB).
