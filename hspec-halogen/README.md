# hspec-halogen

Test [haskell-halogen](https://github.com/Swordlash/haskell-halogen)
components in a real browser, with hspec.

A test mounts a component into a page in Chromium, finds its elements by CSS
selector, clicks and types into them as a user would, and checks what it reads
back. The test suite runs inside the page, so a test holds the component
itself: it can query it and see what it raised, not only the markup it
rendered. Clicks and keystrokes are real input, performed by
[Playwright](https://playwright.dev) on elements it has checked are visible,
enabled and not covered.

```haskell
spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = describe "counter" $
  it "counts clicks" $ runPage $ do
    ui <- mount m Counter.component ()
    find ui "button.increment" >>= click
    find ui ".count" >>= (`shouldHaveText` "1")
    query ui (H.mkRequest Counter.GetCount) `shouldReturn` Just 1
```

Suites run on GHC's WebAssembly backend. You write no JavaScript: the
`hspec-halogen` executable, used as cabal's test wrapper, starts the browser,
serves the suite to it and relays the report.

## What you need

- **The GHC WebAssembly toolchain**
  ([ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta)),
  9.10 or newer. The executable calls `wasm32-wasi-ghc --print-libdir` to find
  the post-linker. Set `HSPEC_HALOGEN_WASM_GHC` if your wasm GHC has another
  name.
- **A host GHC**, to build the `hspec-halogen` executable. It is a native
  program, like `hspec-discover`.
- **Node.js 24 or newer** on `PATH`, or named by `HSPEC_HALOGEN_NODE`.
  ghc-wasm-meta installs one.
- **Two npm packages in your project**, found from the directory the suite
  runs in upwards, as Node finds packages:

  ```sh
  npm install --save-dev playwright @bjorn3/browser_wasi_shim
  ```

- **Chromium for Playwright:**

  ```sh
  npx playwright install chromium
  # in CI, with the system libraries it needs:
  npx playwright install --with-deps --only-shell chromium
  ```

Install the executable with the **host** compiler, not the wasm one. If your
shell has sourced `~/.ghc-wasm/env`, clear the cross-compiler variables it
exports first:

```sh
env -u CC -u CXX -u LD -u AR cabal install hspec-halogen:exe:hspec-halogen
```

## Setting up a test suite

A browser suite is a WebAssembly *reactor*: its entry point, `hs_start`, is
called by the page once it has loaded. Build it that way on wasm only:

```cabal
test-suite browser-test
    type:             exitcode-stdio-1.0
    hs-source-dirs:   test
    main-is:          Test.hs
    build-depends:    base, hspec, hspec-halogen, haskell-halogen-core, my-package

    if arch(wasm32)
        ghc-options:
            -no-hs-main
            -optl-mexec-model=reactor
            "-optl-Wl,--export=hs_start"
```

`main` runs the specs with `runBrowserTests` and chooses the monad the
components run in. The specs themselves name none:

```haskell
{-# LANGUAGE CPP #-}
module Main (main) where

import Halogen.VDom.DOM.Monad (BrowserDOM)
import Test.Hspec.Halogen (runBrowserTests)
import Test.Counter qualified

main :: IO ()
main = runBrowserTests (Test.Counter.spec BrowserDOM)

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif
```

Import `BrowserDOM` without its constructor, as above. `spec BrowserDOM` then
passes the type, not the constructor.

The library also builds natively and on GHC's JavaScript backend, so the
language server can load your suite. Run there, `runBrowserTests` prints that
it skipped the suite and succeeds. Only the WebAssembly backend runs it.

### Your page's scripts and styles

The suite runs in an otherwise empty page. Whatever your components need
loaded first goes in `test/web/`, beside the suite's package:

- every `.css` and `.js` file there is loaded before the suite starts, and
  anything else there is served too;
- if `test/web/bundle.sh` exists, it runs first with an empty directory as its
  argument, and what it writes there is loaded the same way. Use it to build
  your stylesheet or bundle your JavaScript.

## Running

Pass the executable to cabal as the test wrapper, with the wasm compiler:

```sh
cabal test browser-test \
  --with-compiler=wasm32-wasi-ghc \
  --with-hc-pkg=wasm32-wasi-ghc-pkg \
  --with-hsc2hs=wasm32-wasi-hsc2hs \
  --test-wrapper="$(command -v hspec-halogen)"
```

(`wasm32-wasi-cabal test --test-wrapper=…` does the same, if ghc-wasm-meta
installed that wrapper.)

hspec's options pass through as usual, e.g.
`--test-options='--match "/counter/"'`. The wrapper also runs wasm suites that
are not browser suites, under Node, so one wrapper serves a whole project.

If cabal complains of a version mismatch between `ghc` and `ghc-pkg`, it has
cached the host compiler from an earlier build in the same build directory.
Delete `dist-newstyle/cache/compiler`, or use a build directory of its own for
wasm (`--builddir=dist-newstyle/wasm`).

| Environment variable | Effect |
|---|---|
| `HSPEC_HALOGEN_HEADED=1` | show the browser while the suite runs |
| `HSPEC_HALOGEN_SLOWMO=<ms>` | wait this long before every browser action |
| `HSPEC_HALOGEN_TIMEOUT=<s>` | how long the whole suite may take (600) |
| `HSPEC_HALOGEN_WASM_GHC` | the wasm GHC whose post-linker to use (`wasm32-wasi-ghc`) |
| `HSPEC_HALOGEN_NODE` | the Node to run with (`node`) |

## Writing tests

Import `Test.Hspec.Halogen` instead of `Test.Hspec`. It re-exports `Spec`,
`describe`, `it` and the other names a spec is built from, and has its own
expectations under hspec's names.

Each test is a `PageM s` action, run in a page of its own by `runPage`:

```haskell
it "…" $ runPage $ do
  ui <- mount m MyComponent.component input
  …
```

`runPage` gives the test a container at the end of the body. When the test
ends, pass or fail, it unmounts everything mounted there.

`runPage` is to `PageM` what `runST` is to `ST`. The `s` in `Mounted s …` and
`Element s` keeps what a test mounted and found from being used outside it.
`PageM` has no `MonadIO`, so a test cannot open a page inside its own. For
other `IO`, there is `unsafeIOToPageM`, unsafe in the same way as
`unsafeIOToSTM`.

### Mounting and talking to the component

- `mount m component input`: mount into the page. The component has
  rendered and run its initialisers when this returns.
- `query ui q`: send a query, as a parent would.
- `outputs ui`: everything the component has raised, oldest first.

### Finding elements

- `find ui "selector"`: the first match in the component. It waits for one to
  appear, and fails with the component's markup if none does.
- `findAll ui "selector"`: every match, as the page is now.
- `findIn element "selector"` and `findAllIn element "selector"`: the same,
  within one element.

### Acting

- `click`, `typeText element "text"` and `clear` go through Playwright:
  trusted events, to an element it has checked it can reach.
- `press "Enter"` presses a key on the focused element. Key names are
  Playwright's (`Backspace`, `ArrowDown`, `Shift+Tab`, …).
- `focus` and `blur`.

Every action returns once the component has reacted, so a synchronous
`handleAction` has rendered by then.

### Reading and checking

- `textContent`, `getProperty element "value"` (as JavaScript's `String()`
  renders it), `getAttribute`, `classes`, `outerHTML` and `isVisible`.
- `shouldBe`, `shouldNotBe`, `shouldSatisfy`, `shouldContain`, `shouldReturn`
  and `expectationFailure` work as hspec's do, in `PageM`. A failure points at
  the test's line.
- `shouldHaveClass`, `shouldNotHaveClass`, `shouldHaveText`, `shouldBeVisible`
  and `shouldBeHidden` retry for up to two seconds before they fail.
- A pattern that does not match, such as `[a, b] <- findAll ui "li"` finding
  three, fails the test.
- `eventually` gives any expectation the same retrying, for what finishes
  later: a forked effect, a timer, work a JavaScript widget defers to the next
  frame.

  ```haskell
  eventually $ getProperty input "value" `shouldReturn` "saved"
  ```

### Parallel tests

A page has one mouse, one keyboard and one focused element, so pages open one
at a time. Tests marked `parallel` still run in turn, and cannot type into
each other's inputs.

### Your own monad

Components are mounted in any monad with a `MonadBrowserTest` instance.
`BrowserDOM` has one. Your application's monad needs the instances `runUI`
asks for (`MonadUnliftIO`, `MonadFork`, `MonadKill`, `MonadParallel`,
`MonadMask`, `MonadUUID`), and then only has to say how to run it. The default
`mountInto` works for any monad with the browser's DOM (`MonadBrowserDOM`):

```haskell
instance MonadBrowserTest App where
  runTest = runApp testEnv
```

and pass it as `spec App`.

## Rerunning on every save

A suite can also run in browser GHCi. The code runs in a page, so your
components render in a window you can watch and open devtools on, and
[ghciwatch](https://github.com/MercuryTechnologies/ghciwatch) reruns the suite
whenever you save.

1. Give the suite a flag that drops the reactor options and the export, since
   GHCi loads it as ordinary modules:

   ```cabal
   flag interactive
       default: False
       manual:  True

   test-suite browser-test
       …
       if arch(wasm32) && !flag(interactive)
           ghc-options: -no-hs-main -optl-mexec-model=reactor "-optl-Wl,--export=hs_start"
       if flag(interactive)
           cpp-options: -DINTERACTIVE
   ```

   and guard the export with `#if defined(wasm32_HOST_ARCH) && !defined(INTERACTIVE)`.

2. Make a page for GHCi to boot. It is an `index.html` in a directory of its
   own, loading your `test/web` scripts and styles and then GHCi's script:

   ```html
   <link rel="stylesheet" href="./app.css">
   <script src="./app.js"></script>
   <script type="module" src="../main.js"></script>
   ```

3. Start GHCi with the browser interpreter, giving it that directory as an
   absolute path. cabal starts GHCi in the package's directory:

   ```sh
   cabal repl browser-test --enable-shared --constraint='my-package +interactive' \
     --with-compiler=wasm32-wasi-ghc --with-hc-pkg=wasm32-wasi-ghc-pkg \
     --repl-options="-fghci-browser -fghci-browser-port=8080 -fghci-browser-assets-dir=$PWD/page"
   ```

4. Open the page with the executable, not your desktop browser, so the suite's
   clicks and keystrokes are real input. The executable also prints the suite's
   report, which browser GHCi sends to the page's console:

   ```sh
   hspec-halogen open http://127.0.0.1:8080/assets/index.html
   ```

   Then type `:main`, or `:main --match "/counter/"`, in GHCi. Under ghciwatch,
   run `:main` after every reload:

   ```sh
   ghciwatch --command "cabal repl …" --watch test --watch src \
     --after-startup-ghci ':main' --after-reload-ghci ':main'
   ```

   When GHCi restarts, the window reloads itself and reconnects.

   `HSPEC_HALOGEN_HEADLESS=1` opens no window, and `HSPEC_HALOGEN_SLOWMO` works
   here too.

haskell-halogen's own `toolchain/dev-test.sh` puts all of this together.
