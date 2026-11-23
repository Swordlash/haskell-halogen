# hspec-halogen

Test [haskell-halogen](https://github.com/Swordlash/haskell-halogen) components in a real browser, with hspec.

A test mounts a component in Chromium, finds elements by CSS selector, interacts with them and checks the result. The suite runs inside the page, so it can also query the component and inspect its outputs. [Playwright](https://playwright.dev) supplies real clicks and keystrokes after checking that the target is visible, enabled and unobstructed.

```haskell
spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = describe "counter" $
  it "counts clicks" $ runPage $ do
    ui <- mount m Counter.component ()
    find ui "button.increment" >>= click
    find ui ".count" >>= (`shouldHaveText` "1")
    query ui (H.mkRequest Counter.GetCount) `shouldReturn` Just 1
```

Use [haskell-halogen-playwright](../playwright/) for tests that run outside the page: built applications, navigation, multiple tabs or remotely rendered components. It runs as a native hspec suite and waits for the page to become idle after each action. Use this package for focused component tests that need direct access to queries and outputs.

Suites support both GHC browser backends: WebAssembly and JavaScript. The native `hspec-halogen` executable acts as Cabal's test wrapper, starts Chromium, serves the suite and relays the test report. You do not need to write JavaScript.

## What you need

- **A GHC for the browser**, 9.10 or newer:
  - the WebAssembly toolchain ([ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta)). The executable calls `wasm32-wasi-ghc --print-libdir` to find the post-linker; set `HSPEC_HALOGEN_WASM_GHC` if your wasm GHC has another name;
  - or the JavaScript backend (`javascript-unknown-ghcjs-ghc`).
- **A host GHC**, to build the `hspec-halogen` executable. It is a native program, like `hspec-discover`.
- **Node.js 24 or newer** on `PATH`, or named by `HSPEC_HALOGEN_NODE`. ghc-wasm-meta installs one.
- **npm packages in your project**, found from the directory the suite runs in upwards, as Node finds packages. You need `playwright`, plus `@bjorn3/browser_wasi_shim` for WebAssembly or `esbuild` for JavaScript:

  ```sh
  npm install --save-dev playwright @bjorn3/browser_wasi_shim   # WebAssembly
  npm install --save-dev playwright esbuild                     # JavaScript
  ```

- **Chromium for Playwright:**

  ```sh
  npx playwright install chromium
  # in CI, with the system libraries it needs:
  npx playwright install --with-deps --only-shell chromium
  ```

Install the executable with the **host** compiler, not the wasm one. If your shell has sourced `~/.ghc-wasm/env`, clear the cross-compiler variables it exports first:

```sh
env -u CC -u CXX -u LD -u AR cabal install hspec-halogen:exe:hspec-halogen
```

## Setting up a test suite

A WebAssembly browser suite must be built as a reactor with an exported `hs_start` entry point. The page calls it after loading. Apply these options only on wasm; JavaScript-backend suites are ordinary test programs:

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

`main` calls `runBrowserTests` and chooses the component monad. Specs remain polymorphic:

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

Import `BrowserDOM` without its constructor, as above. `spec BrowserDOM` then passes the type, not the constructor.

The library also builds natively so a language server can check your suite. Native runs report a skip and succeed. The same happens if a JavaScript suite runs under Node without the browser runner.

For JavaScript builds, the executable bundles the suite with esbuild before serving it, allowing `js-sources` to import npm packages. Core supplies the `unix` and `splitmix` functions that hspec needs but GHC's JavaScript runtime lacks.

### Your page's scripts and styles

Tests start in an otherwise empty page. Put any required scripts, styles and assets in the package's `test/web/` directory:

- every `.css` and `.js` file there is loaded before the suite starts, and anything else there is served too;
- if `test/web/bundle.sh` exists, it runs first with an empty directory as its argument, and what it writes there is loaded the same way. Use it to build your stylesheet or bundle your JavaScript.

## Running

Pass the executable to cabal as the test wrapper, with the browser compiler:

```sh
cabal test browser-test \
  --with-compiler=wasm32-wasi-ghc \
  --with-hc-pkg=wasm32-wasi-ghc-pkg \
  --with-hsc2hs=wasm32-wasi-hsc2hs \
  --test-wrapper="$(command -v hspec-halogen)"

cabal test browser-test \
  --with-compiler=javascript-unknown-ghcjs-ghc \
  --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg \
  --test-wrapper="$(command -v hspec-halogen)"
```

(`wasm32-wasi-cabal test --test-wrapper=…` does the same, if ghc-wasm-meta installed that wrapper.)

hspec's options pass through as usual, e.g. `--test-options='--match "/counter/"'`. The wrapper also runs suites that are not browser suites, under Node, so one wrapper serves a whole project.

If cabal complains of a version mismatch between `ghc` and `ghc-pkg`, it has cached the host compiler from an earlier build in the same build directory. Delete `dist-newstyle/cache/compiler`, or use a build directory of its own for wasm (`--builddir=dist-newstyle/wasm`).

| Environment variable | Effect |
|---|---|
| `HSPEC_HALOGEN_HEADED=1` | show the browser while the suite runs |
| `HSPEC_HALOGEN_SLOWMO=<ms>` | wait this long before every browser action |
| `HSPEC_HALOGEN_TIMEOUT=<s>` | how long the whole suite may take (600) |
| `HSPEC_HALOGEN_DEADLINE_MS=<ms>` | how long an action, `settle` and `waitQuiet` may wait for the page (5000), see [The deadline](#the-deadline) |
| `HSPEC_HALOGEN_RETRIES=1` | report the waits that retried, on standard error, see [Finding the reads that retry](#finding-the-reads-that-retry) |
| `HSPEC_HALOGEN_JOBS=<n>` | pages for the examples marked `parallel` (1), see [Parallel tests](#parallel-tests) |
| `HSPEC_HALOGEN_WORKERS=<m>` | Web Workers for the examples marked `inWorker` (0), see [Parallel tests](#parallel-tests) |
| `HSPEC_HALOGEN_WASM_GHC` | the wasm GHC whose post-linker to use (`wasm32-wasi-ghc`) |
| `HSPEC_HALOGEN_NODE` | the Node to run with (`node`) |

## Writing tests

The package's own test suite doubles as a set of worked examples. Each component in [`test/Example`](test/Example) has a spec beside it in [`test/Test`](test/Test). They start with the counter above and a couple of small forms, and go on to a todo list with keyed rows, a parent adding and removing child components, and components that load in a forked action or tick from a subscription. `Reconcile` checks what a render keeps, patches and replaces in the page.

Import `Test.Hspec.Halogen` instead of `Test.Hspec`. It re-exports `Spec`, `describe`, `it` and the other names a spec is built from, and has its own expectations under hspec's names.

Each test is a `PageM s` action, run in a page of its own by `runPage`:

```haskell
it "…" $ runPage $ do
  ui <- mount m MyComponent.component input
  …
```

`runPage` gives the test a container at the end of the body (or of the element `runBrowserTestsIn` was given). When the test ends, pass or fail, it unmounts everything mounted there.

`runPage` is to `PageM` what `runST` is to `ST`. The `s` in `Mounted s …` and `Element s` keeps what a test mounted and found from being used outside it. `PageM` has no `MonadIO`, so a test cannot open a page inside its own. For other `IO`, there is `unsafeIOToPageM`, unsafe in the same way as `unsafeIOToSTM`.

### Mounting and talking to the component

- `mount m component input` renders the component and runs initialization up to its first wait. Use `waitQuiet` if the assertion depends on asynchronous initialization.
- `htmlContainer "<p>…</p>"` creates a container from parsed HTML. `hydrate m container component input` adopts it with `hydrateUI`. Find nodes before hydration with `findIn`, then use `shouldBeSameElement` to verify that they were retained.
- `unmount ui` disposes the component and runs finalizers before the test ends.
- `query ui q` sends a component query.
- `outputs ui` returns all component outputs, oldest first.

### Finding elements

- `find ui "selector"`: the first match in the component. It waits for one to appear, and fails with the component's markup if none does.
- `findAll ui "selector"`: every match, as the page is now.
- `findIn element "selector"` and `findAllIn element "selector"`: the same, within one element.

### Acting

- `click`, `typeText element "text"` and `clear` go through Playwright: trusted events, to an element it has checked it can reach.
- `dragTo source target` drags one element onto another with Playwright's mouse: the page sees pointer and mouse events on the way, and Chromium's HTML5 `dragstart`, `dragover` and `drop`, so it tests either kind of drag. A pointer drag should not select text (`user-select: none`), or the next press on the selection drags the text and cancels the pointer, as it would for a user.
- `press "Enter"` presses a key on the focused element. Key names are Playwright's (`Backspace`, `ArrowDown`, `Shift+Tab`, …).
- `focus` and `blur`.

Actions return after `settle` confirms that the program has finished reacting. Synchronous handlers have rendered by then, including handlers for browser events triggered shortly after the original action.

### Waiting for the program

The suite and its components share one program. `Halogen.Activity` reports whether any component tree is busy. Every action calls `settle`, which repeats the following sequence until the program remains idle:

1. Wait for a background-priority browser task, allowing WebAssembly event handlers to start.
2. Wait until no component tree is running or has queued work.
3. Wait for pending history navigation requested by a handler, such as `history.back()`, `forward()` or `go(n)`. This waits up to half a second for `popstate`; navigation with no destination is skipped.
4. Wait for the next animation frame and a task after it. Events such as dialog `close`, `resize` and `ResizeObserver` callbacks can arrive here. Hidden pages are skipped. If a frame takes more than 100 ms, later attempts wait at most 20 ms until frames resume.
5. Check the program again and repeat if those events made it busy.

After an action, one read is enough to inspect its rendered result. Call `settle` explicitly after custom actions, such as an `unsafeIOToPageM` operation that dispatches an event. Settling usually costs one frame, about 17 ms at 60 Hz. It waits through browser promises, allowing the page to continue running. If the program is still busy at the deadline, five seconds unless set otherwise (see below), the test fails with activity counts and a trace.

`settle` does not wait for fibers suspended in `H.async`, such as a request or a counted debounce. Use these APIs when the result depends on them:

- `waitQuiet` settles the page, waits until no fibers remain in `H.async`, then settles again. It has the same deadline as `settle`; `waitQuietWithin ms` gives one of its own. Failures report running trees and waiting fibers.
- `H.delay` and `Hooks.delay` are excluded intentionally. Use them for transient notices and repeating timers. Use `H.delayCounted` for delays before observable work, such as debounced requests or autosaves. `H.async (threadDelay n)` also counts, but a test clock cannot advance it.
- Under core's test clock (`withTestClock` of `Halogen.Activity.Control`, around `runPage`), both kinds of pause sleep until the test advances the clock. A counted pause asleep there is not waited for: the program is quiet with it, and the test advances the clock by the pauses whose work it wants (`perform (advance clock d)`), then waits. A test that forgot reads the page before that work, and the read's failure names each pause asleep on the clock: its component, its length, and when it is due.
- `activity` returns the raw reading: trees, running trees, waiting fibers, and those of them asleep on a test's clock. Import its fields from `Halogen.Activity (Activity (..))`.

These are `haskell-halogen-playwright`'s names for the same waits, so a test reads the same on either harness.

```haskell
find ui "button.save" >>= click
waitQuiet
find ui ".status" >>= textContent >>= (`shouldBe` "Saved")
```

#### Reading once

`find` and the `should…` helpers retry for two seconds, so a read can pass only because it caught the program a moment later, and fail on a machine slower than the one it was written on. A group of examples can instead have its reads look once, after the program has come to rest:

```haskell
describe "the editor" $ readsOnce Idle $ do
  it "saves with Ctrl+S" $ runPage $ do
    ui <- mount m Editor.component ()        -- mount settles
    press "Control+s"
    waitQuiet                                -- the save's answer
    find ui ".notice" >>= (`shouldHaveText` "Saved")
```

- `readsOnce Idle` brings the program to `settle`'s rest before a read; `readsOnce Quiet` to `waitQuiet`'s, so the reads also wait for requests and counted pauses (but for those asleep on a test's clock).
- A read looks at a program at rest at the moment it looks. It checks with one reading of the program (no frame, no task) and settles the program only when it is not at rest, so the reads after an action that settled cost nothing, and work the program started by itself since (a push, a timer, a raw call) is waited for.
- The reads are `find`, `findIn`, `findAll`, `findAllIn`, `textContent`, `getProperty`, `getAttribute`, `classes`, `outerHTML`, `isVisible`, `isAttached`, `outputs`, `shouldBeSameElement` and the `should…` helpers on elements. An absence (`findAll` finding nothing) is read at rest too.
- A read that fails says which rest it looked at, how long reaching it took and what the program was doing, before what it found.
- `eventually` stays the explicit wait for what the program does not know of: the clipboard's answer, a notice taken away in `H.delay`, a message the server pushes. Its attempts look at the page as it is. With `HSPEC_HALOGEN_RETRIES=1` it is still reported.
- `perform io` runs the test's own call on the page and settles, so the reads after it rest again. `unsafeIOToPageM` does not: the harness cannot tell whether it did something.
- `countHeld n` tells the harness how many waits the test holds on purpose (requests its own server keeps unanswered). `waitQuiet` and `readsOnce Quiet` then take the program as quiet with that many fibers waiting. It compares counts, not the waits: a held request whose fiber was cancelled hides one other fiber's wait.
- The one rule for quiet, `quietWith held`, is: idle, and no more fibers waiting than are asleep on a test's clock and held by the test together (`waiting <= asleep + held`). Core counts the pauses asleep exactly; the test says what it holds.

Outside a `readsOnce` group the reads retry as before. `mount`, `hydrate` and `query` settle in either case; `mountUnsettled` returns at once, for a component busy from its start.

#### The deadline

One deadline bounds every wait for the page: an action's wait for its element to be visible and enabled (Playwright's timeout, or the page's own check without the runner), `settle` after it, and `waitQuiet`. It is five seconds unless the runner is given `HSPEC_HALOGEN_DEADLINE_MS`:

```sh
HSPEC_HALOGEN_DEADLINE_MS=30000 cabal test browser-test --test-wrapper="$(command -v hspec-halogen)" …
```

A part of a test can have a deadline of its own: `withDeadline 1000 settle` checks that a watchdog fires without waiting a slow machine's thirty seconds for it, and gives the runner's deadline back afterwards.

The deadline is a watchdog against a program that never finishes, not a measure of how fast it should be. A suite that passes on a developer's machine can run out of five seconds on a loaded CI runner, where the same click takes several times as long; there, raise it in one place. A test that checks the program stays busy for a while, or waits for a request known to be slow, names its own with `waitQuietWithin`. The deadline does not change `eventually`, whose two seconds are its own (`eventuallyWithin` sets another). haskell-halogen-playwright's `HALOGEN_PLAYWRIGHT_TIMEOUT` is the same setting for that harness.

#### Finding the reads that retry

`find` and the `should…` expectations retry for two seconds, so a read can pass only because it caught the program a moment later, and a loaded machine then fails it. With `HSPEC_HALOGEN_RETRIES=1`, each wait whose first attempt failed writes a tab-separated line to standard error:

```
HSPEC_HALOGEN_RETRY  <example>  <function>  <file:line>  <trees/running/waiting>  <ok|timeout|cut>  <attempts>  <ms>  <first failure>  <last failure>
```

with what the program was doing at the first failure (`Halogen.Activity`): a tree still running means an action or a mount had not settled, a fiber waiting means the read wanted `waitQuiet`. A `settle` or `waitQuiet` that took longer than a second, or ran out of time, writes an `HSPEC_HALOGEN_SETTLE` line, and the suite ends with an `HSPEC_HALOGEN_READS` line for every place that waited: how many of its waits succeeded at once and how many retried. A wait inside another (a `find` in an `eventually`) reports itself at its own line; one cut short by the enclosing wait's deadline says `cut`, and the enclosing wait reports only its own attempts, so no retry is counted twice. Without the variable nothing is written.

### Without the runner

Suites can also run in pages without Playwright, including the tutorial's in-browser compiler. In this mode, `jsbits/page.js` performs actions with synthetic events:

- `click` dispatches pointer-down and mouse-down events, applies focus unless prevented, then dispatches pointer-up and mouse-up and calls the element's `click()`. Native click defaults can toggle a checkbox, follow a link or submit a form.
- `typeText` focuses the element and sends keydown, keypress, beforeinput, input and keyup for each character, editing between beforeinput and input. Contenteditable text is inserted at the end using browser editing commands. Number inputs track typed text separately so intermediate values such as the minus sign in `-42` are preserved. `clear` selects and deletes contents with beforeinput/input events. It rejects non-editable element types and waits for read-only fields to become editable.
- `press` sends key and modifier events to the focused element. When not prevented, it handles character insertion, grapheme-aware Backspace/Delete, Enter, Space and Tab. Enter activates buttons and links, inserts a newline in textareas, and performs form implicit submission: it clicks the first submit button unless disabled, or submits a buttonless form only if it has one text-like field. Tab follows positive tabindex order, then document order, skipping disabled, hidden and negative-tabindex targets. Key names follow Playwright, including `Space`, `KeyA` and `Digit1`. Other keys only dispatch events; arrow-key caret movement and select changes are unsupported.
- `dragTo` sends pointer events followed by HTML5 drag events, sharing one `DataTransfer`.

Synthetic input respects event cancellation. Preventing `keydown` stops later keypress, editing and activation behaviour. Preventing `keypress` suppresses character insertion and Enter's default action. Preventing `beforeinput` leaves the field unchanged.

Read-only inputs still receive key events, focus and selection, but their contents do not change. `maxlength` limits insertion using Chromium's UTF-16 code-unit count. A character that does not fit is omitted completely, so an emoji is not split to fill one remaining unit.

Each synthetic action waits up to [the deadline](#the-deadline) for an attached, visible and enabled target. `dragTo` checks both elements. Timeout failures name it, as Playwright's do: `Timeout 5000ms exceeded`. Visibility follows Playwright's definition: the element needs a layout box and must not be hidden by `display` or `visibility`; opacity does not matter. Covered elements are not detected, and disabled elements cannot receive focus for `press`.

These events have `isTrusted = false`. Synthetic actions do not reproduce every browser behaviour, such as arrow-key caret movement, mouse text selection or label-driven focus. Passing here does not replace testing under the real runner.

`runBrowserTestsIn element spec` places each test container inside the supplied element instead of at the end of the body. This is useful on pages with other content. The suite still prints its report, but no external runner receives the result.

### Reading and checking

- `textContent`, `getProperty element "value"` (as JavaScript's `String()` renders it), `getAttribute`, `classes`, `outerHTML` and `isVisible`, which is visible in Playwright's sense above, the same the actions wait for.
- `isAttached`: whether the element is still in the page. Elements compare with `==` as JavaScript's `===` does, and `shouldBeSameElement` checks that a render kept an element rather than replacing it.
- `shouldBe`, `shouldNotBe`, `shouldSatisfy`, `shouldContain`, `shouldReturn` and `expectationFailure` work as hspec's do, in `PageM`. A failure points at the test's line.
- `shouldHaveClass`, `shouldNotHaveClass`, `shouldHaveText`, `shouldBeVisible` and `shouldBeHidden` retry for up to two seconds before they fail.
- A pattern that does not match, such as `[a, b] <- findAll ui "li"` finding three, fails the test.
- `eventually` gives any expectation the same retrying, for what the page does that the program does not know of: a JavaScript widget's own timer, a CSS transition, a real connection's events. What the program does in response to an action is `settle`'s and `waitQuiet`'s, which need no retrying.

  ```haskell
  eventually $ getProperty input "value" `shouldReturn` "saved"
  ```

### What the tree did

Each test retains the last 50 `Halogen.Trace` events from its mounted components. On failure, it prints them in chronological order with elapsed time, component label and ID, and the event description:

```
expected text "2", found "1"

What the tree did (the last 50 events, oldest first):
    0.4 ms  Counter#7  mounted
    0.5 ms  Counter#7  rendered in 0.1 ms
    0.5 ms  Counter#7  initialized
   12.3 ms  Counter#7  action
   12.9 ms  Counter#7  state changed
   13.0 ms  Counter#7  rendered in 0.4 ms
```

The trace helps distinguish a missing click, an unchanged state and a missing render. For `shouldBe` failures it appears before the expected and actual values. Passing tests discard their trace, and no events are retained outside tests.

### Parallel tests

Mark independent examples with hspec's `parallel` and set the number of browser contexts in the runner:

```haskell
spec m = parallel $ do
  it "counts clicks" $ runPage $ do
    ui <- mount m Counter.component ()
    find ui "button.increment" >>= click
    find ui ".count" >>= (`shouldHaveText` "1")
```

```sh
HSPEC_HALOGEN_JOBS=2 npm run test-wasm
```

The default is one context and preserves the usual single-page run. With multiple contexts, unmarked and `sequential` examples finish first on one page. The runner then starts the parallel examples on separate pages, each in its own browser context, with its own focus, keyboard, mouse, storage and Haskell heap. Within each page, `runPage` still admits one example at a time. Web Workers cannot provide this isolation because they have no DOM, but they can run the examples that need none; see below.

The spec must enumerate the same examples in each context. Every example is then assigned to exactly one page. Normal hspec filters and focused examples still apply. Each context runs its own hooks and has its own in-memory fixtures: a `beforeAll` value is not shared between pages. External servers and files remain shared; their fixtures must support concurrent tests. Browser GHCi and suites run without the executable keep running one page at a time.

With multiple contexts, each page's report is buffered and printed together so concurrent reports do not interleave. A failure, page exception, missing result or timeout in any context fails the command. More contexts load more copies of the suite; start with two and measure before increasing the count.

#### Examples that need no page

Mark tests that never call `runPage` with `inWorker`. These can run in Web Workers alongside the page tests, using separate threads and heaps. Suitable examples include pure checks, parsers and computations:

```haskell
spec m = do
  inWorker $ describe "the price model" $ do
    it "reads a price" $ Price.parse "12.35" `Hspec.shouldBe` Just (Price 1235)
    it "totals a large basket" $ Price.total (replicate 100000 (Price 5)) `Hspec.shouldBe` Price 500000
  parallel $ it "shows the total" $ runPage $ do
    ui <- mount m Basket.component ()
    find ui ".total" >>= (`shouldHaveText` "12.35")
```

Such an example is plain `IO`, so its expectations are those of `Test.Hspec` (imported qualified, as `Hspec` here, beside this module's).

```sh
HSPEC_HALOGEN_JOBS=2 HSPEC_HALOGEN_WORKERS=2 npm run test-wasm
```

`HSPEC_HALOGEN_WORKERS=<m>` starts `m` workers in a separate browser context after unmarked tests finish, alongside the parallel pages. The default is 0. Tests marked `inWorker` are divided among workers even without `parallel`, and are excluded from page shards.

Each worker prints its own report as `Browser shard worker/<i>/<m>`. Failures, uncaught exceptions, missing results and timeouts fail the command. When workers are disabled, including in browser GHCi, native runs and pages without a runner, `inWorker` leaves execution unchanged.

Calling `runPage` inside a worker fails the test with `this example runs in a worker, which has no page: take it out of inWorker`. This catches incorrectly marked tests instead of silently skipping them.

Workers support Haskell threads, `threadDelay`, suite hooks and browser APIs available in worker scope, including fetch, timers and promises. Hooks run separately in each worker. The runner does not load `test/web/` scripts or styles into workers.

Each worker is one more copy of the suite in memory, in the same browser as the pages: count them with the pages when choosing the numbers.

### Your own monad

Components can use any monad with a `MonadBrowserTest` instance; `BrowserDOM` already has one. A custom monad needs the classes required by `runUI`: `MonadUnliftIO`, `MonadFork`, `MonadKill`, `MonadParallel`, `MonadMask` and `MonadUUID`. Implement `runTest` to run it. The default `mountInto` works with `MonadBrowserDOM`:

```haskell
instance MonadBrowserTest App where
  runTest = runApp testEnv
```

Then pass the type to your spec as `spec App`.

## Rerunning on every save

A suite can also run in browser GHCi. This works only on WebAssembly, since browser GHCi exists only there. The code runs in a page, so your components render in a window you can watch and open devtools on, and [ghciwatch](https://github.com/MercuryTechnologies/ghciwatch) reruns the suite whenever you save.

1. Give the suite a flag that drops the reactor options and the export, since GHCi loads it as ordinary modules:

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

2. Make a page for GHCi to boot. It is an `index.html` in a directory of its own, loading your `test/web` scripts and styles and then GHCi's script:

   ```html
   <link rel="stylesheet" href="./app.css">
   <script src="./app.js"></script>
   <script type="module" src="../main.js"></script>
   ```

3. Start GHCi with the browser interpreter, giving it that directory as an absolute path. cabal starts GHCi in the package's directory:

   ```sh
   cabal repl browser-test --enable-shared --constraint='my-package +interactive' \
     --with-compiler=wasm32-wasi-ghc --with-hc-pkg=wasm32-wasi-ghc-pkg \
     --repl-options="-fghci-browser -fghci-browser-port=8080 -fghci-browser-assets-dir=$PWD/page"
   ```

4. Open the page with the executable, not your desktop browser, so the suite's clicks and keystrokes are real input. The executable also prints the suite's report, which browser GHCi sends to the page's console:

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
