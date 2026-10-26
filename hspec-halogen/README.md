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

Suites run on GHC's WebAssembly backend or its JavaScript backend. You write no
JavaScript: the `hspec-halogen` executable, used as cabal's test wrapper,
starts the browser, serves the suite to it and relays the report.

## What you need

- **A GHC for the browser**, 9.10 or newer:
  - the WebAssembly toolchain
    ([ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta)).
    The executable calls `wasm32-wasi-ghc --print-libdir` to find the
    post-linker; set `HSPEC_HALOGEN_WASM_GHC` if your wasm GHC has another
    name;
  - or the JavaScript backend (`javascript-unknown-ghcjs-ghc`).
- **A host GHC**, to build the `hspec-halogen` executable. It is a native
  program, like `hspec-discover`.
- **Node.js 24 or newer** on `PATH`, or named by `HSPEC_HALOGEN_NODE`.
  ghc-wasm-meta installs one.
- **npm packages in your project**, found from the directory the suite runs in
  upwards, as Node finds packages. You need `playwright`, plus
  `@bjorn3/browser_wasi_shim` for WebAssembly or `esbuild` for JavaScript:

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

Install the executable with the **host** compiler, not the wasm one. If your
shell has sourced `~/.ghc-wasm/env`, clear the cross-compiler variables it
exports first:

```sh
env -u CC -u CXX -u LD -u AR cabal install hspec-halogen:exe:hspec-halogen
```

## Setting up a test suite

On WebAssembly, a browser suite is a *reactor*: its entry point, `hs_start`, is
called by the page once it has loaded. Build it that way on wasm only. On the
JavaScript backend it is an ordinary test program, and needs nothing extra:

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

The library also builds natively, so the language server can load your suite.
Run natively, or run without the executable (a JavaScript-backend suite is a
Node script), `runBrowserTests` prints that it skipped the suite and succeeds.

On the JavaScript backend, the executable bundles the suite with esbuild before
serving it, so your `js-sources` may `import` npm packages. The few functions
that hspec needs there and GHC's JavaScript runtime lacks (from `unix` and
`splitmix`) come with `haskell-halogen-core`, so a suite doesn't carry its own.

### Your page's scripts and styles

The suite runs in an otherwise empty page. Whatever your components need
loaded first goes in `test/web/`, beside the suite's package:

- every `.css` and `.js` file there is loaded before the suite starts, and
  anything else there is served too;
- if `test/web/bundle.sh` exists, it runs first with an empty directory as its
  argument, and what it writes there is loaded the same way. Use it to build
  your stylesheet or bundle your JavaScript.

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

(`wasm32-wasi-cabal test --test-wrapper=…` does the same, if ghc-wasm-meta
installed that wrapper.)

hspec's options pass through as usual, e.g.
`--test-options='--match "/counter/"'`. The wrapper also runs suites that are
not browser suites, under Node, so one wrapper serves a whole project.

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

The package's own test suite doubles as a set of worked examples. Each
component in [`test/Example`](test/Example) has a spec beside it in
[`test/Test`](test/Test). They start with the counter above and a couple of
small forms, and go on to a todo list with keyed rows, a parent adding and
removing child components, and components that load in a forked action or tick
from a subscription. `Reconcile` checks what a render keeps, patches and
replaces in the page.

Import `Test.Hspec.Halogen` instead of `Test.Hspec`. It re-exports `Spec`,
`describe`, `it` and the other names a spec is built from, and has its own
expectations under hspec's names.

Each test is a `PageM s` action, run in a page of its own by `runPage`:

```haskell
it "…" $ runPage $ do
  ui <- mount m MyComponent.component input
  …
```

`runPage` gives the test a container at the end of the body (or of the
element `runBrowserTestsIn` was given). When the test ends, pass or fail, it
unmounts everything mounted there.

`runPage` is to `PageM` what `runST` is to `ST`. The `s` in `Mounted s …` and
`Element s` keeps what a test mounted and found from being used outside it.
`PageM` has no `MonadIO`, so a test cannot open a page inside its own. For
other `IO`, there is `unsafeIOToPageM`, unsafe in the same way as
`unsafeIOToSTM`.

### Mounting and talking to the component

- `mount m component input`: mount into the page. The component has
  rendered and run its initialisers up to their first wait when this
  returns; what they load may still be on its way, which `shouldHaveText`
  and `eventually` wait for.
- `htmlContainer "<p>…</p>"` and `hydrate m container component input`: a
  container holding HTML the browser parsed, as a prerendered page holds a
  component's, and the component mounted into it with `hydrateUI`, adopting
  that HTML. Find elements in the container with `findIn` before hydrating,
  and `shouldBeSameElement` tells which of them the component kept.
- `unmount ui`: take it down before the test ends, running its finalisers.
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
- `dragTo source target` drags one element onto another with Playwright's
  mouse: the page sees pointer and mouse events on the way, and Chromium's
  HTML5 `dragstart`, `dragover` and `drop`, so it tests either kind of drag.
  A pointer drag should not select text (`user-select: none`), or the next
  press on the selection drags the text and cancels the pointer, as it would
  for a user.
- `press "Enter"` presses a key on the focused element. Key names are
  Playwright's (`Backspace`, `ArrowDown`, `Shift+Tab`, …).
- `focus` and `blur`.

Every action returns once the component has reacted, so a synchronous
`handleAction` has rendered by then.

### Without the runner

A suite can also run in a page with no Playwright behind it: one opened by
hand, or one that compiles and runs a suite itself, as the haskell-halogen
tutorial does with the GHC it runs in the page. There the page performs the
actions itself, with synthetic events (`jsbits/page.js`, its in-page mode):

- `click`: `pointerdown`, `mousedown`, the focus a press gives (unless
  `mousedown`'s default was prevented: to the element or the focusable one
  around it, or, pressing plain text, away from whatever had it),
  `pointerup`, `mouseup`, then the
  element's `click()`, which dispatches `click` with its default action (a
  checkbox toggles, a link is followed, a form's button submits it).
- `typeText`: the element focused, then for each character `keydown`,
  `keypress`, `beforeinput`, the field edited, `input` and `keyup`, after
  what the field holds (an element edited in place, `contenteditable`, is
  edited at its end with the browser's own editing commands). A number
  field keeps the text typed so far apart from its value, as a browser
  does, so `-42` arrives whole although its value is empty after the `-`.
  `clear` selects everything and deletes it, with `beforeinput` and
  `input`; as Playwright's, it fails at once on an element that is not a
  text field or edited in place, and waits for a read-only one to become
  editable.
- `press`: the modifiers' and the key's `keydown` and `keyup` on the focused
  element, and when `keydown`'s default is not prevented (when it is,
  nothing more, not even the `Space` that would press a button), the
  commonest of what the browser would do: a character typed into a text field,
  `Backspace` and `Delete` (a whole character as the reader sees it, a
  grapheme cluster such as an emoji, never half of one), `Enter` on a
  button or a link (a click), in a text input of a form (HTML's implicit
  submission: the form's first submit button is clicked, so its handlers
  run, and nothing happens when it is disabled; a form with no submit
  button is submitted only when it has a single text-like field) and in a
  text area, `Space` on a button or a checkbox, and `Tab` and `Shift+Tab`
  moving the focus in a browser's order (positive `tabindex` first, by it,
  then the rest in document order, past disabled, hidden and
  `tabindex="-1"` elements). Key names are Playwright's, `Space`, `KeyA`
  and `Digit1` included. Any other key, and a printable one on an element
  that is not a field, only sends its events: the arrow keys do not move
  the caret or change a `select`'s choice, which is not supported.
- `dragTo`: pointer events from one element to the other, then HTML5 drag
  and drop's, sharing one `DataTransfer`.

What a page's handlers decide is honoured as a browser honours it: a
`keydown` whose default is prevented sends nothing more (no `keypress`,
no edit, no activation on the `keyup`); a prevented `keypress` keeps the
character out and Enter from acting; a prevented `beforeinput` leaves the
field as it was. So is what the field says of itself: a `readOnly` input or
text area takes the key events (and the focus and the selection) but is not
edited, and `maxlength` caps what is inserted, not what is deleted, in
UTF-16 code units as Chromium counts it, leaving out a whole character
that does not fit (an emoji goes into no field with room for one unit).

Each waits up to five seconds for its element to be in the page, visible and
enabled, as Playwright does, and fails the test if it is not, with
Playwright's "Timeout 5000ms exceeded" in the message (a `dragTo` waits so
for both elements). Visible is Playwright's sense: an element with a box,
not `display: none` or `visibility: hidden` itself or by what holds it;
opacity is not looked at. A disabled element is not enabled, and cannot be
focused for `press` either. It does not
check that nothing covers the element. The events are untrusted
(`isTrusted` is false), which Halogen's handlers do not look at, and what
else the browser does by itself (a caret moved with the arrow keys, text
selected with the mouse, the focus a label gives) does not happen. So a
suite that passes here passes under the runner, as a rule, but not
necessarily the other way round: the runner is the one to trust.

`runBrowserTestsIn element spec` runs a suite with every test's page inside
`element` rather than at the end of the body, for a page that has other
things on it; the report is printed as ever, and there is no runner to tell
how it went.

### Reading and checking

- `textContent`, `getProperty element "value"` (as JavaScript's `String()`
  renders it), `getAttribute`, `classes`, `outerHTML` and `isVisible`, which
  is visible in Playwright's sense above, the same the actions wait for.
- `isAttached`: whether the element is still in the page. Elements compare
  with `==` as JavaScript's `===` does, and `shouldBeSameElement` checks that
  a render kept an element rather than replacing it.
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

### What the tree did

While a test runs, its page keeps the last 50 events of core's trace
(`Halogen.Trace`): what the components it mounted did, from their mounting
on. A test that fails prints them after its own message, oldest first, with
the time since the test started, the component's label (its state's type,
or what `H.named` called it) and id, and what happened:

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

So a failure says whether the click arrived, whether the state changed and
whether the change rendered, not only what was expected. A failure of the
`shouldBe` kind prints the section before what was expected and what came.
Nothing is kept for a passing test, and nothing at all while no test runs.

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

A suite can also run in browser GHCi. This works only on WebAssembly, since
browser GHCi exists only there. The code runs in a page, so your
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
