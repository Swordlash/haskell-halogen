# Revision history for hspec-halogen

## Unreleased

* The package's own suite has a `Promised` example: work started in
  `H.liftEffect`, which returns a `Promise` (haskell-halogen-jsbits'
  `Halogen.JSBits.Promise`) kept in the state and awaited by a later
  action, and tests that the page keeps rendering clicks while it waits and
  that a rejection is shown as a value.

* The package's own suite has an `Islands` example: a routing shell over a
  page whose islands wait seconds in their `Initialize`, and tests that the
  components around them go on meanwhile (outputs, queries, links and the
  page that replaces them), which they do since core stopped holding a
  component's ancestors for its `Initialize`. A component `mount` returns
  has run its `Initialize` up to its first wait, no longer to its end, so a
  check of what it loads when it starts should wait for it, as
  `shouldHaveText` does.

* The package's own suite has a `Busy` example: links clicked while their
  tree's loop is busy rendering, kept from being followed by
  `Halogen.HTML.Events.preventingDefault` and `Halogen.Router`'s links, and
  followed when the action prevents the default itself. Its test suite
  depends on `haskell-halogen-router`.

* `htmlContainer html` puts a container holding HTML the browser parsed on
  the page, as a prerendered page holds a component's, and `hydrate m
  container component input` mounts the component into it with core's
  `hydrateUI`, which adopts that HTML. Find elements in the container with
  `findIn` before hydrating, and `shouldBeSameElement` tells which of them
  the component kept. `MonadBrowserTest` has a `hydrateInto` method beside
  `mountInto`, defaulting to `hydrateUI` as `mountInto` defaults to
  `runUI`; an instance that defined `mountInto` itself defines
  `hydrateInto` too.

* `MonadBrowserTest` asks only for `MonadUnliftIO`, as `runUI` now does; a
  spec whose components make ref labels says `MonadUUID m` itself, as the
  package's own `Dispatch` and `Family` specs do.

* The page functions are one module for both browser backends, without CPP;
  natively they do nothing rather than panic (no suite reaches them there).

* `dragTo source target` drags one element onto another through Playwright's
  mouse, which fires pointer, mouse and HTML5 drag and drop events alike.
  Without the runner it dispatches the same events itself.

* `eventuallyWithin` (and so `eventually`, `find` and everything that waits)
  keeps its timeout as a deadline on the clock: an attempt still running when
  it passes is cut short, so not even one that never ends can hold the suite.
  The timer sleeps in steps, so a timeout past 35 minutes does not overflow
  a 32-bit `Int`; a timeout of zero or less makes one attempt. It used to
  count attempts
  (the timeout over 20 ms), so slow attempts stretched it: an `eventually`
  around a `find` that never matched waited the outer number of attempts
  times the inner two seconds, an hour and more for a minute's timeout,
  which looked like a hung suite.

* First version: mount a component into a real page from an hspec suite, find
  its elements by CSS selector, click and type into them through Playwright,
  and read their text, properties and classes back. Runs on the WebAssembly
  and JavaScript backends, in headless Chromium under `cabal test`, or in a browser window from
  browser GHCi, rerun on every save (`npm run dev-test -- <package>`). Nothing in
  it names a monad: a component is mounted in any `MonadBrowserTest m`, whose
  instance says how to run `m` (and, by default, mounts with `runUI`); a
  suite's `main` picks one, typically `BrowserDOM`. Each test is a `PageM s`
  action run in a page of its own by `runPage`, whose `forall s` keeps what
  the test mounts and finds from outliving it, as `runST` does; `PageM` has no
  `MonadIO`, so pages cannot nest, and it carries hspec's expectations under
  their own names. Pages open one at a time, since the browser has one mouse,
  keyboard and focus, so tests marked `parallel` still run in turn. It builds on every
  backend, so a suite type-checks natively, but runs only in a browser;
  elsewhere `runBrowserTests` reports that it skipped.
* A test suite that is also a set of examples: a counter, a checkbox and a
  digits-only input, a todo list with keyed rows, a parent with child
  components, a loader that forks and a clock that subscribes to a timer,
  each with its spec, and a component for checking reconciliation: what a
  render keeps, patches and replaces. `unmount` takes a component down before
  its test ends, to check what happens once it is gone; elements compare with
  `==` as JavaScript's `===` does, `shouldBeSameElement` checks a render kept
  one, and `isAttached` whether it is still in the page.
* The `hspec-halogen` executable is the host side: `hspec-halogen test`, as
  cabal's `--test-wrapper`, post-links a wasm suite (or bundles a
  JavaScript-backend one with esbuild) and runs it in headless
  Chromium (or under Node, if it is not a browser suite), and `hspec-halogen
  open` opens a page for a suite run in browser GHCi. It carries the
  JavaScript that drives Playwright, so a project writes none; it needs Node,
  the npm packages `playwright` and `@bjorn3/browser_wasi_shim`, and Chromium.
* On the JavaScript backend a suite runs in Chromium as it does on WebAssembly.
  The functions hspec needs there that GHC's JavaScript runtime lacks come
  with haskell-halogen-core.
