# Revision history for hspec-halogen

## Unreleased

Set `HSPEC_HALOGEN_JOBS` to run examples marked `parallel` in isolated browser
contexts. Unmarked and `sequential` examples finish first. Each context has
its own input, storage and runtime; failures in any context fail the command.
Sharding preserves the behavior of `--fail-on=focused` and `--focused-only`.

* A test that fails says what the tree did before it failed: while a test
  runs, `runPage` keeps the last 50 events of core's trace
  (`Halogen.Trace`), and the failure's message ends with a section "What
  the tree did", one line per event, oldest first, with its time since the
  test started, the component's label and id, and what happened
  (`12.3 ms  Counter#3  action`, `12.9 ms  Counter#3  state changed`,
  `13.0 ms  Counter#3  rendered in 0.4 ms`). For an expectation of the
  `shouldBe` kind the section comes before what was expected and what came.

* The worked examples in `test/Example` build their components with
  core's `H.component initialState render [H.onAction handleAction, ...]`
  instead of `H.mkComponent` over a spec made with `H.mkEval` and
  `H.defaultEval`, so they show the shorter way to write a component.

* A suite runs in a page with no runner behind it too: the harness's
  actions, which the runner performs with Playwright, are then performed
  by the page itself with synthetic events (page.js's in-page mode,
  described in the README under "Without the runner"). A click is the
  pointer and mouse events a user's would send, the focus a press gives
  and the click itself; `typeText` sends each character's key events and
  edits the field with `beforeinput` and `input`; `press` sends the key's
  events and does the commonest of what the browser would (a character
  typed, `Backspace`, `Enter` on a button or in a form, which clicks the
  form's first submit button as a browser's implicit submission does,
  `Tab` moving the focus, `Backspace` and `Delete` by whole grapheme
  clusters, `Tab` in `tabindex` order, Playwright's key names such as
  `Space`, and nothing when `keydown`'s default is prevented); a prevented
  `keypress` or `beforeinput` keeps the edit out, a read-only field is not
  edited and `maxlength` caps what is typed without splitting a character,
  `clear` waits for a read-only field to become editable; typing into a
  `contenteditable` element or a number field works as a user's (`-42`
  arrives whole, and Backspace takes its characters back out); and each
  waits for its element to be visible (`visibility: hidden` included) and
  enabled, as Playwright does. Before, a page without the runner had a
  bare `click()`, a value set at once and a key's `keydown` and `keyup`.
  The haskell-halogen tutorial runs its testing lesson's spec this way, in
  the GHC it runs in the page.

* `isVisible`, and with it `shouldBeVisible` and `shouldBeHidden`, count an
  element with `visibility: hidden`, on it or on what holds it, as hidden,
  as Playwright does and as the actions' waits already did; before, only
  `display: none` and `content-visibility: hidden` hid an element from them,
  so `shouldBeVisible` could pass on a button `click` would not act on.

* `runBrowserTestsIn element spec` runs a suite with every test's page
  inside `element` rather than at the end of the body, for a page that has
  more on it than the suite (the tutorial's result pane). `runBrowserTests`
  is unchanged.

* The package's own suite has a `Window` example: a component that
  subscribes to haskell-halogen-core's `Halogen.Query.Event.Window`
  emitters, and tests that it hears a key pressed on the page, the window
  resized, the page hidden and shown, the browser go offline and online,
  the screen's frames in time order and a timer's ticks, and that none of
  them delivers anything more once the component has unsubscribed or is
  gone. The page's events are dispatched by the test itself
  (`test/jsbits/window.js`), and the page's visibility, which a headless
  page cannot change, is overridden on the document while a test needs it.

* A page action that fails (a click, a key press or a drag the runner could
  not perform, which Playwright reports when it times out waiting for the
  element, say) fails the test with a `JSError` from
  haskell-halogen-jsbits' `Halogen.JSBits.Promise`, which names the error
  and gives its message, on both browser backends. The actions return a
  `Promise` that the harness awaits; before, on the JavaScript backend the
  failure was an `IOError` with the message alone, and on WebAssembly
  whatever the JavaScript FFI made of the rejection.

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
