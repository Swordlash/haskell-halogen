# Revision history for hspec-halogen

## Unreleased

* First version: mount a component into a real page from an hspec suite, find
  its elements by CSS selector, click and type into them through Playwright,
  and read their text, properties and classes back. Runs on the WebAssembly
  backend, in headless Chromium under `cabal test`, or in a browser window from
  browser GHCi, rerun on every save (`npm run dev-test -- <package>`). Nothing in
  it names a monad: a component is mounted in any `MonadBrowserTest m`, whose
  instance says how to run `m` (and, by default, mounts with `runUI`); a
  suite's `main` picks one, typically `BrowserDOM`. Each test works in a page
  of its own, from `withPage`, whose `forall s` keeps what the test mounts and
  finds from outliving it; pages open one at a time, since the browser has
  one mouse, keyboard and focus, so tests marked `parallel` still run in turn. It builds on every
  backend, so a suite type-checks natively, but runs only on WebAssembly;
  elsewhere `runBrowserTests` reports that it skipped.
* The `hspec-halogen` executable is the host side: `hspec-halogen test`, as
  cabal's `--test-wrapper`, post-links a wasm suite and runs it in headless
  Chromium (or under Node, if it is not a browser suite), and `hspec-halogen
  open` opens a page for a suite run in browser GHCi. It carries the
  JavaScript that drives Playwright, so a project writes none; it needs Node,
  the npm packages `playwright` and `@bjorn3/browser_wasi_shim`, and Chromium.
