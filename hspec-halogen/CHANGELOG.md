# Revision history for hspec-halogen

## Unreleased

* First version: mount a component into a real page from an hspec suite, find
  its elements by CSS selector, click and type into them through Playwright,
  and read their text, properties and classes back. Runs on the WebAssembly
  backend, in headless Chromium under `cabal test`, or in a browser window from
  browser GHCi, rerun on every save (`npm run dev-test -- <package>`). Nothing in
  it names a monad: a component is mounted in any `MonadBrowserTest m`, whose
  instance says how to run `m` (and, by default, mounts with `runUI`); a
  suite's `main` picks one, typically `BrowserDOM`. It builds on every
  backend, so a suite type-checks natively, but runs only on WebAssembly;
  elsewhere `runBrowserTests` reports that it skipped.
