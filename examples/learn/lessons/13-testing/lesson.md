title: Testing components

A component is tested best where it runs: in a browser, clicked and typed into. `hspec-halogen` does that with hspec. Its test suite runs inside a page in Chromium, mounts the component there, and has [Playwright](https://playwright.dev) perform the clicks and keystrokes as real input, on elements it has checked are visible and not covered. This page cannot run Playwright, so here is the spec, to read, for the component in the editor:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Test.Lesson (spec) where

import Lesson qualified
import Test.Hspec.Halogen

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = describe "Lesson.component" $ do
  it "greets by name" $ runPage $ do
    ui <- mount m Lesson.component ()
    find ui "input.name" >>= (`typeText` "Ada")
    find ui ".greeting" >>= (`shouldHaveText` "Hello, Ada!")

  it "counts clicks" $ runPage $ do
    ui <- mount m Lesson.component ()
    button <- find ui "button.increment"
    click button
    click button
    find ui ".count" >>= (`shouldHaveText` "2")
```

## Reading the spec

- `runPage` runs one test in a page of its own, and takes down what it mounted when the test ends, passed or failed.
- `mount m Lesson.component ()` mounts the component with its input and hands back `ui`. The monad is the spec's argument `m`, any monad with a `MonadBrowserTest` instance, and the suite's `main` picks it: `runBrowserTests (Test.Lesson.spec BrowserDOM)`. The component, polymorphic in its monad, needs nothing more.
- `find ui "button.increment"` is the component's first element matching a CSS selector. It waits for one to appear, and fails, showing the component's HTML, if none does.
- `click` and `typeText` act through Playwright, and return once the component has reacted.
- `shouldHaveText` compares an element's whole text, retrying for up to two seconds before it fails.

So a test reads the page as a reader would, through what the component renders, and the selectors are what it depends on.

## Running it

The spec runs on your machine, with `cabal test`: in a test suite built with GHC's WebAssembly backend (or its JavaScript backend), whose `main` is the `runBrowserTests` line above, with the native `hspec-halogen` executable as cabal's `--test-wrapper`. The wrapper starts Chromium, serves the suite to it and prints hspec's report. `hspec-halogen/README.md` sets such a suite up. In this repository, after `npx playwright install chromium`, `npm run test-wasm` builds that executable and runs every package's tests on WebAssembly, handing the browser suites, `hspec-halogen`'s own examples among them, to it.

## Exercise

The first test passes against `Start.hs`; the second does not, as nothing is `button.increment` or `.count`. Change the component, not the spec: give the button the class `increment` with `HP.class_ (HH.ClassName "increment")`, and put the number alone in an `HH.span` with the class `count`, after the text `"Count: "`. Compile: the page looks the same, and the spec as written would pass.
