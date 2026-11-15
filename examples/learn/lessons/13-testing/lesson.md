title: Testing components
layers: hspec
args: -package hspec-halogen

`hspec-halogen` tests components in a browser. A test mounts a component, finds elements by CSS selector, interacts with them and checks the result.

The editor contains a component and a `spec` with two tests. Compile runs the tests in the result area and prints hspec's report below the editor. Each test removes its component when finished.

## Reading the spec

- `spec m` accepts the component monad as a required type argument, using `forall m ->` and `RequiredTypeArguments`. It needs a `MonadBrowserTest` instance. The wrapper chooses `BrowserDOM` in `runBrowserTestsIn result (spec BrowserDOM)`.
- Import `Test.Hspec.Halogen` for `describe`, `it` and the other hspec helpers, plus browser operations.
- `runPage` gives a test its own container and cleans it up on success or failure.
- `mount m component ()` mounts with `()` input and returns `ui`.
- `find ui "button.increment"` waits for the first matching element. If none appears within two seconds, it fails and includes the component's HTML.
- `click`, `typeText` and `clear` return after the component has reacted.
- `shouldHaveText` compares the complete text, retrying for up to two seconds.

These tests depend on the component's rendered interface, including the selectors used to find it.

## Running it

In a project, build the suite for WebAssembly or JavaScript with `main = runBrowserTests (spec BrowserDOM)`. Run it with `cabal test` and the native `hspec-halogen` executable as `--test-wrapper`. The wrapper starts Chromium, loads the suite and prints its report.

The runner uses [Playwright](https://playwright.dev) for trusted clicks and typing, checking that targets are visible, enabled and unobstructed. See `hspec-halogen/README.md` for setup. In this repository, install Chromium with `npx playwright install chromium`, then use `npm run test-wasm`.

The tutorial has no Playwright process. Here the harness simulates input with synthetic pointer, mouse and key events and edits fields with input events. These events are untrusted, do not check for covered targets and reproduce only supported browser behaviours. They are sufficient for this component. The same spec can run unchanged under the real runner.

## Exercise

Compile the starting code. The first test passes, but the second cannot find `button.increment`. Its failure points to the `find` call in `/tmp/Lesson.hs` and shows the HTML it searched.

1. Add `HP.class_ (HH.ClassName "increment")` to the button. Put the number alone in an `HH.span` with class `count`, after `"Count: "`. Keep the spec unchanged. Compile again: the report should say `2 examples, 0 failures`.
2. Add a test that types `Ada`, clears the input with `clear`, then types `Grace`. Check that the greeting is `Hello, Grace!`. Compile to run all three tests.

## The whole program

For built applications, multiple pages, navigation and server integration, use [`haskell-halogen-playwright`](https://github.com/Swordlash/haskell-halogen/blob/master/playwright/README.md). Its native hspec suite drives Chromium from outside the page. It provides familiar operations such as `click`, `typeText` and `shouldHaveText`, and waits for the program to finish reacting after each action.
