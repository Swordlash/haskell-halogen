title: Testing components
layers: hspec
args: -package hspec-halogen

A component is tested best where it runs: in a browser, clicked and typed into. `hspec-halogen` does that with hspec. A test mounts the component into the page, finds its elements by CSS selector, acts on them as a user would, and checks what it reads back. The editor holds this lesson's component and, below it, `spec`, two tests for it. Compile runs them, here in the page: each test mounts the component into the result, where it shows while the test runs, and hspec's report, a line per test and a summary, shows under the editor.

## Reading the spec

- `spec m` takes the monad the components run in as an argument (`forall m ->`, which needs `RequiredTypeArguments`), any monad with a `MonadBrowserTest` instance. The program that runs the suite picks it: this lesson's is `runBrowserTestsIn result (spec BrowserDOM)`, which runs every test in the page's result. `component`, polymorphic in its monad, needs nothing more.
- `describe` and `it` are hspec's, re-exported by `Test.Hspec.Halogen`: import it in place of `Test.Hspec`.
- `runPage` runs one test in a page of its own, and takes down what it mounted when the test ends, passed or failed. That is why the result is empty once the run is over.
- `mount m component ()` mounts the component with its input and hands back `ui`.
- `find ui "button.increment"` is the component's first element matching a CSS selector. It waits for one to appear, and fails, showing the component's HTML, if none has after two seconds.
- `click`, `typeText` and `clear` act on an element, and return once the component has reacted.
- `shouldHaveText` compares an element's whole text, retrying for up to two seconds before it fails.

So a test reads the page as a reader would, through what the component renders, and the selectors are what it depends on.

## Running it

A real suite runs on your machine, with `cabal test`: a test suite built with GHC's WebAssembly backend (or its JavaScript backend), whose `main` is `runBrowserTests (spec BrowserDOM)`, with the native `hspec-halogen` executable as cabal's `--test-wrapper`. The wrapper starts headless Chromium, serves the suite to it and prints hspec's report; it also performs every `click` and `typeText` through [Playwright](https://playwright.dev), as trusted input, on an element it has checked is visible, enabled and not covered. `hspec-halogen/README.md` sets such a suite up. In this repository, after `npx playwright install chromium`, `npm run test-wasm` builds that executable and runs every package's tests on WebAssembly, handing the browser suites, `hspec-halogen`'s own examples among them, to it.

This page has no Playwright: the spec is compiled by the GHC in the page and runs beside the tutorial. With no runner to perform them, the harness's actions are done by the page itself, with synthetic events: a click is the pointer and mouse events a user's would send, then the click itself; `typeText` sends each character's key events and edits the field with an `input` event, which is what `HE.onValueInput` hears. Those events are untrusted (their `isTrusted` is false), nothing checks that the element is not covered, and only the commonest of what a browser does by itself on a key is reproduced. Halogen's handlers do not mind, so for this component the run here and the real one agree. The same spec runs under the real runner unchanged.

## Exercise

Compile: the first test passes, and the second fails after two seconds, as nothing is `button.increment`. The report says where, `/tmp/Lesson.hs` at the line of that `find` (the page writes your module to `/tmp`), and shows the HTML it searched.

1. Change the component, not the spec: give the button the class `increment` with `HP.class_ (HH.ClassName "increment")`, and put the number alone in an `HH.span` with the class `count`, after the text `"Count: "`. Compile: `2 examples, 0 failures`. The component looks as it did; what changed is only what the spec can find in it.
2. Add a test of your own: that after typing `Ada`, clearing the field with `clear` and typing `Grace`, the greeting is `Hello, Grace!`. Compile again for three examples.
